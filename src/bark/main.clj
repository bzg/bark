;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.main
  "Entry point for BARK. Connects to a mail source (IMAP or Maildir),
  fetches new emails since the last run, and stores+processes them
  atomically. Default mode is single-pass (batch); use --watch for
  persistent watching (IMAP IDLE or filesystem events)."
  (:require [bark.ingest :as ingest]
            [bark.logging :as blog]
            [bark.common :as common]
            [bark.digest :as digest]
            [bark.expire :as expire]
            [bark.roles :as roles]
            [datalevin.core :as d]
            [mailseq :as mailseq]
            [clojure.string :as str]
            [postal.core :as postal]
            [taoensso.timbre :as log])
  (:import [java.time Instant]
           [java.time.temporal ChronoUnit]
           [java.util Date])
  (:gen-class))

;; ---------------------------------------------------------------------------
;; Logging config
;; ---------------------------------------------------------------------------

(log/merge-config!
 {:min-level [["datalevin.*"          :warn]
              ["org.eclipse.angus.*"  :warn]
              ["*"                    :info]]})

(defn configure-email-logging!
  [smtp-cfg {:keys [to level] :or {level :error}}]
  (when (and smtp-cfg to)
    (let [{:keys [host port tls user password from]} smtp-cfg
          conn {:host host :port (or port 587) :tls (boolean tls)
                :user user :pass password}]
      (log/merge-config!
       {:appenders
        {:email
         {:enabled?   true
          :min-level  level
          :rate-limit [[5 (* 5 60 1000)]]
          :fn         (fn [data]
                        (try
                          (let [level-str (str/upper-case (name (:level data)))
                                msg       (force (:msg_ data))]
                            (postal/send-message
                             conn
                             {:from    from
                              :to      [to]
                              :subject (str "[BARK] " level-str " — " (:?ns-str data))
                              :body    (str (force (:timestamp_ data)) " " level-str " "
                                            (:?ns-str data) " — " msg)}))
                          (catch Exception e
                            (.println System/err
                                      (str "Failed to send log email: " (.getMessage e))))))}}}))))

;; ---------------------------------------------------------------------------
;; Shutdown coordination
;; ---------------------------------------------------------------------------

(def ^:private shutdown? (atom false))

(defn shutting-down? [] @shutdown?)

;; ---------------------------------------------------------------------------
;; Initial-fetch parsing
;; ---------------------------------------------------------------------------

(defn- parse-initial-fetch [v]
  (cond
    (integer? v) {:limit v}
    (string? v)
    (or (when (re-matches #"\d{4}-\d{2}-\d{2}" v)
          {:since (Date/from (Instant/parse (str v "T00:00:00Z")))})
        (when-let [days (common/parse-duration-str v)]
          {:since (Date/from (.minus (Instant/now) days ChronoUnit/DAYS))})
        (throw (ex-info (str "Invalid :initial-fetch value: " (pr-str v)
                            " (expected integer, \"Nd/w/m/y\" duration, or \"yyyy-MM-dd\" date)")
                       {:value v})))
    :else {:limit 50}))

;; ---------------------------------------------------------------------------
;; Watermark management
;; ---------------------------------------------------------------------------

(defn- max-contiguous-safe-uid
  "Walk sorted UIDs from lowest to highest and return the last UID in the
  unbroken prefix of safe-uids.  Returns nil if the very first UID is
  not safe — this is deliberate: we only advance the watermark past UIDs
  that are known-good in sequence, so that a failed message is retried
  on next reconnect rather than silently skipped."
  [all-uids safe-uids]
  (reduce (fn [acc uid]
            (if (contains? safe-uids uid) uid (reduced acc)))
          nil all-uids))

(defn- advance-watermark! [db-conn msgs safe-ids]
  (let [all-uids (->> msgs (keep :uid) sort)]
    (if-let [new-wm (max-contiguous-safe-uid all-uids safe-ids)]
      (do (when (not= new-wm (some->> all-uids last))
            (log/warn "Watermark stopped at UID" new-wm
                      "(some messages failed — will retry on next reconnect)"))
          (ingest/save-imap-uid! db-conn new-wm))
      ;; First UID in batch failed — watermark cannot advance at all.
      (when (seq all-uids)
        (log/warn "Watermark not advanced: first UID" (first all-uids)
                  "failed — entire batch of" (count all-uids)
                  "message(s) will be retried on next reconnect")))))

;; ---------------------------------------------------------------------------
;; Atomic store+process
;; ---------------------------------------------------------------------------

(defn- try-digest!
  "Run process-email!, returning :ok on success, :retry on exception."
  [db-conn source-map sources email mid]
  (try
    (digest/process-email! db-conn source-map sources email)
    :ok
    (catch Exception e
      (log/error e "Failed to digest email" mid
                 (or (.getMessage e) (str (class e))))
      :retry)))

(defn- store-and-process!
  "Classify, store, and digest an email.
  Returns one of:
    :ok    — fully processed (or already digested), advance watermark
    :skip  — deterministic skip (oversized, no source, no Message-ID),
             advance watermark since retrying won't help
    :retry — transient failure, do not advance watermark
  When an email was previously stored but not yet digested (e.g. crash
  between store and digest), re-runs process-email! which is idempotent.
  Already-digested emails are skipped to avoid redundant work."
  [db-conn source-map sources msg {:keys [max-size max-attachment-size]}]
  (let [size (:size msg -1)
        id   (:id msg)]
    (if (and max-size (pos? size) (> size max-size))
      (do (log/warn "Skipping oversized email id:" id
                    "size:" size (str "bytes (max: " max-size ")"))
          :skip)
      (if-let [src-name (digest/pre-classify-source (d/db db-conn) source-map sources msg)]
        (let [mid (:message-id msg)
              store-opts (cond-> {}
                           max-attachment-size (assoc :max-attachment-size max-attachment-size))]
          (if (nil? mid)
            (do (log/warn "No Message-ID for id:" id "— skipping")
                :skip)
            (let [lookup [:email/message-id mid]]
              (if (ingest/store-email! db-conn msg store-opts)
                ;; Freshly stored — stamp source and digest.
                (do (d/transact! db-conn [{:db/id lookup :email/source src-name}])
                    (try-digest! db-conn source-map sources
                                 (d/pull (d/db db-conn) digest/email-pull-pattern lookup) mid))
                ;; Already stored (duplicate message-id or id collision).
                (let [email (d/pull (d/db db-conn) digest/email-pull-pattern lookup)]
                  (cond
                    ;; Lookup miss → id collision (different message-id already
                    ;; occupies this id).  Original was fully handled — safe.
                    (not (:db/id email)) :ok
                    ;; Already fully digested — nothing to do.
                    (:email/digested-at email)
                    (do (log/debug "Already digested, skipping:" mid) :ok)
                    ;; Stored but not digested (prior crash) — recover.
                    :else
                    (do (log/info "Re-processing previously stored email:" mid)
                        (when-not (:email/source email)
                          (d/transact! db-conn [{:db/id lookup :email/source src-name}]))
                        (try-digest! db-conn source-map sources email mid))))))))
        (do (log/debug "No matching source for id:" id "— not stored")
            :skip)))))

;; ---------------------------------------------------------------------------
;; Catch-up fetch (store+process per email)
;; ---------------------------------------------------------------------------

(defn- catch-up-imap!
  "IMAP incremental fetch: use UID watermark to fetch only new messages."
  [src db-conn folder fetch-opts source-map sources ingest-opts]
  (let [watermark (ingest/max-imap-uid db-conn)
        msgs (if (zero? watermark)
               (let [{:keys [limit since]} fetch-opts]
                 (if since
                   (log/info "First run — fetching messages since" since)
                   (log/info "First run — fetching last" limit "messages"))
                 (mailseq/messages src folder
                                  (merge {:attachments? true} fetch-opts)))
               (do (log/info "Resuming — fetching UIDs >" watermark)
                   (mailseq/by-id-range src folder
                                        (str (inc watermark)) nil)))]
    (log/info "Fetched" (count msgs) "messages")
    (when (and (seq msgs) (not (shutting-down?)))
      (let [safe-ids (reduce (fn [acc msg]
                               (try
                                 (let [result (store-and-process! db-conn source-map sources msg ingest-opts)]
                                   (if (and (not= :retry result) (:uid msg))
                                     (conj acc (:uid msg))
                                     acc))
                                 (catch Exception e
                                   (log/error e "Failed to process id:" (:id msg))
                                   acc)))
                             #{} msgs)]
        (advance-watermark! db-conn msgs safe-ids)))))

(defn- catch-up-maildir!
  "Maildir incremental fetch: diff list-ids against known :email/id in DB.
  On first run (no known ids), uses mailseq/messages with fetch-opts
  to honour :limit/:since, just like the IMAP path."
  [src db-conn folder fetch-opts source-map sources ingest-opts]
  (let [known   (ingest/known-email-ids db-conn)
        msgs    (if (empty? known)
                  ;; First run — use fetch-opts (:limit/:since) to avoid
                  ;; ingesting the entire Maildir at once.
                  (let [{:keys [limit since]} fetch-opts]
                    (if since
                      (log/info "First run — fetching messages since" since)
                      (log/info "First run — fetching last" (or limit "all") "messages"))
                    (mailseq/messages src folder
                                     (merge {:attachments? true} fetch-opts)))
                  ;; Subsequent runs — diff ids.
                  (let [all-ids (mailseq/list-ids src folder)
                        new-ids (filterv (complement known) all-ids)]
                    (when (seq new-ids)
                      (mailseq/by-ids src folder new-ids))))]
    (if (empty? msgs)
      (log/info "No new messages in Maildir")
      (do (log/info "Fetched" (count msgs) "new messages from Maildir")
          (doseq [msg msgs
                  :while (not (shutting-down?))]
            (try
              (store-and-process! db-conn source-map sources msg ingest-opts)
              (catch Exception e
                (log/error e "Failed to process id:" (:id msg)))))))))

(defn catch-up-fetch!
  "Fetch messages missed while the process was down.
  Dispatches to IMAP (watermark) or Maildir (id diff) strategy."
  [src db-conn folder fetch-opts source-map sources ingest-opts mailbox-type]
  (when-not (shutting-down?)
    (case mailbox-type
      :imap    (catch-up-imap! src db-conn folder fetch-opts source-map sources ingest-opts)
      :maildir (catch-up-maildir! src db-conn folder fetch-opts source-map sources ingest-opts))))

;; ---------------------------------------------------------------------------
;; Mail source connection
;; ---------------------------------------------------------------------------

(defn- mailbox->mailseq-cfg
  "Convert a bark :mailbox config map to the format expected by mailseq/open.
  Maps the single :folder key to the :folders map mailseq expects."
  [{:keys [type folder path] :or {folder "INBOX"} :as cfg}]
  (let [base    (dissoc cfg :folder :path)
        folders (case type
                  :imap    {folder folder}
                  :maildir {folder (str path "/" folder)})]
    (assoc base :folders folders)))

(defn open-mailbox [mailbox-cfg]
  (try
    (log/info "Opening mailbox" (pr-str (:type mailbox-cfg))
              (case (:type mailbox-cfg)
                :imap    (str (:user mailbox-cfg) "@" (:host mailbox-cfg))
                :maildir (:path mailbox-cfg)
                ""))
    (mailseq/open (mailbox->mailseq-cfg mailbox-cfg))
    (catch Exception e
      (log/error e "Mailbox connection failed:" (or (.getMessage e) (str (class e))))
      nil)))

;; ---------------------------------------------------------------------------
;; Periodic tasks (atom-gated, once per day)
;; ---------------------------------------------------------------------------

(def ^:private one-day-ms (* 24 60 60 1000))

(defn- maybe-expire!
  "Run expire-reports! if at least one day has elapsed since `last-ms`.
  Returns the updated timestamp on success, or `last-ms` on failure
  so that the next cycle retries."
  [db-conn source-map last-ms]
  (let [now (System/currentTimeMillis)]
    (if (> (- now last-ms) one-day-ms)
      (try
        (expire/expire-reports! db-conn source-map)
        now
        (catch Exception e
          (log/error e "Expire failed:" (or (.getMessage e) (str (class e))))
          last-ms))
      last-ms)))

;; ---------------------------------------------------------------------------
;; Watch mode with reconnection
;; ---------------------------------------------------------------------------

(def ^:private max-backoff-ms (* 5 60 1000))

(defn start-watch!
  "Start watching for new messages, storing+processing each as it arrives."
  [src db-conn folder source-map sources ingest-opts]
  (log/info "Starting watch on" folder)
  (mailseq/watch src folder
                 (fn [msg]
                   (when-not (shutting-down?)
                     (if (nil? msg)
                       (log/warn "Watch delivered nil message, skipping")
                       (do
                         (log/info "New message via watch — id:" (:id msg)
                                   "Subject:" (:subject msg))
                         (try
                           (let [result (store-and-process! db-conn source-map sources msg ingest-opts)]
                             ;; Advance IMAP watermark when applicable
                             (when (and (not= :retry result) (:uid msg))
                               (ingest/save-imap-uid! db-conn (:uid msg))))
                           (catch Exception e
                             (log/error e "Error processing watch message id:" (:id msg)
                                        (str "(" (.getName (class e)) ": "
                                             (or (.getMessage e) "no message") ")"))))))))
                 {:parse-opts   {:attachments? true}
                  :heartbeat-ms (* 20 60 1000)}))

(defn watch-loop!
  "Run watch with automatic reconnection and exponential backoff.
  Reloads config.edn on each reconnect so changes take effect."
  [mailbox-cfg db-conn ingest-cfg config-path]
  (let [folder       (or (:folder mailbox-cfg) "INBOX")
        mailbox-type (:type mailbox-cfg)
        fetch-opts   (parse-initial-fetch (or (:initial-fetch ingest-cfg) 50))
        ingest-opts  (select-keys ingest-cfg [:max-size :max-attachment-size])]
    (loop [backoff-ms 1000
           last-expire-ms 0]
      (when-not (shutting-down?)
        (let [config     (or (common/load-config config-path) {})
              source-map (common/build-source-map config)
              sources    (or (:sources config) [])
              src        (open-mailbox mailbox-cfg)]
          (if-not src
            (do (log/error "Mailbox connection failed, retrying in" (/ backoff-ms 1000) "s")
                (Thread/sleep backoff-ms)
                (recur (min (* backoff-ms 2) max-backoff-ms) last-expire-ms))
            (do
              (let [new-expire-ms
                    (try
                      (log/info "Mailbox connected, folder:" folder)
                      (catch-up-fetch! src db-conn folder fetch-opts source-map sources ingest-opts mailbox-type)
                      (let [ts (maybe-expire! db-conn source-map last-expire-ms)]
                        (when-not (shutting-down?)
                          (start-watch! src db-conn folder source-map sources ingest-opts))
                        ts)
                      (catch Exception e
                        (log/error e "Watch interrupted:" (or (.getMessage e) (str (class e))))
                        last-expire-ms))]
                (try (mailseq/close src)
                     (catch Exception e
                       (log/debug "Mailbox close failed:" (.getMessage e))))
                (when-not (shutting-down?)
                  (log/debug "Watch exited, reconnecting in 1s")
                  (Thread/sleep 1000)
                  (recur 1000 new-expire-ms))))))))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn- batch-run!
  "Single-pass mode (default): connect, fetch new messages, expire, exit."
  [mailbox-cfg db-conn ingest-cfg config-path]
  (let [folder       (or (:folder mailbox-cfg) "INBOX")
        mailbox-type (:type mailbox-cfg)
        fetch-opts   (parse-initial-fetch (or (:initial-fetch ingest-cfg) 50))
        ingest-opts  (select-keys ingest-cfg [:max-size :max-attachment-size])
        config       (or (common/load-config config-path) {})
        source-map   (common/build-source-map config)
        sources      (or (:sources config) [])
        src          (open-mailbox mailbox-cfg)]
    (when-not src
      (log/error "Mailbox connection failed.")
      (System/exit 1))
    (try
      (catch-up-fetch! src db-conn folder fetch-opts source-map sources ingest-opts mailbox-type)
      (expire/expire-reports! db-conn source-map)
      (finally
        (try (mailseq/close src)
             (catch Exception e
               (log/debug "Mailbox close failed:" (.getMessage e)))))))))

(defn -main [& args]
  (let [;; Parse CLI args: --initial-fetch, --watch, -c config-path
        arg-set   (set args)
        watch?    (contains? arg-set "--watch")
        pairs     (partition 2 args)
        cli-fetch (some (fn [[a b]] (when (= "--initial-fetch" a) b)) pairs)
        config-path (or (some (fn [[a b]] (when (= "-c" a) b)) pairs) "config.edn")
        config      (common/load-config config-path)]
    (when (nil? config)
      (log/error "Config file not found:" config-path)
      (System/exit 1))
    (let [mailbox-cfg (:mailbox config)
          ingest-cfg  (cond-> (or (:ingest config) {})
                        cli-fetch (assoc :initial-fetch cli-fetch))]
      (when-let [logging (:logging config)]
        (blog/configure-file-logging! logging)
        (when-let [email-cfg (:email logging)]
          (if-let [smtp (get-in config [:notifications :smtp])]
            (configure-email-logging! smtp email-cfg)
            (log/warn "Logging :email configured but no :notifications :smtp found."))))
      (when-not mailbox-cfg
        (log/error "No :mailbox key in config.edn.")
        (System/exit 1))
      (when-not (#{:imap :maildir} (:type mailbox-cfg))
        (log/error "Invalid :type in :mailbox — expected :imap or :maildir, got:" (pr-str (:type mailbox-cfg)))
        (System/exit 1))
      (let [db-cfg  (:db config)
            db-conn (ingest/connect (:path db-cfg))
            _       (log/info "Datalevin connected.")]
        ;; Initialize roles from config
        (roles/ensure-source-roles! db-conn config)
        (doseq [{:keys [name]} (:sources config)]
          (roles/ensure-notify-defaults! db-conn name
                                         (roles/get-tenures (d/db db-conn) name)))
        (.addShutdownHook
         (Runtime/getRuntime)
         (Thread.
          (fn []
            (log/info "Shutting down...")
            (reset! shutdown? true)
            (Thread/sleep 1000)
            (try (ingest/close db-conn)
                 (catch Exception e
                   (log/debug "DB close failed:" (.getMessage e))))
            (log/info "Goodbye."))))
        (if watch?
          (watch-loop! mailbox-cfg db-conn ingest-cfg config-path)
          (do
            (batch-run! mailbox-cfg db-conn ingest-cfg config-path)
            ;; Datalevin/LMDB keeps non-daemon threads alive; without an
            ;; explicit close + System/exit the JVM hangs after batch mode.
            (try (ingest/close db-conn)
                 (catch Exception e
                   (log/debug "DB close failed:" (.getMessage e))))
            (shutdown-agents)
            (System/exit 0)))))))
