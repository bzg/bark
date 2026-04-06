;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.main
  "Entry point for BARK. Connects to a single IMAP mailbox,
  fetches new emails since the last run, and stores+processes them
  atomically. Default mode is single-pass (batch); use --watch for
  persistent IMAP IDLE."
  (:require [bark.ingest :as ingest]
            [bark.logging :as blog]
            [bark.common :as common]
            [bark.digest :as digest]
            [bark.expire :as expire]
            [bark.roles :as roles]
            [datalevin.core :as d]
            [fetch-imap.core :as imap]
            [fetch-imap.fetch :as fetch]
            [fetch-imap.idle :as idle]
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

(defn- advance-watermark! [db-conn msgs safe-uids]
  (let [all-uids (->> msgs (keep :uid) sort)]
    (if-let [new-wm (max-contiguous-safe-uid all-uids safe-uids)]
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
  (let [size (:size msg -1)]
    (if (and max-size (pos? size) (> size max-size))
      (do (log/warn "Skipping oversized email UID:" (:uid msg)
                    "size:" size (str "bytes (max: " max-size ")"))
          :skip)
      (if-let [src-name (digest/pre-classify-source (d/db db-conn) source-map sources msg)]
        (let [mid (:message-id msg)
              store-opts (cond-> {}
                           max-attachment-size (assoc :max-attachment-size max-attachment-size))]
          (if (nil? mid)
            (do (log/warn "No Message-ID for UID:" (:uid msg) "— skipping")
                :skip)
            (let [lookup [:email/message-id mid]]
              (if (ingest/store-email! db-conn msg store-opts)
                ;; Freshly stored — stamp source and digest.
                (do (d/transact! db-conn [{:db/id lookup :email/source src-name}])
                    (try-digest! db-conn source-map sources
                                 (d/pull (d/db db-conn) digest/email-pull-pattern lookup) mid))
                ;; Already stored (duplicate message-id or UID collision).
                (let [email (d/pull (d/db db-conn) digest/email-pull-pattern lookup)]
                  (cond
                    ;; Lookup miss → UID collision (different message-id already
                    ;; occupies this UID).  Original was fully handled — safe.
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
        (do (log/debug "No matching source for UID:" (:uid msg) "— not stored")
            :skip)))))

;; ---------------------------------------------------------------------------
;; Catch-up fetch (store+process per email)
;; ---------------------------------------------------------------------------

(defn catch-up-fetch!
  "Fetch messages missed while the process was down.
  Each message is stored and processed atomically."
  [imap-conn db-conn folder fetch-opts source-map sources ingest-opts]
  (when-not (shutting-down?)
    (let [watermark (ingest/max-imap-uid db-conn)
          msgs (if (zero? watermark)
                 (let [{:keys [limit since]} fetch-opts]
                   (if since
                     (log/info "First run — fetching messages since" since)
                     (log/info "First run — fetching last" limit "messages"))
                   (fetch/messages imap-conn folder
                                  (merge {:attachments? true} fetch-opts)))
                 (do (log/info "Resuming — fetching UIDs >" watermark)
                     (fetch/by-uid-range imap-conn folder
                                         (inc watermark) Long/MAX_VALUE)))]
      (log/info "Fetched" (count msgs) "messages from IMAP")
      (when (and (seq msgs) (not (shutting-down?)))
        (let [safe-uids (reduce (fn [acc msg]
                                  (try
                                    (let [result (store-and-process! db-conn source-map sources msg ingest-opts)]
                                      (if (and (not= :retry result) (:uid msg))
                                        (conj acc (:uid msg))
                                        acc))
                                    (catch Exception e
                                      (log/error e "Failed to process UID:" (:uid msg))
                                      acc)))
                                #{} msgs)]
          (advance-watermark! db-conn msgs safe-uids))))))

;; ---------------------------------------------------------------------------
;; IMAP connection
;; ---------------------------------------------------------------------------

(defn connect-imap [imap-cfg]
  (try
    (log/info "Connecting to IMAP" (:host imap-cfg) "as" (:user imap-cfg))
    (imap/connect (select-keys imap-cfg [:host :port :ssl :user :password :oauth2-token]))
    (catch Exception e
      (log/error e "IMAP connection failed:" (or (.getMessage e) (str (class e))))
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
;; IDLE mode with reconnection
;; ---------------------------------------------------------------------------

(def ^:private max-backoff-ms (* 5 60 1000))

(defn start-idle!
  "Start IMAP IDLE, storing+processing each new message as it arrives."
  [imap-conn db-conn folder source-map sources ingest-opts]
  (log/info "Starting IMAP IDLE on" folder)
  (idle/idle imap-conn folder
             (fn [msg]
               (when-not (shutting-down?)
                 (if (nil? msg)
                   (log/warn "IDLE delivered nil message, skipping")
                   (do
                     (log/info "New message via IDLE — UID:" (:uid msg)
                               "Subject:" (:subject msg))
                     (try
                       (let [result (store-and-process! db-conn source-map sources msg ingest-opts)]
                         (when (and (not= :retry result) (:uid msg))
                           (ingest/save-imap-uid! db-conn (:uid msg))))
                       (catch Exception e
                         (log/error e "Error processing IDLE message UID:" (:uid msg)
                                    (str "(" (.getName (class e)) ": "
                                         (or (.getMessage e) "no message") ")"))))))))
             {:parse-opts   {:attachments? true}
              :heartbeat-ms (* 20 60 1000)}))

(defn idle-loop!
  "Run IDLE with automatic reconnection and exponential backoff.
  Reloads config.edn on each reconnect so changes take effect."
  [imap-cfg db-conn ingest-cfg config-path]
  (let [folder      (or (:folder imap-cfg) "INBOX")
        fetch-opts  (parse-initial-fetch (or (:initial-fetch ingest-cfg) 50))
        ingest-opts (select-keys ingest-cfg [:max-size :max-attachment-size])]
    (loop [backoff-ms 1000
           last-expire-ms 0]
      (when-not (shutting-down?)
        (let [config     (or (common/load-config config-path) {})
              source-map (common/build-source-map config)
              sources    (or (:sources config) [])
              conn       (connect-imap imap-cfg)]
          (if-not conn
            (do (log/error "IMAP connection failed, retrying in" (/ backoff-ms 1000) "s")
                (Thread/sleep backoff-ms)
                (recur (min (* backoff-ms 2) max-backoff-ms) last-expire-ms))
            (do
              (let [new-expire-ms
                    (try
                      (log/info "IMAP connected, folder:" folder)
                      (catch-up-fetch! conn db-conn folder fetch-opts source-map sources ingest-opts)
                      (let [ts (maybe-expire! db-conn source-map last-expire-ms)]
                        (when-not (shutting-down?)
                          (start-idle! conn db-conn folder source-map sources ingest-opts))
                        ts)
                      (catch Exception e
                        (log/error e "IDLE interrupted:" (or (.getMessage e) (str (class e))))
                        last-expire-ms))]
                (try (imap/disconnect conn) (catch Exception _))
                (when-not (shutting-down?)
                  (log/debug "IDLE exited, reconnecting in 1s")
                  (Thread/sleep 1000)
                  (recur 1000 new-expire-ms))))))))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn- batch-run!
  "Single-pass mode (default): connect, fetch new messages, expire, exit."
  [imap-cfg db-conn ingest-cfg config-path]
  (let [folder      (or (:folder imap-cfg) "INBOX")
        fetch-opts  (parse-initial-fetch (or (:initial-fetch ingest-cfg) 50))
        ingest-opts (select-keys ingest-cfg [:max-size :max-attachment-size])
        config      (or (common/load-config config-path) {})
        source-map  (common/build-source-map config)
        sources     (or (:sources config) [])
        conn        (connect-imap imap-cfg)]
    (when-not conn
      (log/error "IMAP connection failed.")
      (System/exit 1))
    (try
      (catch-up-fetch! conn db-conn folder fetch-opts source-map sources ingest-opts)
      (expire/expire-reports! db-conn source-map)
      (finally
        (try (imap/disconnect conn) (catch Exception _))))))

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
    (let [imap-cfg   (:imap config)
          ingest-cfg (cond-> (or (:ingest config) {})
                       cli-fetch (assoc :initial-fetch cli-fetch))]
      (when-let [logging (:logging config)]
        (blog/configure-file-logging! logging)
        (when-let [email-cfg (:email logging)]
          (if-let [smtp (get-in config [:notifications :smtp])]
            (configure-email-logging! smtp email-cfg)
            (log/warn "Logging :email configured but no :notifications :smtp found."))))
      (when-not imap-cfg
        (log/error "No :imap key in config.edn.")
        (System/exit 1))
      (let [db-cfg  (:db config)
            db-conn (ingest/connect (:path db-cfg))
            _       (log/info "Datalevin connected.")]
        ;; Initialize roles from config
        (roles/ensure-source-roles! db-conn config)
        (doseq [{:keys [name]} (:sources config)]
          (roles/ensure-notify-defaults! db-conn name
                                         (roles/get-roles (d/db db-conn) name)))
        (.addShutdownHook
         (Runtime/getRuntime)
         (Thread.
          (fn []
            (log/info "Shutting down...")
            (reset! shutdown? true)
            (Thread/sleep 1000)
            (try (ingest/close db-conn) (catch Exception _))
            (log/info "Goodbye."))))
        (if watch?
          (idle-loop! imap-cfg db-conn ingest-cfg config-path)
          (batch-run! imap-cfg db-conn ingest-cfg config-path))))))
