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
;; Fetch parsing
;;
;; `:fetch` accepts exactly one of three disjoint map shapes — strict,
;; no key mixing, empty map rejected:
;;
;;   {:limit N}              — latest N messages (pos-int)
;;   {:since "Nd"|"Nw"|...}  — relative duration from now (duration-only)
;;   {:start "yyyy-MM-dd"
;;    :end   "yyyy-MM-dd"?}  — absolute window; :start alone, :end alone,
;;                             or both are all valid (ISO dates only).
;;
;; Mailseq's wire vocabulary is :since/:before; this function translates
;; :start → :since and :end → :before at the boundary.
;; ---------------------------------------------------------------------------

(defn- iso->date [s]
  (when (and (string? s) (re-matches #"\d{4}-\d{2}-\d{2}" s))
    (Date/from (Instant/parse (str s "T00:00:00Z")))))

(defn- duration->days [s]
  (when (and (string? s) (re-matches #"\d+[dwmy]" s))
    (common/parse-duration-str s)))

(defn- fetch-err [msg v]
  (throw (ex-info (str "Invalid :fetch " msg) {:value v})))

(defn- parse-fetch-limit [{:keys [limit] :as v}]
  (when-not (pos-int? limit)
    (fetch-err (str ":limit: " (pr-str limit) " (expected a positive integer)") v))
  {:limit limit})

(defn- parse-fetch-since [{:keys [since] :as v}]
  (if-let [days (duration->days since)]
    {:since (Date/from (.minus (Instant/now) days ChronoUnit/DAYS))}
    (fetch-err (str ":since: " (pr-str since)
                    " (expected duration like \"30d\", \"6w\", \"3m\", \"1y\")")
               v)))

(defn- parse-fetch-window [{:keys [start end] :as v}]
  (let [start-d (when start
                  (or (iso->date start)
                      (fetch-err (str ":start: " (pr-str start)
                                      " (expected \"yyyy-MM-dd\")") v)))
        end-d   (when end
                  (or (iso->date end)
                      (fetch-err (str ":end: " (pr-str end)
                                      " (expected \"yyyy-MM-dd\")") v)))]
    (when (and start-d end-d (not (.before ^Date start-d ^Date end-d)))
      (fetch-err (str "window: " (pr-str v)
                      " (:start must be strictly before :end)") v))
    (cond-> {}
      start-d (assoc :since  start-d)
      end-d   (assoc :before end-d))))

(defn- parse-fetch [v]
  (when-not (map? v)
    (fetch-err (str "value: " (pr-str v)
                    " (expected a map with :limit, :since, or :start/:end)") v))
  (let [ks (set (keys v))]
    (cond
      (= ks #{:limit})                          (parse-fetch-limit v)
      (= ks #{:since})                          (parse-fetch-since v)
      (and (seq ks) (every? #{:start :end} ks)) (parse-fetch-window v)
      :else
      (fetch-err (str "value: " (pr-str v)
                      " (expected exactly one of {:limit N}, {:since \"30d\"}, "
                      "or {:start/:end ISO} — no key mixing, no empty map)") v))))

(defn- cli-fetch->map
  "Convert a scalar --fetch CLI arg into the canonical map form.
   \"50\"           → {:limit 50}
   \"30d\" etc.     → {:since \"30d\"}
   \"2020-01-01\"   → {:start \"2020-01-01\"}"
  [s]
  (cond
    (re-matches #"\d+" s)          {:limit (Long/parseLong s)}
    (re-matches #"\d+[dwmy]" s)    {:since s}
    (re-matches #"\d{4}-\d{2}-\d{2}" s) {:start s}
    :else
    (throw (ex-info (str "Invalid --fetch CLI value: " (pr-str s)
                         " (expected integer count, duration like \"30d\", or ISO date)")
                    {:value s}))))

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
      (log/error e "Failed to digest email" mid (blog/exception-msg e))
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
        id   (:id msg)
        mid  (:message-id msg)]
    (cond
      (and max-size (pos? size) (> size max-size))
      (do (log/warn "Skipping oversized email id:" id
                    "size:" size (str "bytes (max: " max-size ")"))
          :skip)

      (nil? mid)
      (do (log/warn "No Message-ID for id:" id "— skipping")
          :skip)

      :else
      (if-let [src-name (digest/pre-classify-source (d/db db-conn) sources msg)]
        (let [lookup     [:email/message-id mid]
              store-opts (if max-attachment-size
                           {:max-attachment-size max-attachment-size}
                           {})]
          (if (ingest/store-email! db-conn msg store-opts)
            (do (d/transact! db-conn [{:db/id lookup :email/source src-name}])
                (try-digest! db-conn source-map sources
                             (d/pull (d/db db-conn) digest/email-pull-pattern lookup) mid))
            (let [email (d/pull (d/db db-conn) digest/email-pull-pattern lookup)]
              (cond
                (not (:db/id email))
                :ok

                (:email/digested-at email)
                (do (log/debug "Already digested, skipping:" mid) :ok)

                :else
                (do (log/info "Re-processing previously stored email:" mid)
                    (when-not (:email/source email)
                      (d/transact! db-conn [{:db/id lookup :email/source src-name}]))
                    (try-digest! db-conn source-map sources email mid))))))
        (do (log/debug "No matching source for id:" id "— not stored")
            :skip)))))

;; ---------------------------------------------------------------------------
;; Catch-up fetch (store+process per email)
;; ---------------------------------------------------------------------------

(defn- log-first-run [{:keys [limit since]}]
  (log/info "First run — fetching"
            (if since (str "messages since " since) (str "last " (or limit "all") " messages"))))

(defn- first-run-messages [src folder fetch-opts]
  (log-first-run fetch-opts)
  (mailseq/messages src folder (merge {:attachments? true} fetch-opts)))

(defn- sort-chronologically
  "Sort a batch of mailseq messages oldest-first by the Date: header,
  falling back to server receive time then UID so messages without a
  date do not reshuffle well-dated ones.  Ensures a parent is ingested
  before its replies when both land in the same batch — descendant
  threading relies on the parent already existing in the DB."
  [msgs]
  (sort-by (fn [msg]
             [(or (some-> ^Date (:date-sent msg) .getTime) Long/MAX_VALUE)
              (or (some-> ^Date (:date-received msg) .getTime) Long/MAX_VALUE)
              (or (:uid msg) Long/MAX_VALUE)])
           msgs))

(defn- safe-store-and-process!
  "Run store-and-process! on `msg`, logging and swallowing exceptions.
  Returns the result keyword (:ok/:skip/:retry) or nil on exception.
  Honours `shutting-down?` — callers that iterate should also check it
  via a `:while` clause to stop cleanly."
  [db-conn source-map sources msg ingest-opts]
  (try
    (store-and-process! db-conn source-map sources msg ingest-opts)
    (catch Exception e
      (log/error e "Failed to process id:" (:id msg))
      nil)))

(defn- collect-safe-uids
  "Process `msgs` and return the set of UIDs for messages that didn't
  need a retry.  Used by the IMAP path to decide how far the UID
  watermark may advance."
  [db-conn source-map sources msgs ingest-opts]
  (reduce (fn [acc msg]
            (let [result (safe-store-and-process! db-conn source-map sources msg ingest-opts)]
              (if (and (not= :retry result) (:uid msg))
                (conj acc (:uid msg))
                acc)))
          #{} msgs))

(defn- process-each!
  "Ingest every message in `msgs` in order, stopping early on shutdown.
  Exceptions from a single message are logged and don't abort the loop."
  [db-conn source-map sources msgs ingest-opts]
  (doseq [msg msgs
          :while (not (shutting-down?))]
    (safe-store-and-process! db-conn source-map sources msg ingest-opts)))

(defn- catch-up-imap!
  "IMAP incremental fetch: use UID watermark to fetch only new messages.
  The batch is sorted chronologically before processing so parents are
  ingested before their replies within the same fetch.

  Before fetching we check the folder's UIDVALIDITY: if it has changed
  since the last run, the stored UID watermark points nowhere and must
  be cleared — otherwise `by-id-range` would silently return nothing
  forever. On reset we fall through to the first-run fetch path."
  [src db-conn folder fetch-opts source-map sources ingest-opts]
  (let [live-uv   (try (mailseq/uid-validity src folder)
                       (catch Exception e
                         (log/debug "Could not read UIDVALIDITY:" (.getMessage e))
                         nil))
        _         (ingest/sync-uid-validity! db-conn live-uv)
        watermark (ingest/max-imap-uid db-conn)
        msgs (sort-chronologically
              (if (zero? watermark)
                (first-run-messages src folder fetch-opts)
                (do (log/info "Resuming — fetching UIDs >" watermark)
                    (mailseq/by-id-range src folder
                                         (str (inc watermark)) nil))))]
    (log/info "Fetched" (count msgs) "messages")
    (when (and (seq msgs) (not (shutting-down?)))
      (let [safe-ids (collect-safe-uids db-conn source-map sources msgs ingest-opts)]
        (advance-watermark! db-conn msgs safe-ids)))))

(defn- catch-up-maildir!
  "Maildir incremental fetch: diff list-ids against known :email/id in DB.
  On first run (maildir-init not yet done), uses mailseq/messages with
  fetch-opts to honour :limit/:since, then records all pre-existing ids
  as seen.  The init flag is set last so a crash mid-first-run safely
  retries (store-and-process! is idempotent for already-stored emails).
  Both paths sort the batch chronologically so parents are ingested
  before their replies within the same fetch."
  [src db-conn folder fetch-opts source-map sources ingest-opts]
  (let [init-done? (ingest/maildir-init-done? db-conn)
        all-ids    (mailseq/list-ids src folder)]
    (if init-done?
      ;; Incremental run: diff against stored emails + seen baseline
      (let [known   (into (ingest/known-email-ids db-conn)
                          (ingest/seen-maildir-ids db-conn))
            new-ids (remove known all-ids)]
        (if (empty? new-ids)
          (log/info "No new messages in Maildir")
          (let [msgs (sort-chronologically
                      (mailseq/by-ids src folder (vec new-ids)))]
            (log/info "Fetched" (count msgs) "new messages from Maildir")
            (process-each! db-conn source-map sources msgs ingest-opts))))
      ;; First run (or retry after crash): fetch limited set, then seal baseline
      (let [msgs (sort-chronologically (first-run-messages src folder fetch-opts))]
        (cond
          (and (empty? msgs) (seq all-ids))
          (log/warn "First-run filter matched 0 of" (count all-ids)
                    "Maildir files — verify :fetch and :folder"
                    "(all" (count all-ids) "ids will be sealed as seen)")
          (empty? msgs)
          (log/info "No new messages in Maildir")
          :else
          (do (log/info "Fetched" (count msgs) "messages from Maildir (first run)")
              (process-each! db-conn source-map sources msgs ingest-opts)))
        ;; Record all pre-existing ids not yet stored, then flag init done.
        ;; If we crash before this point, the next run retries the first-run
        ;; path — store-and-process! skips already-stored emails harmlessly.
        (let [stored (ingest/known-email-ids db-conn)
              unseen (remove stored all-ids)]
          (when (seq unseen)
            (log/info "Marking" (count unseen) "pre-existing Maildir ids as seen")
            (ingest/mark-ids-seen! db-conn unseen)))
        (ingest/set-maildir-init-done! db-conn)))))

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

(defn- maildir-folder-path
  "Resolve a :maildir mailbox config to its on-disk folder path."
  [{:keys [path folder] :or {folder "INBOX"}}]
  (str path "/" folder))

(defn- mailbox->mailseq-cfg
  "Convert a bark :mailbox config map to the format expected by mailseq/open.
  Maps the single :folder key to the :folders map mailseq expects."
  [{:keys [type folder] :or {folder "INBOX"} :as cfg}]
  (let [base    (dissoc cfg :folder :path)
        folders (case type
                  :imap    {folder folder}
                  :maildir {folder (maildir-folder-path cfg)})]
    (assoc base :folders folders)))

(defn open-mailbox [mailbox-cfg]
  (try
    (log/info "Opening mailbox" (pr-str (:type mailbox-cfg))
              (case (:type mailbox-cfg)
                :imap    (str (:user mailbox-cfg) "@" (:host mailbox-cfg))
                :maildir (maildir-folder-path mailbox-cfg)
                ""))
    (mailseq/open (mailbox->mailseq-cfg mailbox-cfg))
    (catch Exception e
      (log/error e "Mailbox connection failed:" (blog/exception-msg e))
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
          (log/error e "Expire failed:" (blog/exception-msg e))
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

;; ---------------------------------------------------------------------------
;; Run context — shared by batch and watch modes
;; ---------------------------------------------------------------------------

(defn- run-opts
  "Derive the per-run options from mailbox and ingest config.  Same shape
  for batch and watch — factoring this prevents the two paths drifting."
  [mailbox-cfg ingest-cfg]
  {:folder       (or (:folder mailbox-cfg) "INBOX")
   :mailbox-type (:type mailbox-cfg)
   :fetch-opts   (parse-fetch (or (:fetch ingest-cfg) {:limit 50}))
   :ingest-opts  (select-keys ingest-cfg [:max-size :max-attachment-size])})

(defn- init-roles! [db-conn config]
  (roles/ensure-source-roles! db-conn config)
  (doseq [{:keys [name]} (:sources config)]
    (roles/ensure-notify-defaults! db-conn name
                                   (roles/get-tenures (d/db db-conn) name))))

(defn- load-context
  "Re-read config, seed any newly-added maintainers into the tenure
  model, and derive source-map/sources.  Called once at startup in
  batch mode, and on every reconnect in watch mode so edits to
  config.edn — including new maintainers — take effect without a
  restart.  `ensure-source-roles!` and `ensure-notify-defaults!` are
  both idempotent, so re-running them on every reconnect is safe."
  [db-conn config-path]
  (let [config (or (common/load-config config-path) {})]
    (init-roles! db-conn config)
    {:source-map (common/build-source-map config)
     :sources    (or (:sources config) [])}))

(defn- close-mailbox! [src]
  (try (mailseq/close src)
       (catch Exception e
         (log/debug "Mailbox close failed:" (.getMessage e)))))

(defn watch-loop!
  "Run watch with automatic reconnection and exponential backoff.
  Reloads config.edn on each reconnect so changes take effect."
  [mailbox-cfg db-conn ingest-cfg config-path]
  (let [{:keys [folder mailbox-type fetch-opts ingest-opts]}
        (run-opts mailbox-cfg ingest-cfg)]
    (loop [backoff-ms 1000
           last-expire-ms 0]
      (when-not (shutting-down?)
        (let [{:keys [source-map sources]} (load-context db-conn config-path)
              src (open-mailbox mailbox-cfg)]
          (if-not src
            (do (log/error "Mailbox connection failed, retrying in" (/ backoff-ms 1000) "s")
                (Thread/sleep backoff-ms)
                (recur (min (* backoff-ms 2) max-backoff-ms) last-expire-ms))
            (let [new-expire-ms
                  (try
                    (log/info "Mailbox connected, folder:" folder)
                    (catch-up-fetch! src db-conn folder fetch-opts source-map sources ingest-opts mailbox-type)
                    (let [ts (maybe-expire! db-conn source-map last-expire-ms)]
                      (when-not (shutting-down?)
                        (start-watch! src db-conn folder source-map sources ingest-opts))
                      ts)
                    (catch Exception e
                      (log/error e "Watch interrupted:" (blog/exception-msg e))
                      last-expire-ms))]
              (close-mailbox! src)
              (when-not (shutting-down?)
                (log/debug "Watch exited, reconnecting in 1s")
                (Thread/sleep 1000)
                (recur 1000 new-expire-ms)))))))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn- batch-run!
  "Single-pass mode (default): connect, fetch new messages, expire, exit."
  [mailbox-cfg db-conn ingest-cfg config-path]
  (let [{:keys [folder mailbox-type fetch-opts ingest-opts]}
        (run-opts mailbox-cfg ingest-cfg)
        {:keys [source-map sources]} (load-context db-conn config-path)
        src (open-mailbox mailbox-cfg)]
    (when-not src
      (log/error "Mailbox connection failed.")
      (System/exit 1))
    (try
      (catch-up-fetch! src db-conn folder fetch-opts source-map sources ingest-opts mailbox-type)
      (expire/expire-reports! db-conn source-map)
      (finally
        (close-mailbox! src)))))

(defn- parse-main-args [args]
  (let [arg-set (set args)
        pairs   (partition 2 args)]
    {:watch?      (arg-set "--watch")
     :cli-fetch   (some (fn [[a b]] (when (= "--fetch" a) b)) pairs)
     :config-path (or (some (fn [[a b]] (when (= "-c" a) b)) pairs) "config.edn")}))

(defn- setup-logging! [config]
  (when-let [logging (:logging config)]
    (blog/configure-file-logging! logging)
    (when-let [email-cfg (:email logging)]
      (if-let [smtp (get-in config [:notifications :smtp])]
        (configure-email-logging! smtp email-cfg)
        (log/warn "Logging :email configured but no :notifications :smtp found.")))))

(defn- validate-mailbox-cfg! [mailbox-cfg]
  (when-not mailbox-cfg
    (log/error "No :mailbox key in config.edn.")
    (System/exit 1))
  (when-not (#{:imap :maildir} (:type mailbox-cfg))
    (log/error "Invalid :type in :mailbox — expected :imap or :maildir, got:"
               (pr-str (:type mailbox-cfg)))
    (System/exit 1)))

(defn- install-shutdown-hook! [db-conn]
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
      (log/info "Goodbye.")))))

(defn -main [& args]
  (let [{:keys [watch? cli-fetch config-path]} (parse-main-args args)
        config (common/load-config config-path)]
    (when (nil? config)
      (log/error "Config file not found:" config-path)
      (System/exit 1))
    (let [mailbox-cfg (:mailbox config)
          ingest-cfg  (cond-> (or (:ingest config) {})
                        cli-fetch (assoc :fetch (cli-fetch->map cli-fetch)))]
      (setup-logging! config)
      (validate-mailbox-cfg! mailbox-cfg)
      (let [db-conn (ingest/connect (or (:path (:db config)) "data/bark-db"))]
        (log/info "Datalevin connected.")
        (install-shutdown-hook! db-conn)
        (if watch?
          (watch-loop! mailbox-cfg db-conn ingest-cfg config-path)
          (do (batch-run! mailbox-cfg db-conn ingest-cfg config-path)
              ;; Datalevin/LMDB keeps non-daemon threads alive; without an
              ;; explicit close + System/exit the JVM hangs after batch mode.
              (try (ingest/close db-conn)
                   (catch Exception e
                     (log/debug "DB close failed:" (.getMessage e))))
              (shutdown-agents)
              (System/exit 0)))))))
