;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.main
  "Entry point for BONE. Connects to a mail source (IMAP or Maildir),
  fetches new emails since the last run, and stores+processes them
  atomically. Default mode is single-pass (batch); use --watch for
  persistent watching (IMAP IDLE or filesystem events)."
  (:require [bone.ingest :as ingest]
            [bone.logging :as blog]
            [bone.common :as common]
            [bone.digest :as digest]
            [bone.expire :as expire]
            [bone.roles :as roles]
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
                              :subject (str "[BONE] " level-str " -- " (:?ns-str data))
                              :body    (str (force (:timestamp_ data)) " " level-str " "
                                            (:?ns-str data) " -- " msg)}))
                          (catch Exception e
                            (.println System/err
                                      (str "Failed to send log email: " (.getMessage e))))))}}}))))

;; ---------------------------------------------------------------------------
;; Shutdown coordination
;; ---------------------------------------------------------------------------

(def ^:private shutdown? (atom false))

(defn shutting-down? [] @shutdown?)

;; Futures spawned by `watch-all!`.  Stored here so the shutdown hook
;; can wait for them to finish before closing the database.  Nil in
;; batch mode.
(def ^:private watch-futures (atom nil))

;; ---------------------------------------------------------------------------
;; Fetch parsing
;;
;; :fetch accepts exactly one of three disjoint map shapes -- strict,
;; no key mixing, empty map rejected:
;;
;;   {:limit N}              -- latest N messages (pos-int)
;;   {:since "Nd"|"Nw"|...}  -- relative duration from now (duration-only)
;;   {:start "yyyy-MM-dd"
;;    :end   "yyyy-MM-dd"?}  -- absolute window; :start alone, :end alone,
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
                      "or {:start/:end ISO} -- no key mixing, no empty map)") v))))

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
  "Last UID of the unbroken safe-uids prefix in `all-uids` (sorted).
  Returns nil if the very first UID isn't safe -- the watermark only
  advances past in-sequence successes, so failures get retried."
  [all-uids safe-uids]
  (reduce (fn [acc uid]
            (if (contains? safe-uids uid) uid (reduced acc)))
          nil all-uids))

(defn- advance-watermark! [db-conn mailbox-name msgs safe-ids]
  (let [all-uids (->> msgs (keep :uid) sort)]
    (if-let [new-wm (max-contiguous-safe-uid all-uids safe-ids)]
      (do (when (not= new-wm (some->> all-uids last))
            (log/warn "Watermark stopped at UID" new-wm
                      "(some messages failed -- will retry on next reconnect)"))
          (ingest/save-imap-uid! db-conn mailbox-name new-wm))
      ;; First UID in batch failed -- watermark cannot advance at all.
      (when (seq all-uids)
        (log/warn "Watermark not advanced: first UID" (first all-uids)
                  "failed -- entire batch of" (count all-uids)
                  "message(s) will be retried on next reconnect")))))

;; ---------------------------------------------------------------------------
;; Atomic store+process
;; ---------------------------------------------------------------------------

;; Message-ids whose digest is currently in flight on some thread.
;; In multi-mailbox watch mode, two mailboxes that subscribe to the
;; same list may both observe a new email and race to digest it
;; before `:email/digested-at` lands.  `store-email!` already
;; prevents duplicate DB entities, but a sibling thread that sees
;; "stored but not digested" then re-enters `try-digest!` --
;; legitimately, for crash recovery, but incorrectly here.  Mids in
;; this set are claimed by another thread; concurrent callers skip.
(def ^:private digesting-mids (atom #{}))

(defn take-mid-ownership!
  "Atomically register `mid` as digested-by-us.  Returns true if the
  caller now owns the digest and must release via
  `release-mid-ownership!`; false if another thread already owns it."
  [mid]
  (let [[old _] (swap-vals! digesting-mids conj mid)]
    (not (contains? old mid))))

(defn release-mid-ownership!
  "Release the mid claimed by `take-mid-ownership!`."
  [mid]
  (swap! digesting-mids disj mid))

(defn- try-digest!
  "Run process-email!, returning :ok on success, :retry on exception.
  Concurrent callers for the same mid return :ok without re-running
  -- a sibling thread already owns it (see `digesting-mids`).  The
  `:ok` here means \"do not retry / watermark may advance\", not
  \"I digested\" : the sibling thread is responsible for posting
  :email/digested-at.  `resolved-source` (or nil) is forwarded to
  `process-email!` to skip re-classifying a freshly stored email."
  [{:keys [db-conn source-map sources]} email mid resolved-source]
  (if-not (take-mid-ownership! mid)
    (do (log/debug "Digest already in flight for" mid "-- skipping")
        :ok)
    (try
      (digest/process-email! db-conn source-map sources email
                             {:resolved-source resolved-source})
      :ok
      (catch Exception e
        (log/error e "Failed to digest email" mid (blog/exception-msg e))
        :retry)
      (finally
        (release-mid-ownership! mid)))))

(defn- guard-reject-reason
  "Pure pre-storage guard. nil = accept; else a reason keyword:
  :oversized, :no-mid, or :oversized-mid (over the LMDB key limit)."
  [msg mid max-size]
  (let [size (:size msg -1)]
    (cond
      (and max-size (pos? size) (> size max-size)) :oversized
      (nil? mid)                                   :no-mid
      (not (common/indexable-mid? mid))            :oversized-mid)))

(defn- store-and-process!
  "Classify, store, and digest an email.  Returns :ok (advance watermark),
  :skip (oversized/no source/no mid -- still advance), or :retry
  (transient failure).  Idempotent: emails previously stored but not
  digested are re-digested; already-digested ones skipped."
  [{:keys [db-conn sources ingest-opts] :as ctx} msg]
  (let [{:keys [max-size max-attachment-size]} ingest-opts
        id   (:id msg)
        ;; Normalize like `store-email!` so :email/message-id lookups
        ;; resolve consistently after store (raw mid padding can drift).
        mid  (common/extract-bracketed-id (:message-id msg))]
    (if-let [reason (guard-reject-reason msg mid max-size)]
      (do (case reason
            :oversized     (log/warn "Skipping oversized email id:" id
                                     "size:" (:size msg -1) (str "bytes (max: " max-size ")"))
            :no-mid        (log/warn "No Message-ID for id:" id "-- skipping")
            :oversized-mid (log/warn "Skipping email with oversized Message-ID ("
                                     (count mid) "chars), id:" id))
          :skip)
      ;; Classify the source once here; the result is threaded to
      ;; process-email! (:resolved-source) so it isn't re-classified after store.
      (let [{:keys [src-name] :as resolved}
            (digest/pre-classify-source (d/db db-conn) sources msg)]
        (if-not src-name
          (do (log/debug "No matching source for id:" id "-- not stored")
              :skip)
          (let [lookup     [:email/message-id mid]
                store-opts (cond-> {:source src-name :message-id mid}
                             max-attachment-size (assoc :max-attachment-size
                                                        max-attachment-size))]
            (if (ingest/store-email! db-conn msg store-opts)
              (let [eid (d/entid (d/db db-conn) lookup)]
                (try-digest! ctx
                             (d/pull (d/db db-conn) digest/email-pull-pattern eid)
                             mid resolved))
              ;; store-email! returned false: either dup mid (re-digest the
              ;; existing entity) or id collision (different mid stored under
              ;; the same Maildir filename -- skip).  On re-digest, pass no
              ;; :resolved-source so process-email! reads the stored
              ;; :email/source (the entity may predate a config change).
              (if-let [eid (d/entid (d/db db-conn) lookup)]
                (let [email (d/pull (d/db db-conn) digest/email-pull-pattern eid)]
                  (cond
                    (:email/digested-at email)
                    (do (log/debug "Already digested, skipping:" mid) :ok)

                    :else
                    (do (log/info "Re-processing previously stored email:" mid)
                        (when-not (:email/source email)
                          (d/transact! db-conn [{:db/id eid :email/source src-name}]))
                        (try-digest! ctx email mid nil))))
                (do (log/warn "Skipping id collision (different Message-ID stored):" mid)
                    :skip)))))))))

;; ---------------------------------------------------------------------------
;; Catch-up fetch (store+process per email)
;; ---------------------------------------------------------------------------

(defn- log-first-run [{:keys [limit since]}]
  (log/info "First run -- fetching"
            (if since (str "messages since " since) (str "last " (or limit "all") " messages"))))

(defn- first-run-messages [src folder fetch-opts]
  (log-first-run fetch-opts)
  (mailseq/messages src folder (merge {:attachments? true} fetch-opts)))

(defn- sort-chronologically
  "Oldest-first by Date: header (fallback: receive time, UID).
  Ensures parents are stored before replies in a single batch -- the
  descendant threading needs the parent to exist in the DB."
  [msgs]
  (sort-by (fn [msg]
             [(or (some-> ^Date (:date-sent msg) .getTime) Long/MAX_VALUE)
              (or (some-> ^Date (:date-received msg) .getTime) Long/MAX_VALUE)
              (or (:uid msg) Long/MAX_VALUE)])
           msgs))

(defn- safe-store-and-process!
  "Run store-and-process! on `msg`, logging and swallowing exceptions.
  Returns the result keyword (:ok/:skip/:retry); :retry on exception so
  the IMAP watermark stays put and the message gets a fresh attempt on
  the next catch-up.  Honours `shutting-down?` -- callers that iterate
  should also check it via a :while clause to stop cleanly."
  [ctx msg]
  (try
    (store-and-process! ctx msg)
    (catch Exception e
      (log/error e "Failed to process id:" (:id msg) (blog/exception-msg e))
      :retry)))

(defn- collect-safe-uids
  "Set of UIDs from `msgs` that didn't need retry.  Caps the IMAP
  watermark advance."
  [ctx msgs]
  (reduce (fn [acc msg]
            (let [result (safe-store-and-process! ctx msg)]
              (if (and (not= :retry result) (:uid msg))
                (conj acc (:uid msg))
                acc)))
          #{} msgs))

(defn- process-each!
  "Ingest each message in order; per-message exceptions are logged and
  don't abort the loop.  Stops early on shutdown."
  [ctx msgs]
  (doseq [msg msgs
          :while (not (shutting-down?))]
    (safe-store-and-process! ctx msg)))

(defn- catch-up-imap!
  "IMAP incremental fetch via UID watermark; falls back to first-run
  on UIDVALIDITY change (stored watermark is cleared)."
  [{:keys [db-conn mailbox-name] :as ctx} src folder fetch-opts]
  (let [live-uv   (try (mailseq/uid-validity src folder)
                       (catch Exception e
                         (log/debug "Could not read UIDVALIDITY:" (.getMessage e))
                         nil))
        _         (ingest/sync-uid-validity! db-conn mailbox-name live-uv)
        watermark (ingest/max-imap-uid db-conn mailbox-name)
        msgs (sort-chronologically
              (if (zero? watermark)
                (first-run-messages src folder fetch-opts)
                (do (log/info "Resuming -- fetching UIDs >" watermark)
                    (mailseq/by-id-range src folder
                                         (str (inc watermark)) nil))))]
    (log/info "Fetched" (count msgs) "messages")
    (when (and (seq msgs) (not (shutting-down?)))
      (let [safe-ids (collect-safe-uids ctx msgs)]
        (advance-watermark! db-conn mailbox-name msgs safe-ids)))))

(defn- catch-up-maildir!
  "Maildir incremental fetch: diff list-ids against known :email/id.
  First run honours fetch-opts then seals pre-existing ids as seen;
  the init flag is set last so a crash retries safely."
  [{:keys [db-conn mailbox-name] :as ctx} src folder fetch-opts]
  (let [init-done? (ingest/maildir-init-done? db-conn mailbox-name)
        all-ids    (mailseq/list-ids src folder)]
    (if init-done?
      ;; Incremental run: diff against stored emails + seen baseline
      (let [known   (into (ingest/known-email-ids db-conn)
                          (ingest/seen-maildir-ids db-conn mailbox-name))
            new-ids (remove known all-ids)]
        (if (empty? new-ids)
          (log/info "No new messages in Maildir")
          (let [msgs (sort-chronologically
                      (mailseq/by-ids src folder (vec new-ids)))]
            (log/info "Fetched" (count msgs) "new messages from Maildir")
            (process-each! ctx msgs))))
      ;; First run (or retry after crash): fetch limited set, then seal baseline
      (let [msgs (sort-chronologically (first-run-messages src folder fetch-opts))]
        (cond
          (and (empty? msgs) (seq all-ids))
          (log/warn "First-run filter matched 0 of" (count all-ids)
                    "Maildir files -- verify :fetch and :folder"
                    "(all" (count all-ids) "ids will be sealed as seen)")
          (empty? msgs)
          (log/info "No new messages in Maildir")
          :else
          (do (log/info "Fetched" (count msgs) "messages from Maildir (first run)")
              (process-each! ctx msgs)))
        ;; Seal pre-existing ids as seen, then flag init done.
        (let [stored (ingest/known-email-ids db-conn)
              unseen (remove stored all-ids)]
          (when (seq unseen)
            (log/info "Marking" (count unseen) "pre-existing Maildir ids as seen")
            (ingest/mark-ids-seen! db-conn mailbox-name unseen)))
        (ingest/set-maildir-init-done! db-conn mailbox-name)))))

(defn catch-up-fetch!
  "Fetch messages missed while the process was down.
  Dispatches to IMAP (watermark) or Maildir (id diff) strategy."
  [ctx src folder fetch-opts mailbox-type]
  (when-not (shutting-down?)
    (case mailbox-type
      :imap    (catch-up-imap! ctx src folder fetch-opts)
      :maildir (catch-up-maildir! ctx src folder fetch-opts))))

;; ---------------------------------------------------------------------------
;; Mail source connection
;; ---------------------------------------------------------------------------

(defn- maildir-folder-path
  "On-disk folder path for a :maildir mailbox config."
  [{:keys [path folder] :or {folder "INBOX"}}]
  (str (str/replace (common/expand-home path) #"/+$" "") "/" folder))

(defn- mailbox->mailseq-cfg
  "Translate bone's :mailbox config into mailseq/open's :folders form."
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

;; Days before pending-thread emails are force-flushed (genuine
;; orphans must eventually leave the queue).
(def ^:private pending-flush-max-age-days 7)

;; Last successful expire+flush timestamp.  Shared across all watch
;; threads: a single CAS gates the daily run so N watch loops never
;; duplicate work.
(def ^:private last-expire-ms (atom 0))

;; How often the dedicated scheduler thread wakes to attempt
;; maybe-expire!.  The real frequency-of-execution is once per day
;; (gated by the CAS inside maybe-expire!); this interval just sets
;; how quickly we react when the 24-hour window opens and how long
;; the scheduler thread takes to notice a shutdown.
(def ^:private expire-tick-ms (* 60 60 1000))

(defn- maybe-expire!
  "Run expire + flush-stale-pending if at least a day has elapsed
  since the last successful run.  Thread-safe -- a single atomic CAS
  picks which caller executes the work; concurrent callers return
  without retrying.  On failure the timestamp is rolled back via CAS
  so a sibling thread can retry at its next reconnect (rather than
  waiting another 24 hours)."
  [db-conn source-map sources]
  (let [now  (System/currentTimeMillis)
        prev @last-expire-ms]
    (when (and (> (- now prev) one-day-ms)
               (compare-and-set! last-expire-ms prev now))
      (try
        (expire/expire-reports! db-conn source-map)
        (digest/flush-stale-pending! db-conn source-map sources
                                     pending-flush-max-age-days)
        (catch Exception e
          (log/error e "Expire/flush failed:" (blog/exception-msg e))
          ;; Roll the watermark back.  If a sibling already moved it
          ;; forward in the meantime (very unlikely given the 24h
          ;; gate), the CAS is a no-op -- that sibling is in charge.
          (compare-and-set! last-expire-ms now prev))))))

;; ---------------------------------------------------------------------------
;; Watch mode with reconnection
;; ---------------------------------------------------------------------------

(def ^:private max-backoff-ms (* 5 60 1000))

(defn start-watch!
  "Start watching for new messages, storing+processing each as it arrives."
  [{:keys [db-conn mailbox-name] :as ctx} src folder]
  (log/info "Mailbox" (pr-str mailbox-name) "-- starting watch on" folder)
  (mailseq/watch src folder
                 (fn [msg]
                   (when-not (shutting-down?)
                     (if (nil? msg)
                       (log/warn "Mailbox" (pr-str mailbox-name)
                                 "-- watch delivered nil message, skipping")
                       (do
                         (log/info "Mailbox" (pr-str mailbox-name)
                                   "-- new message id:" (:id msg)
                                   "Subject:" (:subject msg))
                         (try
                           (let [result (store-and-process! ctx msg)]
                             ;; Advance IMAP watermark when applicable
                             (when (and (not= :retry result) (:uid msg))
                               (ingest/save-imap-uid! db-conn mailbox-name (:uid msg))))
                           (catch Exception e
                             (log/error e "Mailbox" (pr-str mailbox-name)
                                        "-- error processing message id:" (:id msg)
                                        (blog/exception-msg e))))))))
                 {:parse-opts   {:attachments? true}
                  :heartbeat-ms (* 20 60 1000)}))

;; ---------------------------------------------------------------------------
;; Run context -- shared by batch and watch modes
;; ---------------------------------------------------------------------------

(defn- run-opts
  "Per-run options for one mailbox.  Effective ingest settings are
  (merge global-ingest local-ingest), with a CLI --fetch override
  winning over both.  Priority: CLI > mailbox :ingest > global
  :ingest > defaults."
  [mailbox-cfg ingest-cfg cli-fetch-map]
  (let [effective (cond-> (merge ingest-cfg (:ingest mailbox-cfg))
                    cli-fetch-map (assoc :fetch cli-fetch-map))]
    {:mailbox-name (:name mailbox-cfg)
     :folder       (or (:folder mailbox-cfg) "INBOX")
     :mailbox-type (:type mailbox-cfg)
     :fetch-opts   (parse-fetch (or (:fetch effective) {:limit 50}))
     :ingest-opts  (select-keys effective [:max-size :max-attachment-size])}))

(defn- make-run-ctx
  "Bundle the run-invariant context for one mailbox pass, threaded as a
  single map through the catch-up / store-and-process call chain."
  [db-conn mailbox-name source-map sources ingest-opts]
  {:db-conn      db-conn
   :mailbox-name mailbox-name
   :source-map   source-map
   :sources      sources
   :ingest-opts  ingest-opts})

(defn- load-context
  "Re-read config, seed maintainers into tenures, return source-map+sources.
  Called once in batch mode, on every reconnect in watch mode (so
  config edits take effect without restart).  Idempotent."
  [db-conn config-path]
  (let [config (or (common/load-config config-path) {})]
    (roles/sync-all-sources! db-conn config)
    {:source-map (common/build-source-map config)
     :sources    (or (:sources config) [])}))

(defn- close-mailbox! [src]
  (try (mailseq/close src)
       (catch Exception e
         (log/debug "Mailbox close failed:" (.getMessage e)))))

(defn- live-run-opts
  "Recompute run-opts from the latest config.edn so edits to this
  mailbox's own :ingest, :folder, host/credentials etc. take effect on
  reconnect.  Falls back to the original mailbox-cfg / ingest-cfg when
  the entry has been removed from the config between reconnects --
  silently dropping a live mailbox would be more confusing than
  carrying on with the last known state."
  [mailbox-cfg ingest-cfg cli-fetch-map config-path]
  (let [config      (common/load-config config-path)
        live-mb     (or (some #(when (= (:name %) (:name mailbox-cfg)) %)
                              (:mailboxes config))
                        mailbox-cfg)
        live-ingest (or (:ingest config) ingest-cfg)]
    (assoc (run-opts live-mb live-ingest cli-fetch-map)
           :mailbox-cfg live-mb)))

(defn watch-loop!
  "Run watch on one mailbox with automatic reconnection and
  exponential backoff.  Reloads config.edn on each reconnect so
  changes take effect.  Designed to be run in its own future when
  several mailboxes are configured."
  [mailbox-cfg db-conn ingest-cfg cli-fetch-map config-path]
  (loop [backoff-ms 1000]
    (when-not (shutting-down?)
      (let [;; Config reload + connection run inside try: a half-edited
            ;; config.edn (edn/read-string throws) or a bad :fetch value
            ;; must back off and retry, not kill this watch loop for good.
            setup (try
                    (let [{:keys [source-map sources]} (load-context db-conn config-path)
                          {:keys [mailbox-cfg mailbox-name folder mailbox-type
                                  fetch-opts ingest-opts]}
                          (live-run-opts mailbox-cfg ingest-cfg cli-fetch-map config-path)
                          ctx (make-run-ctx db-conn mailbox-name source-map sources
                                            ingest-opts)]
                      {:mailbox-name mailbox-name
                       :folder       folder
                       :mailbox-type mailbox-type
                       :fetch-opts   fetch-opts
                       :source-map   source-map
                       :sources      sources
                       :ctx          ctx
                       :src          (open-mailbox mailbox-cfg)})
                    (catch Exception e
                      (log/error e "Mailbox" (pr-str (:name mailbox-cfg))
                                 "-- setup failed:" (blog/exception-msg e))
                      nil))
            {:keys [mailbox-name folder mailbox-type fetch-opts
                    source-map sources ctx src]} setup]
        (if-not src
          (do (log/error "Mailbox" (pr-str (or mailbox-name (:name mailbox-cfg)))
                         "-- connection failed, retrying in" (/ backoff-ms 1000) "s")
              (Thread/sleep backoff-ms)
              (recur (min (* backoff-ms 2) max-backoff-ms)))
          (do
            (try
              (log/info "Mailbox" (pr-str mailbox-name) "-- connected, folder:" folder)
              (catch-up-fetch! ctx src folder fetch-opts mailbox-type)
              (maybe-expire! db-conn source-map sources)
              (when-not (shutting-down?)
                (start-watch! ctx src folder))
              (catch Exception e
                (log/error e "Mailbox" (pr-str mailbox-name)
                           "-- watch interrupted:" (blog/exception-msg e))))
            (close-mailbox! src)
            (when-not (shutting-down?)
              (log/debug "Mailbox" (pr-str mailbox-name) "-- watch exited, reconnecting in 1s")
              (Thread/sleep 1000)
              (recur 1000))))))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn- expire-scheduler!
  "Periodically wake to give maybe-expire! a chance to run.  Necessary
  because start-watch! blocks on IMAP IDLE / inotify -- without this
  thread, expire and flush-stale-pending would only run when a watch
  loop reconnects.  The actual work is rate-limited to once per day
  by the CAS inside maybe-expire!.  Exits cleanly on shutdown."
  [db-conn config-path]
  (while (not (shutting-down?))
    (Thread/sleep ^long expire-tick-ms)
    (when-not (shutting-down?)
      (try
        (let [{:keys [source-map sources]} (load-context db-conn config-path)]
          (maybe-expire! db-conn source-map sources))
        (catch Exception e
          (log/error e "Scheduled expire failed:" (blog/exception-msg e)))))))

(defn- watch-all!
  "Run watch on every configured mailbox in parallel: one future per
  mailbox, plus a dedicated future for the daily expire scheduler.
  The futures are published into `watch-futures` so the shutdown hook
  can wait for them before closing the DB.  The main thread blocks on
  `run! deref` over the watch loops until every one exits (typically
  only on shutdown); the scheduler exits via the shared shutdown flag.
  Uncaught throwables inside a future are logged so a dead thread
  cannot silently disappear."
  [mailboxes db-conn ingest-cfg cli-fetch-map config-path]
  (let [watch-futs (mapv (fn [mb]
                           (future
                             (try
                               (watch-loop! mb db-conn ingest-cfg cli-fetch-map config-path)
                               (catch Throwable t
                                 (log/error t "Mailbox" (pr-str (:name mb))
                                            "-- watch loop terminated:"
                                            (blog/exception-msg t))))))
                         mailboxes)
        sched-fut  (future
                     (try
                       (expire-scheduler! db-conn config-path)
                       (catch Throwable t
                         (log/error t "Expire scheduler terminated:"
                                    (blog/exception-msg t)))))]
    (reset! watch-futures (conj watch-futs sched-fut))
    (run! deref watch-futs)
    ;; Reached when every watch loop has exited.  Under normal
    ;; operation that only happens during shutdown; otherwise every
    ;; thread died and the daemon is doing nothing useful -- treat
    ;; that as a fatal condition rather than silently returning.
    (when-not (shutting-down?)
      (log/error "All" (count mailboxes)
                 "watch loop(s) exited without a shutdown signal -- aborting.")
      (System/exit 1))))

(defn- batch-fetch-one!
  "Open, catch-up, and close a single mailbox in batch mode.  Returns
  true on success, false on connection failure or fetch error (so
  the caller can decide whether at least one mailbox advanced)."
  [mailbox-cfg db-conn ingest-cfg cli-fetch-map source-map sources]
  (let [{:keys [mailbox-name folder mailbox-type fetch-opts ingest-opts]}
        (run-opts mailbox-cfg ingest-cfg cli-fetch-map)
        ctx (make-run-ctx db-conn mailbox-name source-map sources ingest-opts)]
    (log/info "Mailbox" (pr-str mailbox-name) "-- batch fetch starting")
    (if-let [src (open-mailbox mailbox-cfg)]
      (try
        (catch-up-fetch! ctx src folder fetch-opts mailbox-type)
        (log/info "Mailbox" (pr-str mailbox-name) "-- batch fetch done")
        true
        (catch Exception e
          (log/error e "Mailbox" (pr-str mailbox-name)
                     "-- batch fetch failed:" (blog/exception-msg e))
          false)
        (finally (close-mailbox! src)))
      (do (log/error "Mailbox" (pr-str mailbox-name) "-- connection failed, skipping")
          false))))

(defn- batch-run!
  "Single-pass mode (default): for each configured mailbox, connect,
  fetch new messages, then expire/flush once globally.  Continues
  across mailbox failures; exits non-zero only if every mailbox
  attempted actually failed.  A shutdown signalled mid-loop is
  honoured silently (no spurious exit 1)."
  [mailboxes db-conn ingest-cfg cli-fetch-map config-path]
  (let [{:keys [source-map sources]} (load-context db-conn config-path)
        results (doall
                 (for [mb mailboxes
                       :while (not (shutting-down?))]
                   (batch-fetch-one! mb db-conn ingest-cfg cli-fetch-map source-map sources)))]
    (when (and (not (shutting-down?))
               (seq results)
               (not-any? true? results))
      (log/error (count results) "mailbox(es) failed -- aborting before expire/flush.")
      (System/exit 1))
    (when-not (shutting-down?)
      (expire/expire-reports! db-conn source-map)
      (digest/flush-stale-pending! db-conn source-map sources
                                   pending-flush-max-age-days))))

(defn- parse-main-args [args]
  (let [arg-set (set args)
        ;; Sliding pairs: non-overlapping (partition 2) drops valued flags
        ;; depending on position (e.g. --watch --fetch 50 loses the 50).
        pairs   (partition 2 1 args)]
    {:watch?      (arg-set "--watch")
     :fresh?      (arg-set "--fresh")
     :cli-fetch   (some (fn [[a b]] (when (= "--fetch" a) b)) pairs)
     :config-path (or (some (fn [[a b]] (when (= "-c" a) b)) pairs)
                      (System/getenv "BONE_CONFIG")
                      "config.edn")}))

(defn- confirm-fresh! [db-path]
  (print (str "Wipe DB at " db-path "? [y/N] ")) (flush)
  (#{"y" "Y" "yes" "YES"} (some-> (read-line) clojure.string/trim)))

(defn- delete-recursively! [^java.io.File f]
  (when (.isDirectory f)
    (doseq [child (.listFiles f)] (delete-recursively! child)))
  (.delete f))

(defn- maybe-wipe-db! [db-path]
  (let [f (java.io.File. ^String db-path)]
    (when (.exists f)
      (if (confirm-fresh! db-path)
        (do (log/info "Wiping" (.getAbsolutePath f))
            (delete-recursively! f))
        (do (log/info "Aborted.") (System/exit 0))))))

(defn- setup-logging! [config]
  (when-let [logging (:logging config)]
    (blog/configure-file-logging! logging)
    (when-let [email-cfg (:email logging)]
      (if-let [smtp (get-in config [:notifications :smtp])]
        (configure-email-logging! smtp email-cfg)
        (log/warn "Logging :email configured but no :notifications :smtp found.")))))

(defn check-mailboxes
  "Validate the :mailboxes vector.  Returns either {:ok mailboxes}
  or {:error message}.  Rejects the singleton :mailbox key explicitly
  (BONE is in 0.y.z -- no rétrocompat).  Pure -- callers decide
  whether to exit or surface the error.  Name format is shared with
  :sources via `common/valid-config-name?`."
  [config]
  (let [mailboxes (:mailboxes config)]
    (or (when (contains? config :mailbox)
          {:error common/singleton-mailbox-error})
        (when (or (not (vector? mailboxes)) (empty? mailboxes))
          {:error ":mailboxes must be a non-empty vector of mailbox maps."})
        (some (fn [[idx mb]]
                (when-not (common/valid-config-name? (:name mb))
                  {:error (str "Mailbox at index " idx
                               " has invalid :name -- expected a non-blank string matching "
                               (pr-str (str common/config-name-regex))
                               ", got: " (pr-str (:name mb)))}))
              (map-indexed vector mailboxes))
        (some (fn [mb]
                (when-not (#{:imap :maildir} (:type mb))
                  {:error (str "Mailbox " (pr-str (:name mb))
                               " has invalid :type -- expected :imap or :maildir, got: "
                               (pr-str (:type mb)))}))
              mailboxes)
        (let [names (mapv :name mailboxes)]
          (when-not (common/all-distinct? names)
            {:error (str "Mailbox :name values must be unique, got: "
                         (pr-str names))}))
        {:ok mailboxes})))

(defn- validate-mailboxes!
  "Return the validated :mailboxes vector, or exit with a message."
  [config]
  (let [{:keys [ok error]} (check-mailboxes config)]
    (when error
      (log/error error)
      (System/exit 1))
    ok))

;; Total grace period (across all watch threads) when waiting for
;; futures to settle during shutdown.  Long enough to let an
;; in-flight `d/transact!` complete; short enough that wedged
;; `mailseq/watch` threads (which block on IMAP IDLE / NIO and
;; don't poll `shutdown?`) don't keep the JVM around forever.
(def ^:private shutdown-grace-ms 5000)

(defn- install-shutdown-hook! [db-conn]
  (.addShutdownHook
   (Runtime/getRuntime)
   (Thread.
    (fn []
      (log/info "Shutting down...")
      (reset! shutdown? true)
      ;; Let any active watch thread finish its current transact
      ;; before we close the DB underneath it.  The grace period is
      ;; shared across all futures (deadline-based) so a multi-
      ;; mailbox shutdown doesn't compound to N x grace.
      (when-let [futs @watch-futures]
        (let [deadline (+ (System/currentTimeMillis) shutdown-grace-ms)]
          (doseq [f futs]
            (let [remaining (max 0 (- deadline (System/currentTimeMillis)))]
              (deref f remaining :timeout)))))
      (try (ingest/close db-conn)
           (catch Exception e
             (log/debug "DB close failed:" (.getMessage e))))
      (shutdown-agents)
      (log/info "Goodbye.")))))

(defn -main [& args]
  (let [{:keys [watch? fresh? cli-fetch config-path]} (parse-main-args args)
        config (common/load-config config-path)]
    (when (nil? config)
      (log/error "Config file not found:" config-path)
      (System/exit 1))
    (setup-logging! config)
    (let [mailboxes     (validate-mailboxes! config)
          ingest-cfg    (or (:ingest config) {})
          cli-fetch-map (some-> cli-fetch cli-fetch->map)
          db-path       (common/expand-home
                         (or (:path (:db config)) "data/bone-db"))]
      (when fresh? (maybe-wipe-db! db-path))
      (let [db-conn (ingest/connect db-path)]
        (log/info "Datalevin connected.")
        (install-shutdown-hook! db-conn)
        (if watch?
          (watch-all! mailboxes db-conn ingest-cfg cli-fetch-map config-path)
          (do (batch-run! mailboxes db-conn ingest-cfg cli-fetch-map config-path)
              ;; Datalevin/LMDB keeps non-daemon threads alive; explicit
              ;; close + System/exit avoids a hung JVM after batch mode.
              (try (ingest/close db-conn)
                   (catch Exception e
                     (log/debug "DB close failed:" (.getMessage e))))
              (shutdown-agents)
              (System/exit 0)))))))
