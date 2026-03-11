;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark-ingest.main
  "Entry point for bark-ingest. Connects to a single IMAP mailbox,
  watches for new emails, and stores them in a Datalevin database.
  Source classification is deferred to bark-digest."
  (:require [bark-ingest.db :as db]
            [bark-ingest.ingest :as ingest]
            [fetch-imap.core :as imap]
            [fetch-imap.fetch :as fetch]
            [fetch-imap.idle :as idle]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [postal.core :as postal]
            [taoensso.timbre :as log])
  (:gen-class))

;; ---------------------------------------------------------------------------
;; Logging config — replaces logback.xml
;; ---------------------------------------------------------------------------

(log/merge-config!
 {:min-level [["datalevin.*"          :warn]
              ["org.eclipse.angus.*"  :warn]
              ["*"                    :info]]})

(defn- parse-size
  "Parse a size string like \"10MB\" into bytes. Supports KB, MB, GB."
  [s]
  (let [s (str/upper-case (str/trim (str s)))]
    (cond
      (str/ends-with? s "GB") (* (parse-long (str/replace s #"GB$" "")) 1024 1024 1024)
      (str/ends-with? s "MB") (* (parse-long (str/replace s #"MB$" "")) 1024 1024)
      (str/ends-with? s "KB") (* (parse-long (str/replace s #"KB$" "")) 1024)
      :else                   (parse-long s))))

(defn- rotate-log!
  "Rotate log-file if it exceeds max-bytes, keeping up to backlog files."
  [log-file max-bytes backlog]
  (let [f (io/file log-file)]
    (when (and (.exists f) (> (.length f) max-bytes))
      ;; Shift existing rotated files
      (doseq [i (range (dec backlog) 0 -1)]
        (let [src (io/file (str log-file "." i))
              dst (io/file (str log-file "." (inc i)))]
          (when (.exists src) (.renameTo src dst))))
      (.renameTo f (io/file (str log-file ".1"))))))

(defn configure-file-logging!
  "If config contains :logging with :file, add a Timbre file appender
  that persists logs at or above the specified :level."
  [{:keys [file level max-size backlog]
    :or   {level :warn max-size "10MB" backlog 5}}]
  (when file
    (io/make-parents file)
    (let [max-bytes (parse-size max-size)]
      (log/merge-config!
       {:appenders
        {:file
         {:enabled?  true
          :min-level level
          :fn        (fn [data]
                       (rotate-log! file max-bytes backlog)
                       (spit file
                             (str (force (:timestamp_ data)) " "
                                  (str/upper-case (name (:level data))) " "
                                  (:?ns-str data) " - "
                                  (force (:msg_ data)) "\n")
                             :append true))}}}))))

(defn configure-email-logging!
  "Add a Timbre email appender using Postal and the SMTP config from :notifications."
  [smtp-cfg {:keys [to level] :or {level :error}}]
  (when (and smtp-cfg to)
    (let [{:keys [host port tls user password from]} smtp-cfg
          conn {:host host
                :port (or port 587)
                :tls  (boolean tls)
                :user user
                :pass password}]
      (log/merge-config!
       {:appenders
        {:email
         {:enabled?   true
          :min-level  level
          :rate-limit [[5 (* 5 60 1000)]]  ;; max 5 emails per 5 min
          :fn         (fn [data]
                        (try
                          (let [level-str (str/upper-case (name (:level data)))
                                msg       (force (:msg_ data))]
                            (postal/send-message
                             conn
                             {:from    from
                              :to      [to]
                              :subject (str "[Bark] " level-str " — " (:?ns-str data))
                              :body    (str (force (:timestamp_ data)) " " level-str " "
                                            (:?ns-str data) " — " msg)}))
                          (catch Exception e
                            ;; Avoid recursive logging — print to stderr
                            (.println System/err
                                      (str "Failed to send log email: "
                                           (.getMessage e)))))}}}}))))

;; ---------------------------------------------------------------------------
;; Config
;; ---------------------------------------------------------------------------

(defn load-config
  "Load config.edn from the given path, or default to ./config.edn."
  ([] (load-config "config.edn"))
  ([path]
   (let [f (io/file path)]
     (if (.exists f)
       (do (log/info "Loading config from" (.getAbsolutePath f))
           (edn/read-string (slurp f)))
       (throw (ex-info (str "Config file not found: " path)
                       {:path path}))))))

;; ---------------------------------------------------------------------------
;; Shutdown coordination
;; ---------------------------------------------------------------------------

(def ^:private shutdown? (atom false))

(defn shutting-down? [] @shutdown?)

;; ---------------------------------------------------------------------------
;; Catch-up fetch
;; ---------------------------------------------------------------------------

(defn catch-up-fetch!
  "Fetch messages missed while the process was down.
  - First run (no watermark): fetch the last `initial-limit` messages.
  - Restart (watermark exists): fetch all messages with UID > watermark."
  [imap-conn db-conn folder initial-limit]
  (when-not (shutting-down?)
    (let [watermark (db/max-imap-uid db-conn)]
      (if (zero? watermark)
        (do (log/info "First run — fetching last" initial-limit "messages")
            (let [msgs (fetch/messages imap-conn folder
                                       {:limit        initial-limit
                                        :attachments? true})]
              (log/info "Fetched" (count msgs) "messages from IMAP")
              (when-not (shutting-down?)
                (ingest/store-emails! db-conn msgs)
                (when-let [max-uid (some->> msgs (keep :uid) seq (apply max))]
                  (db/save-imap-uid! db-conn max-uid)))))
        (do (log/info "Resuming — fetching UIDs >" watermark)
            (let [msgs (fetch/by-uid-range imap-conn folder
                                           (inc watermark) Long/MAX_VALUE)]
              (log/info "Fetched" (count msgs) "messages since watermark")
              (when (and (seq msgs) (not (shutting-down?)))
                (ingest/store-emails! db-conn msgs)
                (when-let [max-uid (some->> msgs (keep :uid) seq (apply max))]
                  (db/save-imap-uid! db-conn max-uid)))))))))

;; ---------------------------------------------------------------------------
;; IMAP connection
;; ---------------------------------------------------------------------------

(defn connect-imap
  "Connect to IMAP. Returns the connection, or nil on failure."
  [imap-cfg]
  (try
    (log/info "Connecting to IMAP" (:host imap-cfg) "as" (:user imap-cfg))
    (imap/connect (select-keys imap-cfg
                               [:host :port :ssl :user
                                :password :oauth2-token]))
    (catch Exception e
      (log/error "IMAP connection failed:" (.getMessage e))
      nil)))

;; ---------------------------------------------------------------------------
;; IDLE mode with reconnection
;; ---------------------------------------------------------------------------

(def ^:private max-backoff-ms (* 5 60 1000))  ;; cap at 5 minutes

(defn start-idle!
  "Start IMAP IDLE, storing each new message as it arrives."
  [imap-conn db-conn folder]
  (log/info "Starting IMAP IDLE on" folder)
  (idle/idle imap-conn folder
             (fn [msg]
               (when-not (shutting-down?)
                 (log/info "New message via IDLE — UID:" (:uid msg)
                           "Subject:" (:subject msg))
                 (try
                   (ingest/store-email! db-conn msg)
                   (when-let [uid (:uid msg)]
                     (let [current (db/max-imap-uid db-conn)]
                       (when (> uid current)
                         (db/save-imap-uid! db-conn uid))))
                   (catch Exception e
                     (log/error "Error storing message:" (.getMessage e))))))
             {:parse-opts   {:attachments? true}
              :heartbeat-ms (* 20 60 1000)}))

(defn idle-loop!
  "Run IDLE with automatic reconnection and exponential backoff.
  On each reconnect, performs a catch-up fetch to recover missed messages."
  [imap-cfg db-conn ingest-cfg]
  (let [folder (or (:folder imap-cfg) "INBOX")]
    (loop [backoff-ms 1000]
      (when-not (shutting-down?)
        (let [conn (connect-imap imap-cfg)]
          (if-not conn
            (do (log/error "IMAP connection failed, retrying in" (/ backoff-ms 1000) "s")
                (Thread/sleep backoff-ms)
                (recur (min (* backoff-ms 2) max-backoff-ms)))
            (do
              (try
                (log/info "IMAP connected, folder:" folder)
                (catch-up-fetch! conn db-conn folder
                                 (or (:initial-fetch ingest-cfg) 50))
                (when-not (shutting-down?)
                  (start-idle! conn db-conn folder))
                (catch Exception e
                  (log/error "IDLE interrupted:" (.getMessage e))))
              ;; If we get here, IDLE exited (heartbeat or server disconnect)
              (try (imap/disconnect conn) (catch Exception _))
              (when-not (shutting-down?)
                ;; Reset backoff — the connection was working
                (log/debug "IDLE exited, reconnecting in 1s")
                (Thread/sleep 1000)
                (recur 1000)))))))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn -main [& args]
  (let [config-path (or (first args) "config.edn")
        config      (load-config config-path)
        imap-cfg    (:imap config)]
    (when-let [logging (:logging config)]
      (configure-file-logging! logging)
      (when-let [email-cfg (:email logging)]
        (if-let [smtp (get-in config [:notifications :smtp])]
          (configure-email-logging! smtp email-cfg)
          (log/warn "Logging :email configured but no :notifications :smtp found."))))
    (when-not imap-cfg
      (log/error "No :imap key in config.edn.")
      (System/exit 1))
    (let [db-cfg     (:db config)
          ingest-cfg (:ingest config)
          db-conn    (db/connect (:path db-cfg))
          _          (log/info "Datalevin connected.")]
      (.addShutdownHook
       (Runtime/getRuntime)
       (Thread.
        (fn []
          (log/info "Shutting down...")
          (reset! shutdown? true)
          (Thread/sleep 1000)
          (try (db/close db-conn) (catch Exception _))
          (log/info "Goodbye."))))
      (idle-loop! imap-cfg db-conn ingest-cfg))))
