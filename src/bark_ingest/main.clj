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
            [clojure.tools.logging :as log])
  (:gen-class))

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
              ;; If we get here, IDLE exited (error or server disconnect)
              (try (imap/disconnect conn) (catch Exception _))
              (when-not (shutting-down?)
                ;; Reset backoff — the connection was working
                (log/warn "IDLE exited, reconnecting in 1s")
                (Thread/sleep 1000)
                (recur 1000)))))))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn -main [& args]
  (let [config-path (or (first args) "config.edn")
        config      (load-config config-path)
        imap-cfg    (:imap config)]
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
