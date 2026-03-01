;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark-ingest.main
  "Entry point for bark-ingest. Connects to IMAP, watches for new emails,
  and stores them in a Datalevin database."
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
;; Initial fetch
;; ---------------------------------------------------------------------------

(defn initial-fetch!
  "Fetch recent messages and store any that aren't already in the database."
  [imap-conn db-conn folder-name limit]
  (log/info "Initial fetch: up to" limit "messages from" folder-name)
  (let [msgs (fetch/messages imap-conn folder-name
                             {:limit        limit
                              :attachments? true})]
    (log/info "Fetched" (count msgs) "messages from IMAP")
    (ingest/store-emails! db-conn msgs)))

;; ---------------------------------------------------------------------------
;; IDLE mode
;; ---------------------------------------------------------------------------

(defn start-idle!
  "Start IMAP IDLE, storing each new message as it arrives."
  [imap-conn db-conn folder-name]
  (log/info "Starting IMAP IDLE on" folder-name)
  (idle/idle imap-conn folder-name
             (fn [msg]
               (log/info "New message via IDLE — UID:" (:uid msg)
                         "Subject:" (:subject msg))
               (try
                 (ingest/store-email! db-conn msg)
                 (catch Exception e
                   (log/error "Error storing message:" (.getMessage e)))))
             {:parse-opts   {:attachments? true}
              :heartbeat-ms (* 20 60 1000)}))

;; ---------------------------------------------------------------------------
;; Shutdown coordination
;; ---------------------------------------------------------------------------

(def ^:private shutdown? (volatile! false))

(defn shutting-down? [] @shutdown?)

;; ---------------------------------------------------------------------------
;; Poll mode
;; ---------------------------------------------------------------------------

(defn start-poll!
  "Poll for new messages at a regular interval."
  [imap-conn db-conn folder-name interval-secs]
  (log/info "Starting poll mode on" folder-name
            "every" interval-secs "seconds")
  (loop []
    (when-not (or (Thread/interrupted) (shutting-down?))
      (try
        (let [max-uid (db/max-uid db-conn)
              _       (log/debug "Polling — max stored UID:" max-uid)
              msgs    (fetch/messages imap-conn folder-name
                                     {:limit        20
                                      :attachments? true})]
          ;; Only store messages with UIDs greater than what we already have
          (let [new-msgs (filter #(and (:uid %) (> (:uid %) max-uid)) msgs)]
            (when (seq new-msgs)
              (log/info "Found" (count new-msgs) "new messages")
              (ingest/store-emails! db-conn new-msgs))))
        (catch Exception e
          (log/error "Poll error:" (.getMessage e))))
      (Thread/sleep (* interval-secs 1000))
      (recur))))

;; ---------------------------------------------------------------------------
;; Shutdown hook
;; ---------------------------------------------------------------------------

(defn add-shutdown-hook! [imap-conn db-conn]
  (.addShutdownHook
   (Runtime/getRuntime)
   (Thread.
    (fn []
      (log/info "Shutting down...")
      (vreset! shutdown? true)
      ;; Give IDLE/poll loops time to exit cleanly
      (Thread/sleep 2000)
      (try (imap/disconnect imap-conn) (catch Exception _))
      (try (db/close db-conn) (catch Exception _))
      (log/info "Goodbye.")))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn -main [& args]
  (let [config-path (or (first args) "config.edn")
        config      (load-config config-path)
        imap-cfg    (:imap config)
        db-cfg      (:db config)
        ingest-cfg  (:ingest config)
        folder      (or (:folder imap-cfg) "INBOX")
        mode        (or (:mode ingest-cfg) :idle)]

    ;; Connect to Datalevin
    (let [db-conn   (db/connect (:path db-cfg))
          _         (log/info "Datalevin connected. Max stored UID:" (db/max-uid db-conn))

          ;; Connect to IMAP
          imap-conn (do (log/info "Connecting to IMAP" (:host imap-cfg) "as" (:user imap-cfg))
                        (imap/connect (select-keys imap-cfg [:host :port :ssl :user :password :oauth2-token])))]

      (log/info "IMAP connected:" (imap/connected? imap-conn))

      ;; Register shutdown hook
      (add-shutdown-hook! imap-conn db-conn)

      ;; Initial fetch
      (initial-fetch! imap-conn db-conn folder
                      (or (:initial-fetch ingest-cfg) 50))

      ;; Start watching
      (case mode
        :idle (start-idle! imap-conn db-conn folder)
        :poll (start-poll! imap-conn db-conn folder
                           (or (:poll-interval ingest-cfg) 60))
        (do (log/error "Unknown ingest mode:" mode)
            (System/exit 1))))))
