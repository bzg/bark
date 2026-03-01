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
;; Shutdown coordination
;; ---------------------------------------------------------------------------

(def ^:private shutdown? (volatile! false))

(defn shutting-down? [] @shutdown?)

;; ---------------------------------------------------------------------------
;; Initial fetch
;; ---------------------------------------------------------------------------

(defn initial-fetch!
  "Fetch recent messages and store any that aren't already in the database."
  [imap-conn db-conn mailbox-id folder-name limit]
  (when-not (shutting-down?)
    (log/info "Initial fetch: up to" limit "messages from" folder-name
              "(" mailbox-id ")")
    (let [msgs (fetch/messages imap-conn folder-name
                               {:limit        limit
                                :attachments? true})]
      (log/info "Fetched" (count msgs) "messages from IMAP (" mailbox-id ")")
      (when-not (shutting-down?)
        (ingest/store-emails! db-conn mailbox-id msgs)
        ;; Update per-mailbox watermark
        (when-let [max-uid (some->> msgs (keep :uid) seq (apply max))]
          (let [current (db/max-imap-uid db-conn mailbox-id)]
            (when (> max-uid current)
              (db/save-imap-uid! db-conn mailbox-id max-uid))))))))

;; ---------------------------------------------------------------------------
;; IDLE mode
;; ---------------------------------------------------------------------------

(defn start-idle!
  "Start IMAP IDLE, storing each new message as it arrives."
  [imap-conn db-conn mailbox-id folder-name]
  (log/info "Starting IMAP IDLE on" folder-name "(" mailbox-id ")")
  (idle/idle imap-conn folder-name
             (fn [msg]
               (when-not (shutting-down?)
                 (log/info "New message via IDLE — UID:" (:uid msg)
                           "Subject:" (:subject msg))
                 (try
                   (ingest/store-email! db-conn mailbox-id msg)
                   ;; Update watermark
                   (when-let [uid (:uid msg)]
                     (let [current (db/max-imap-uid db-conn mailbox-id)]
                       (when (> uid current)
                         (db/save-imap-uid! db-conn mailbox-id uid))))
                   (catch Exception e
                     (log/error "Error storing message:" (.getMessage e))))))
             {:parse-opts   {:attachments? true}
              :heartbeat-ms (* 20 60 1000)}))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn- watch-mailbox!
  "Connect to one IMAP mailbox, do initial fetch, and start IDLE.
  Registers the IMAP connection in imap-conns atom before blocking."
  [imap-cfg db-conn ingest-cfg imap-conns]
  (let [mailbox-id (db/mailbox-id imap-cfg)
        folder     (or (:folder imap-cfg) "INBOX")]
    (log/info "Connecting to IMAP" (:host imap-cfg) "as" (:user imap-cfg)
              "(" mailbox-id ")")
    (let [imap-conn (imap/connect
                     (select-keys imap-cfg
                                  [:host :port :ssl :user :password :oauth2-token]))]
      (log/info "IMAP connected:" (imap/connected? imap-conn)
                "folder:" folder)
      ;; Register connection NOW so shutdown hook can disconnect it
      (swap! imap-conns conj imap-conn)
      (initial-fetch! imap-conn db-conn mailbox-id folder
                      (or (:initial-fetch ingest-cfg) 50))
      (when-not (shutting-down?)
        (log/info "Entering IDLE for" mailbox-id)
        (start-idle! imap-conn db-conn mailbox-id folder)))))

(defn -main [& args]
  (let [config-path (or (first args) "config.edn")
        config      (load-config config-path)
        mailboxes   (:mailboxes config)
        db-cfg      (:db config)
        ingest-cfg  (:ingest config)]

    (when (empty? mailboxes)
      (log/error "No mailboxes configured. Add :mailboxes to config.edn.")
      (System/exit 1))

    (let [db-conn    (db/connect (:path db-cfg))
          _          (log/info "Datalevin connected.")
          imap-conns (atom [])]

      ;; Shutdown hook:
      ;; 1. Set flag so callbacks stop writing to DB
      ;; 2. Disconnect IMAP — this breaks the blocking IDLE calls
      ;; 3. Wait briefly for threads to exit
      ;; 4. Close DB
      (.addShutdownHook
       (Runtime/getRuntime)
       (Thread.
        (fn []
          (log/info "Shutting down...")
          (vreset! shutdown? true)
          (doseq [ic @imap-conns]
            (try (imap/disconnect ic) (catch Exception _)))
          ;; Give IDLE threads time to return after disconnect
          (Thread/sleep 1000)
          (try (db/close db-conn) (catch Exception _))
          (log/info "Goodbye."))))

      (if (= (count mailboxes) 1)
        ;; Single mailbox: run in main thread
        (watch-mailbox! (first mailboxes) db-conn ingest-cfg imap-conns)
        ;; Multiple mailboxes: each in its own thread
        (let [threads (mapv
                       (fn [mb]
                         (let [t (Thread.
                                  (fn []
                                    (try
                                      (watch-mailbox! mb db-conn ingest-cfg imap-conns)
                                      (catch Exception e
                                        (when-not (shutting-down?)
                                          (log/error "Mailbox" (db/mailbox-id mb) "failed:"
                                                     (.getMessage e)))))))]
                           (.setDaemon t false)
                           (.start t)
                           t))
                       mailboxes)]
          (doseq [t threads]
            (.join t)))))))
