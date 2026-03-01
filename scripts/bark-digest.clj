#!/usr/bin/env bb

;; bark-digest.clj — Digest emails and create reports.
;;
;; BARK: Bug And Report Keeper
;;
;; Reads emails and writes reports to the same datalevin database.
;;
;; Usage:
;;   bb bark-digest.clj digest [--all]    — scan new emails (or all with --all)
;;   bb bark-digest.clj bugs              — list all bug reports
;;
;; Or via bb tasks:
;;   bb digest [--all]
;;   bb bugs
;;
;; Environment / defaults:
;;   BARK_DB — path to db (default: ./data/bark-db)

(require '[babashka.pods :as pods]
         '[clojure.string :as str])

(pods/load-pod 'huahaiy/datalevin "0.10.5")

(require '[pod.huahaiy.datalevin :as d])

;; ---------------------------------------------------------------------------
;; Schema for reports (merged into the existing email DB)
;; ---------------------------------------------------------------------------

(def report-schema
  {;; Report type: :bug, :patch, :request, :announcement, :release, :change
   :report/type        {:db/valueType :db.type/keyword}

   ;; Reference to the email that created this report
   :report/email       {:db/valueType :db.type/ref}

   ;; Message-ID of the originating email (for dedup)
   :report/message-id  {:db/valueType :db.type/string
                        :db/unique    :db.unique/identity}

   ;; Version extracted from subject, e.g. [BUG 9.7.1]
   :report/version     {:db/valueType :db.type/string}

   ;; States — each is a ref to the email that triggered the state change.
   ;; Presence means "on", absence means "off".
   ;; Retract to undo (e.g. un-ack, un-own).
   :report/acked       {:db/valueType :db.type/ref}
   :report/owned       {:db/valueType :db.type/ref}
   :report/closed      {:db/valueType :db.type/ref}
   :report/urgent      {:db/valueType :db.type/ref}
   :report/important   {:db/valueType :db.type/ref}

   ;; When this report was created by bark-digest
   :report/digested-at {:db/valueType :db.type/instant}

   ;; High-water mark: last UID processed by digest
   :digest/last-uid    {:db/valueType :db.type/long
                        :db/unique    :db.unique/identity}})

;; ---------------------------------------------------------------------------
;; Subject pattern matching
;; ---------------------------------------------------------------------------

(def bug-pattern
  #"(?i)^\[BUG(?:\s+([^\]]*))?\]")

(defn detect-bug [subject]
  (when-let [m (re-find bug-pattern subject)]
    {:type    :bug
     :version (when (second m) (str/trim (second m)))}))

(defn detect-report [subject]
  (detect-bug subject))

;; ---------------------------------------------------------------------------
;; Database operations
;; ---------------------------------------------------------------------------

(defn get-last-uid [db]
  (or (d/q '[:find ?uid .
             :where [_ :digest/last-uid ?uid]]
           db)
      0))

(defn save-last-uid! [conn uid]
  (d/transact! conn [{:digest/last-uid uid}]))

(defn emails-since [db since-uid]
  (d/q '[:find ?e ?uid ?subject ?mid
         :in $ ?since
         :where
         [?e :email/uid ?uid]
         [?e :email/subject ?subject]
         [?e :email/message-id ?mid]
         [(> ?uid ?since)]]
       db since-uid))

(defn all-emails [db]
  (d/q '[:find ?e ?uid ?subject ?mid
         :where
         [?e :email/uid ?uid]
         [?e :email/subject ?subject]
         [?e :email/message-id ?mid]]
       db))

(defn report-exists? [db message-id]
  (some? (d/q '[:find ?r .
                :in $ ?mid
                :where [?r :report/message-id ?mid]]
              db message-id)))

(defn create-report! [conn email-eid message-id report-info]
  (d/transact! conn
               [(cond-> {:report/type        (:type report-info)
                         :report/email       email-eid
                         :report/message-id  message-id
                         :report/digested-at (java.util.Date.)}
                  (:version report-info)
                  (assoc :report/version (:version report-info)))]))

(defn all-bug-reports [db]
  (->> (d/q '[:find (pull ?r [:db/id :report/version
                               :report/acked :report/owned :report/closed
                               :report/urgent :report/important
                               {:report/email [:email/subject :email/from-address
                                               :email/date-sent :email/uid]}])
              :where
              [?r :report/type :bug]]
            db)
       (map first)
       (sort-by #(get-in % [:report/email :email/date-sent])
                #(compare %2 %1))))

;; ---------------------------------------------------------------------------
;; Commands
;; ---------------------------------------------------------------------------

(defn cmd-digest!
  [conn process-all?]
  (let [db       (d/db conn)
        last-uid (get-last-uid db)
        emails   (if process-all?
                   (do (println "Processing ALL emails...")
                       (all-emails db))
                   (do (println (str "Processing emails since UID " last-uid "..."))
                       (emails-since db last-uid)))
        sorted   (sort-by second emails)]
    (println (str "Found " (count sorted) " email(s) to scan."))
    (let [created (atom 0)
          max-uid (atom last-uid)]
      (doseq [[eid uid subject message-id] sorted]
        (when (> uid @max-uid)
          (reset! max-uid uid))
        (when-let [report-info (detect-report subject)]
          (when-not (report-exists? (d/db conn) message-id)
            (println (str "  [" (name (:type report-info)) "] " subject))
            (create-report! conn eid message-id report-info)
            (swap! created inc))))
      (when (> @max-uid last-uid)
        (save-last-uid! conn @max-uid))
      (println (str "Created " @created " new report(s). Last UID: " @max-uid)))))

(defn cmd-bugs [conn]
  (let [bugs (all-bug-reports (d/db conn))]
    (if (empty? bugs)
      (println "No bug reports found.")
      (do
        (println (str (count bugs) " bug report(s):\n"))
        (doseq [report bugs]
          (let [email    (:report/email report)
                subject  (:email/subject email)
                from     (:email/from-address email)
                date     (str (:email/date-sent email))
                date-str (subs date 0 (min 19 (count date)))
                version  (:report/version report)
                flags    (str (when (:report/acked report) "A")
                              (when (:report/owned report) "O")
                              (when (:report/closed report) "C")
                              (when (:report/urgent report) "U")
                              (when (:report/important report) "I"))]
            (println (format "  %-5s %-25s %s  %s%s"
                             (if (empty? flags) "-----" flags)
                             from date-str subject
                             (if version (str " (" version ")") "")))))))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [args    *command-line-args*
      all?    (some #{"--all"} args)
      clean   (remove #{"--all"} args)
      ;; When called via bb tasks, command is passed as first arg
      command (or (first clean) "digest")
      db-path (or (System/getenv "BARK_DB") "data/bark-db")
      conn    (d/get-conn db-path report-schema)]
  (try
    (case command
      "digest" (cmd-digest! conn all?)
      "bugs"   (cmd-bugs conn)
      (do (println (str "Unknown command: " command))
          (println "Usage: bb bark-digest.clj [digest|bugs] [--all]")
          (println "  BARK_DB — db path (default: ./data/bark-db)")))
    (finally
      (d/close conn))))
