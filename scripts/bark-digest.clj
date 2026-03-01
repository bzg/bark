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
;;   bb bark-digest.clj patches           — list all patch reports
;;
;; Or via bb tasks:
;;   bb digest [--all]
;;   bb bugs
;;   bb patches
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

   ;; Topic extracted from subject, e.g. [PATCH org-agenda: fix sorting]
   :report/topic       {:db/valueType :db.type/string}

   ;; Patch sequence, e.g. "3/5" from [PATCH 3/5]
   :report/patch-seq   {:db/valueType :db.type/string}

   ;; How the patch was detected: :subject, :attachment, :inline, or combination
   :report/patch-source {:db/valueType :db.type/keyword
                         :db/cardinality :db.cardinality/many}

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

;; [BUG] or [BUG version]
(def bug-pattern
  #"(?i)^\[BUG(?:\s+([^\]]*))?\]")

;; [PATCH] or [PATCH n/m] or [PATCH topic] or [PATCH topic n/m]
;; The n/m sequence pattern is matched greedily at the end.
(def patch-subject-pattern
  #"(?i)^\[PATCH(?:\s+([^\]]*))?\]")

(def patch-seq-pattern
  #"(\d+/\d+)\s*$")

(defn detect-bug [subject]
  (when-let [m (re-find bug-pattern subject)]
    {:type    :bug
     :version (when (second m) (str/trim (second m)))}))

(defn detect-patch-subject
  "Detect patch from subject line. Returns {:type :patch ...} or nil."
  [subject]
  (when-let [m (re-find patch-subject-pattern subject)]
    (let [inner   (when (second m) (str/trim (second m)))
          seq-m   (when inner (re-find patch-seq-pattern inner))
          seq-str (when seq-m (first seq-m))
          topic   (when inner
                    (let [t (if seq-str
                              (str/trim (subs inner 0 (- (count inner) (count seq-str))))
                              inner)]
                      (when-not (str/blank? t) t)))]
      (cond-> {:type         :patch
               :patch-source #{:subject}}
        seq-str (assoc :patch-seq seq-str)
        topic   (assoc :topic topic)))))

;; ---------------------------------------------------------------------------
;; Attachment & inline patch detection
;; ---------------------------------------------------------------------------

(def patch-filename-pattern
  "Filenames ending in .patch or .diff"
  #"(?i)\.(patch|diff)$")

(defn has-patch-attachment?
  "Check if any attachment looks like a patch file."
  [attachments]
  (some (fn [att]
          (let [fname (:attachment/filename att)]
            (when fname
              (re-find patch-filename-pattern fname))))
        attachments))

(def inline-patch-indicators
  "Patterns that suggest an inline patch in the body text."
  [#"(?m)^diff --git "
   #"(?m)^--- a/"
   #"(?m)^\+\+\+ b/"
   #"(?m)^@@ [-+]\d+"
   #"(?m)^index [0-9a-f]+\.\.[0-9a-f]+"])

(defn has-inline-patch?
  "Check if the body text contains inline patch content.
  Requires at least 2 indicators to avoid false positives."
  [body-text]
  (when body-text
    (let [hits (count (filter #(re-find % body-text) inline-patch-indicators))]
      (>= hits 2))))

;; ---------------------------------------------------------------------------
;; Combined report detection
;; ---------------------------------------------------------------------------

(defn detect-report
  "Detect report type from email data. Returns report-info map or nil.
  Takes the full email pull map."
  [email]
  (let [subject     (:email/subject email)
        attachments (:email/attachments email)
        body-text   (or (:email/body-text email)
                        (:email/body-text-from-html email))]
    ;; Bug takes precedence (subject-only)
    (or (detect-bug subject)
        ;; Patch: subject OR attachment OR inline
        (let [from-subject    (detect-patch-subject subject)
              from-attachment (when (has-patch-attachment? attachments) :attachment)
              from-inline    (when (has-inline-patch? body-text) :inline)
              sources        (cond-> #{}
                               from-subject    (into (:patch-source from-subject))
                               from-attachment (conj :attachment)
                               from-inline    (conj :inline))]
          (when (seq sources)
            (cond-> {:type         :patch
                     :patch-source sources}
              (:patch-seq from-subject) (assoc :patch-seq (:patch-seq from-subject))
              (:topic from-subject)     (assoc :topic (:topic from-subject))))))))

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

(defn emails-since
  "Return full email entities with UID > since-uid."
  [db since-uid]
  (->> (d/q '[:find (pull ?e [:db/id :email/uid :email/subject :email/message-id
                               :email/body-text :email/body-text-from-html
                               {:email/attachments [:attachment/filename
                                                    :attachment/content-type]}])
              :in $ ?since
              :where
              [?e :email/uid ?uid]
              [(> ?uid ?since)]]
            db since-uid)
       (map first)))

(defn all-emails
  "Return all email entities."
  [db]
  (->> (d/q '[:find (pull ?e [:db/id :email/uid :email/subject :email/message-id
                               :email/body-text :email/body-text-from-html
                               {:email/attachments [:attachment/filename
                                                    :attachment/content-type]}])
              :where
              [?e :email/uid _]]
            db)
       (map first)))

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
                  (assoc :report/version (:version report-info))
                  (:topic report-info)
                  (assoc :report/topic (:topic report-info))
                  (:patch-seq report-info)
                  (assoc :report/patch-seq (:patch-seq report-info))
                  (:patch-source report-info)
                  (assoc :report/patch-source (:patch-source report-info)))]))

(defn all-reports-by-type
  "Return all reports of a given type, with email details, sorted by date desc."
  [db report-type]
  (->> (d/q '[:find (pull ?r [:db/id :report/version :report/topic
                               :report/patch-seq :report/patch-source
                               :report/acked :report/owned :report/closed
                               :report/urgent :report/important
                               {:report/email [:email/subject :email/from-address
                                               :email/date-sent :email/uid]}])
              :in $ ?type
              :where
              [?r :report/type ?type]]
            db report-type)
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
        sorted   (sort-by :email/uid emails)]
    (println (str "Found " (count sorted) " email(s) to scan."))
    (let [created (atom 0)
          max-uid (atom last-uid)]
      (doseq [email sorted]
        (let [uid        (:email/uid email)
              message-id (:email/message-id email)
              eid        (:db/id email)]
          (when (> uid @max-uid)
            (reset! max-uid uid))
          (when-let [report-info (detect-report email)]
            (when-not (report-exists? (d/db conn) message-id)
              (println (str "  [" (name (:type report-info)) "] "
                            (:email/subject email)
                            (when-let [src (:patch-source report-info)]
                              (str " {" (str/join "," (map name src)) "}"))))
              (create-report! conn eid message-id report-info)
              (swap! created inc)))))
      (when (> @max-uid last-uid)
        (save-last-uid! conn @max-uid))
      (println (str "Created " @created " new report(s). Last UID: " @max-uid)))))

(defn- format-flags [report]
  (let [flags (str (when (:report/acked report) "A")
                   (when (:report/owned report) "O")
                   (when (:report/closed report) "C")
                   (when (:report/urgent report) "U")
                   (when (:report/important report) "I"))]
    (if (empty? flags) "-----" flags)))

(defn- format-report-line [report]
  (let [email    (:report/email report)
        subject  (:email/subject email)
        from     (:email/from-address email)
        date     (str (:email/date-sent email))
        date-str (subs date 0 (min 19 (count date)))]
    (format "  %-5s %-25s %s  %s"
            (format-flags report) from date-str subject)))

(defn cmd-bugs [conn]
  (let [bugs (all-reports-by-type (d/db conn) :bug)]
    (if (empty? bugs)
      (println "No bug reports found.")
      (do
        (println (str (count bugs) " bug report(s):\n"))
        (doseq [report bugs]
          (let [version (:report/version report)]
            (println (str (format-report-line report)
                          (when version (str " (" version ")"))))))))))

(defn cmd-patches [conn]
  (let [patches (all-reports-by-type (d/db conn) :patch)]
    (if (empty? patches)
      (println "No patch reports found.")
      (do
        (println (str (count patches) " patch report(s):\n"))
        (doseq [report patches]
          (let [topic   (:report/topic report)
                seq-str (:report/patch-seq report)
                sources (:report/patch-source report)
                extra   (str/join " "
                                  (remove nil?
                                          [(when topic (str "[" topic "]"))
                                           (when seq-str (str "(" seq-str ")"))
                                           (when sources
                                             (str "{" (str/join ","
                                                       (map name sources)) "}"))]))]
            (println (str (format-report-line report)
                          (when-not (str/blank? extra)
                            (str " " extra))))))))))

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
      "digest"  (cmd-digest! conn all?)
      "bugs"    (cmd-bugs conn)
      "patches" (cmd-patches conn)
      (do (println (str "Unknown command: " command))
          (println "Usage: bb bark-digest.clj [digest|bugs|patches] [--all]")
          (println "  BARK_DB — db path (default: ./data/bark-db)")))
    (finally
      (d/close conn))))
