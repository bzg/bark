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
;;   bb bark-digest.clj requests          — list all requests
;;   bb bark-digest.clj announcements     — list all announcements
;;   bb bark-digest.clj releases          — list all releases
;;   bb bark-digest.clj changes           — list all changes
;;   bb bark-digest.clj reports           — list all reports
;;
;; Or via bb tasks:
;;   bb digest [--all]
;;   bb bugs / bb patches / bb requests / bb announcements / bb releases
;;   bb changes / bb reports
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

   ;; How the patch was detected: :subject, :attachment, :inline
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
(def patch-subject-pattern
  #"(?i)^\[PATCH(?:\s+([^\]]*))?\]")

(def patch-seq-pattern
  #"(\d+/\d+)\s*$")

;; [POLL] or [FR] or [FP] or [RFC] or [RFE] or [TASK]
(def request-pattern
  #"(?i)^\[(POLL|FR|FP|RFC|RFE|TASK)\]")

;; [ANN] or [ANNOUNCEMENT]
(def announcement-pattern
  #"(?i)^\[(ANN|ANNOUNCEMENT)\]")

;; [REL] or [RELEASE] optionally followed by version
(def release-pattern
  #"(?i)^\[(REL|RELEASE)(?:\s+([^\]]*))?\]")

;; [CHG] or [CHANGE] optionally followed by version
(def change-pattern
  #"(?i)^\[(CHG|CHANGE)(?:\s+([^\]]*))?\]")

;; --- Detectors (subject-only) ---

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

(defn detect-request [subject]
  (when (re-find request-pattern subject)
    {:type :request}))

(defn detect-announcement [subject]
  (when (re-find announcement-pattern subject)
    {:type :announcement}))

(defn detect-release [subject]
  (when-let [m (re-find release-pattern subject)]
    (let [ver (nth m 2 nil)]
      (cond-> {:type :release}
        (and ver (not (str/blank? ver)))
        (assoc :version (str/trim ver))))))

(defn detect-change [subject]
  (when-let [m (re-find change-pattern subject)]
    (let [ver (nth m 2 nil)]
      (cond-> {:type :change}
        (and ver (not (str/blank? ver)))
        (assoc :version (str/trim ver))))))

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
  Takes the full email pull map.
  Priority order: bug > patch > request > announcement > release > change."
  [email]
  (let [subject     (:email/subject email)
        attachments (:email/attachments email)
        body-text   (or (:email/body-text email)
                        (:email/body-text-from-html email))]
    ;; Subject-only detectors, in priority order
    (or (detect-bug subject)
        ;; Patch: subject OR attachment OR inline (or combination)
        (let [from-subject    (detect-patch-subject subject)
              from-attachment (when (has-patch-attachment? attachments) :attachment)
              from-inline     (when (has-inline-patch? body-text) :inline)
              sources         (cond-> #{}
                                from-subject    (into (:patch-source from-subject))
                                from-attachment (conj :attachment)
                                from-inline     (conj :inline))]
          (when (seq sources)
            (cond-> {:type         :patch
                     :patch-source sources}
              (:patch-seq from-subject) (assoc :patch-seq (:patch-seq from-subject))
              (:topic from-subject)     (assoc :topic (:topic from-subject)))))
        (detect-request subject)
        (detect-announcement subject)
        (detect-release subject)
        (detect-change subject))))

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

(def email-pull-pattern
  '[:db/id :email/uid :email/subject :email/message-id
    :email/body-text :email/body-text-from-html
    {:email/attachments [:attachment/filename
                         :attachment/content-type]}])

(defn emails-since
  "Return full email entities with UID > since-uid."
  [db since-uid]
  (->> (d/q (list :find (list 'pull '?e email-pull-pattern)
                  :in '$ '?since
                  :where
                  ['?e :email/uid '?uid]
                  '[(> ?uid ?since)])
            db since-uid)
       (map first)))

(defn all-emails
  "Return all email entities."
  [db]
  (->> (d/q (list :find (list 'pull '?e email-pull-pattern)
                  :where
                  ['?e :email/uid '_])
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

(def report-pull-pattern
  '[:db/id :report/type :report/version :report/topic
    :report/patch-seq :report/patch-source
    :report/acked :report/owned :report/closed
    :report/urgent :report/important
    {:report/email [:email/subject :email/from-address
                    :email/date-sent :email/uid]}])

(defn all-reports-by-type
  "Return all reports of a given type, with email details, sorted by date desc."
  [db report-type]
  (->> (d/q (list :find (list 'pull '?r report-pull-pattern)
                  :in '$ '?type
                  :where
                  ['?r :report/type '?type])
            db report-type)
       (map first)
       (sort-by #(get-in % [:report/email :email/date-sent])
                #(compare %2 %1))))

(defn all-reports
  "Return all reports, with email details, sorted by date desc."
  [db]
  (->> (d/q (list :find (list 'pull '?r report-pull-pattern)
                  :where
                  ['?r :report/type '_])
            db)
       (map first)
       (sort-by #(get-in % [:report/email :email/date-sent])
                #(compare %2 %1))))

;; ---------------------------------------------------------------------------
;; Display helpers
;; ---------------------------------------------------------------------------

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

(defn- format-extra [report]
  (let [version (:report/version report)
        topic   (:report/topic report)
        seq-str (:report/patch-seq report)
        sources (:report/patch-source report)
        parts   (remove nil?
                        [(when version (str "(" version ")"))
                         (when topic (str "[" topic "]"))
                         (when seq-str (str "(" seq-str ")"))
                         (when sources
                           (str "{" (str/join "," (map name sources)) "}"))])]
    (when (seq parts)
      (str " " (str/join " " parts)))))

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
                            (:email/subject email)))
              (create-report! conn eid message-id report-info)
              (swap! created inc)))))
      (when (> @max-uid last-uid)
        (save-last-uid! conn @max-uid))
      (println (str "Created " @created " new report(s). Last UID: " @max-uid)))))

(defn cmd-list
  "List reports, optionally filtered by type."
  [conn report-type]
  (let [db      (d/db conn)
        reports (if report-type
                  (all-reports-by-type db report-type)
                  (all-reports db))
        label   (if report-type (name report-type) "report")]
    (if (empty? reports)
      (println (str "No " label "s found."))
      (do
        (println (str (count reports) " " label "(s):\n"))
        (doseq [report reports]
          (let [type-tag (when-not report-type
                           (str "[" (name (:report/type report)) "] "))]
            (println (str (format-report-line report)
                          (when type-tag (str " " type-tag))
                          (format-extra report)))))))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [args    *command-line-args*
      all?    (some #{"--all"} args)
      clean   (remove #{"--all"} args)
      command (or (first clean) "digest")
      db-path (or (System/getenv "BARK_DB") "data/bark-db")
      conn    (d/get-conn db-path report-schema)]
  (try
    (case command
      "digest"        (cmd-digest! conn all?)
      "bugs"          (cmd-list conn :bug)
      "patches"       (cmd-list conn :patch)
      "requests"      (cmd-list conn :request)
      "announcements" (cmd-list conn :announcement)
      "releases"      (cmd-list conn :release)
      "changes"       (cmd-list conn :change)
      "reports"       (cmd-list conn nil)
      (do (println (str "Unknown command: " command))
          (println "Usage: bb bark-digest.clj <command> [--all]")
          (println "")
          (println "Commands:")
          (println "  digest        Scan new emails and create reports")
          (println "  bugs          List bug reports")
          (println "  patches       List patch reports")
          (println "  requests      List requests")
          (println "  announcements List announcements")
          (println "  releases      List releases")
          (println "  changes       List changes")
          (println "  reports       List all reports")
          (println "")
          (println "Options:")
          (println "  --all         Rescan all emails (with digest)")
          (println "")
          (println "Environment:")
          (println "  BARK_DB       db path (default: ./data/bark-db)")))
    (finally
      (d/close conn))))
