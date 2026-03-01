#!/usr/bin/env bb

;; bark-digest.clj — Digest emails and create reports.
;;
;; BARK: Bug And Report Keeper
;;
;; Reads emails and writes reports to the same datalevin database.
;; For each email, either:
;;   1. It triggers a new report
;;   2. It is a descendant of an existing report (added to :report/descendants)
;;   3. It is unrelated
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

   ;; Emails in the thread below this report (direct and indirect replies)
   :report/descendants {:db/valueType   :db.type/ref
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

;; --- Detectors ---

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

(defn- detect-versioned-tag
  "Detect a subject tag with an optional version. Returns {:type type} or nil."
  [pattern type subject]
  (when-let [m (re-find pattern subject)]
    (let [ver (nth m 2 nil)]
      (cond-> {:type type}
        (and ver (not (str/blank? ver)))
        (assoc :version (str/trim ver))))))

(defn detect-release [subject]
  (detect-versioned-tag release-pattern :release subject))

(defn detect-change [subject]
  (detect-versioned-tag change-pattern :change subject))

;; ---------------------------------------------------------------------------
;; Attachment & inline patch detection
;; ---------------------------------------------------------------------------

(def patch-filename-pattern
  #"(?i)\.(patch|diff)$")

(defn has-patch-attachment? [attachments]
  (some (fn [att]
          (when-let [fname (:attachment/filename att)]
            (re-find patch-filename-pattern fname)))
        attachments))

(def inline-patch-indicators
  [#"(?m)^diff --git "
   #"(?m)^--- a/"
   #"(?m)^\+\+\+ b/"
   #"(?m)^@@ [-+]\d+"
   #"(?m)^index [0-9a-f]+\.\.[0-9a-f]+"])

(defn has-inline-patch? [body-text]
  (when body-text
    (>= (count (filter #(re-find % body-text) inline-patch-indicators)) 2)))

;; ---------------------------------------------------------------------------
;; Combined patch detection
;; ---------------------------------------------------------------------------

(defn detect-patch
  "Detect patch from subject, attachments, and/or inline content.
  Returns {:type :patch :patch-source #{...} ...} or nil."
  [subject attachments body-text]
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
        (:topic from-subject)     (assoc :topic (:topic from-subject))))))

;; ---------------------------------------------------------------------------
;; Combined report detection
;; ---------------------------------------------------------------------------

(defn detect-report
  "Detect report type from email data. Returns report-info map or nil.
  Priority order: bug > patch > request > announcement > release > change."
  [email]
  (when-let [subject (:email/subject email)]
    (let [attachments (:email/attachments email)
          body-text   (or (:email/body-text email)
                          (:email/body-text-from-html email))]
      (or (detect-bug subject)
          (detect-patch subject attachments body-text)
          (detect-request subject)
          (detect-announcement subject)
          (detect-release subject)
          (detect-change subject)))))

;; ---------------------------------------------------------------------------
;; Threading: ancestor message-ids from an email
;; ---------------------------------------------------------------------------

(defn ancestor-mids
  "Return the set of message-ids this email references as ancestors.
  Combines In-Reply-To and all References entries."
  [email]
  (let [irt  (:email/in-reply-to email)
        refs (:email/references email)]
    (cond-> #{}
      irt       (conj irt)
      (coll? refs) (into refs))))

;; ---------------------------------------------------------------------------
;; Thread index: message-id → report entity id
;;
;; Maps every message-id that belongs to a report (either the originating
;; email or a descendant) to the report's entity id.
;; ---------------------------------------------------------------------------

(defn build-thread-index
  "Build the initial thread index from existing reports and their descendants."
  [db]
  (let [reports     (d/q '[:find ?rid ?mid
                            :where [?rid :report/message-id ?mid]]
                         db)
        descendants (d/q '[:find ?rid ?dmid
                            :where
                            [?rid :report/descendants ?de]
                            [?de :email/message-id ?dmid]]
                         db)]
    (into (into {} (map (fn [[rid mid]] [mid rid])) reports)
          (map (fn [[rid dmid]] [dmid rid]))
          descendants)))

(defn find-report-for-email
  "Given an email and the thread index, return the report entity id
  this email is a descendant of, or nil."
  [email thread-index]
  (some thread-index (ancestor-mids email)))

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
    :email/in-reply-to :email/references
    :email/date-sent
    :email/body-text :email/body-text-from-html
    {:email/attachments [:attachment/filename
                         :attachment/content-type]}])

(defn emails-since [db since-uid]
  (->> (d/q (list :find (list 'pull '?e email-pull-pattern)
                  :in '$ '?since
                  :where
                  ['?e :email/uid '?uid]
                  '[(> ?uid ?since)])
            db since-uid)
       (map first)))

(defn all-emails [db]
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
               [(into {:report/type        (:type report-info)
                       :report/email       email-eid
                       :report/message-id  message-id
                       :report/digested-at (java.util.Date.)}
                      (filter val)
                      {:report/version      (:version report-info)
                       :report/topic        (:topic report-info)
                       :report/patch-seq    (:patch-seq report-info)
                       :report/patch-source (:patch-source report-info)})]))

(defn add-descendant! [conn report-eid email-eid]
  (d/transact! conn [[:db/add report-eid :report/descendants email-eid]]))

(def report-pull-pattern
  '[:db/id :report/type :report/version :report/topic
    :report/patch-seq :report/patch-source
    :report/acked :report/owned :report/closed
    :report/urgent :report/important
    :report/descendants
    {:report/email [:email/subject :email/from-address
                    :email/date-sent :email/uid]}])

(defn all-reports-by-type [db report-type]
  (->> (d/q (list :find (list 'pull '?r report-pull-pattern)
                  :in '$ '?type
                  :where
                  ['?r :report/type '?type])
            db report-type)
       (map first)
       (sort-by #(get-in % [:report/email :email/date-sent])
                #(compare %2 %1))))

(defn all-reports [db]
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

(defn- descendant-count [report]
  (let [d (:report/descendants report)]
    (if (coll? d) (count d) 0)))

(defn- format-report-line [report]
  (let [email    (:report/email report)
        date     (str (or (:email/date-sent email) ""))
        date-str (if (>= (count date) 19) (subs date 0 19) date)]
    (format "  %-5s %3d %-25s %s  %s"
            (format-flags report)
            (descendant-count report)
            (or (:email/from-address email) "?")
            date-str
            (or (:email/subject email) "(no subject)"))))

(defn- format-extra [report]
  (let [parts (remove nil?
                      [(when-let [v (:report/version report)]   (str "(" v ")"))
                       (when-let [t (:report/topic report)]     (str "[" t "]"))
                       (when-let [s (:report/patch-seq report)] (str "(" s ")"))
                       (when-let [sources (:report/patch-source report)]
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
        sorted   (sort-by :email/uid emails)
        init-idx (build-thread-index db)
        {:keys [created threaded max-uid]}
        (reduce
         (fn [{:keys [created threaded max-uid thread-index] :as acc} email]
           (let [uid        (:email/uid email)
                 message-id (:email/message-id email)
                 eid        (:db/id email)
                 max-uid    (max max-uid uid)
                 ;; 1. Does it trigger a new report?
                 report-info (detect-report email)
                 new-report? (and report-info
                                  (not (report-exists? (d/db conn) message-id)))
                 ;; Create report and update index
                 [created thread-index]
                 (if new-report?
                   (do (println (str "  [" (name (:type report-info)) "] "
                                     (:email/subject email)))
                       (create-report! conn eid message-id report-info)
                       (let [report-eid (d/q '[:find ?r .
                                               :in $ ?mid
                                               :where [?r :report/message-id ?mid]]
                                             (d/db conn) message-id)]
                         [(inc created)
                          (assoc thread-index message-id report-eid)]))
                   [created thread-index])
                 ;; 2. Is it a descendant of an existing report?
                 parent-report-eid (find-report-for-email email thread-index)
                 [threaded thread-index]
                 (if parent-report-eid
                   (do (add-descendant! conn parent-report-eid eid)
                       [(inc threaded)
                        (assoc thread-index message-id parent-report-eid)])
                   [threaded thread-index])]
             {:created created :threaded threaded
              :max-uid max-uid :thread-index thread-index}))
         {:created 0 :threaded 0 :max-uid last-uid :thread-index init-idx}
         sorted)]
    (println (str "Found " (count sorted) " email(s). "
                  "Thread index: " (count init-idx) " entries."))
    (when (> max-uid last-uid)
      (save-last-uid! conn max-uid))
    (println (str "Created " created " report(s), "
                  "threaded " threaded " email(s). "
                  "Last UID: " max-uid))))

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
