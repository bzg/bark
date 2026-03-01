#!/usr/bin/env bb

;; bark-digest.clj — Digest emails and create reports.
;;
;; BARK: Bug And Report Keeper
;;
;; Reads emails and writes reports to the same datalevin database.
;; For each email, either:
;;   1. It triggers a new report (respecting permissions)
;;   2. It is a descendant of an existing report (added to :report/descendants)
;;   3. It contains admin/maintainer commands (role management)
;;   4. It is unrelated or from an ignored address
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
;;   bb bark-digest.clj roles             — show admins, maintainers, ignored
;;
;; Or via bb tasks:
;;   bb digest [--all]
;;   bb bugs / bb patches / bb requests / bb announcements / bb releases
;;   bb changes / bb reports / bb roles
;;
;; Environment / defaults:
;;   BARK_DB    — path to db (default: ./data/bark-db)
;;   BARK_ADMIN — default admin email address

(require '[babashka.pods :as pods]
         '[clojure.string :as str]
         '[clojure.edn :as edn])

(pods/load-pod 'huahaiy/datalevin "0.10.5")

(require '[pod.huahaiy.datalevin :as d])

;; ---------------------------------------------------------------------------
;; Schema for reports and roles (merged into the existing email DB)
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
   :report/acked       {:db/valueType :db.type/ref}
   :report/owned       {:db/valueType :db.type/ref}
   :report/closed      {:db/valueType :db.type/ref}
   :report/urgent      {:db/valueType :db.type/ref}
   :report/important   {:db/valueType :db.type/ref}

   ;; When this report was created by bark-digest
   :report/digested-at {:db/valueType :db.type/instant}

   ;; High-water mark: singleton with stable identity key
   :digest/id           {:db/valueType :db.type/string
                         :db/unique    :db.unique/identity}
   :digest/last-run     {:db/valueType :db.type/instant}

   ;; --- Per-mailbox roles ---
   ;; One entity per mailbox, keyed by the mailbox's :email from config
   :roles/mailbox-email {:db/valueType :db.type/string
                         :db/unique    :db.unique/identity}
   :roles/admin         {:db/valueType :db.type/string}
   :roles/maintainers   {:db/valueType   :db.type/string
                         :db/cardinality :db.cardinality/many}
   :roles/ignored       {:db/valueType   :db.type/string
                         :db/cardinality :db.cardinality/many}})

;; ---------------------------------------------------------------------------
;; Config: mailbox mappings
;; ---------------------------------------------------------------------------

(defn- mailbox-id
  "Compute a stable mailbox identifier from a mailbox config map.
  Matches bark-ingest.db/mailbox-id."
  [mb]
  (str (:host mb) ":" (:user mb)))

(defn- build-mailbox-map
  "Build a map of {mailbox-id → {:email ... :admin ... :mailing-list-email ...}}
  from config. Falls back to top-level :admin for mailboxes without their own."
  [config]
  (let [default-admin (:admin config)]
    (into {}
          (map (fn [mb]
                 [(mailbox-id mb)
                  {:email              (:email mb)
                   :admin              (or (:admin mb) default-admin)
                   :mailing-list-email (:mailing-list-email mb)}]))
          (:mailboxes config))))

;; ---------------------------------------------------------------------------
;; Per-mailbox roles: read and update
;; ---------------------------------------------------------------------------

(defn get-roles
  "Return the roles entity for a mailbox-email, or a default map."
  [db mailbox-email]
  (or (d/pull db '[:roles/admin :roles/maintainers :roles/ignored]
              [:roles/mailbox-email mailbox-email])
      {}))

(defn ensure-mailbox-roles!
  "Ensure a roles entity exists for each mailbox in config, seeding admin."
  [conn config]
  (let [default-admin (:admin config)]
    (doseq [mb (:mailboxes config)]
      (let [mb-email (:email mb)
            admin    (or (:admin mb) default-admin)
            existing (d/q '[:find ?e .
                            :in $ ?mbe
                            :where [?e :roles/mailbox-email ?mbe]]
                          (d/db conn) mb-email)]
        (if existing
          ;; Update admin from config (may have changed)
          (d/transact! conn [{:roles/mailbox-email mb-email
                              :roles/admin         admin}])
          ;; Create new roles entity
          (do (d/transact! conn [{:roles/mailbox-email mb-email
                                  :roles/admin         admin}])
              (println (str "  Initialized roles for " mb-email
                            " (admin: " admin ")"))))))))

(defn- roles-set
  "Get a role attribute as a set (handles nil, single val, collection)."
  [roles attr]
  (let [v (get roles attr)]
    (cond
      (nil? v)    #{}
      (string? v) #{v}
      :else       (set v))))

(defn admin? [roles addr]
  (= (:roles/admin roles) addr))

(defn maintainer? [roles addr]
  (contains? (roles-set roles :roles/maintainers) addr))

(defn admin-or-maintainer? [roles addr]
  (or (admin? roles addr) (maintainer? roles addr)))

(defn ignored? [roles addr]
  (contains? (roles-set roles :roles/ignored) addr))

(defn- roles-eid [conn mailbox-email]
  (d/q '[:find ?e .
         :in $ ?mbe
         :where [?e :roles/mailbox-email ?mbe]]
       (d/db conn) mailbox-email))

(defn- add-role! [conn mailbox-email attr addresses]
  (when-let [eid (roles-eid conn mailbox-email)]
    (doseq [addr addresses]
      (d/transact! conn [[:db/add eid attr addr]]))))

(defn- remove-role! [conn mailbox-email attr addresses]
  (when-let [eid (roles-eid conn mailbox-email)]
    (doseq [addr addresses]
      (d/transact! conn [[:db/retract eid attr addr]]))))

;; ---------------------------------------------------------------------------
;; Role commands: parsed from email body lines
;;
;; Syntax: "Command: addr1 addr2 ..."
;; Commands are case-sensitive, at start of line, with mandatory colon.
;; All commands are scoped to the mailbox the email arrived in.
;; ---------------------------------------------------------------------------

(def role-command-pattern
  #"(?m)^(Add admin|Remove admin|Add maintainer|Remove maintainer|Ignore|Unignore):\s+(.+)$")

(defn- parse-addresses
  "Split a space-separated string of email addresses."
  [s]
  (when s
    (remove str/blank? (str/split (str/trim s) #"\s+"))))

(defn- parse-role-commands
  "Parse all role commands from body text.
  Returns a seq of {:command str :addresses [str ...]}."
  [body-text]
  (when body-text
    (->> (re-seq role-command-pattern body-text)
         (map (fn [[_ cmd addrs]]
                {:command   cmd
                 :addresses (parse-addresses addrs)})))))

(defn apply-role-commands!
  "Process role commands from an email, scoped to mailbox-email.
  Returns the number of commands applied."
  [conn roles mailbox-email from-addr body-text]
  (let [commands (parse-role-commands body-text)
        is-admin (admin? roles from-addr)
        is-maint (admin-or-maintainer? roles from-addr)]
    (reduce
     (fn [n {:keys [command addresses]}]
       (let [applied
             (cond
               ;; Admin-only commands
               (and is-admin (= command "Add admin"))
               ;; For per-mailbox model, "Add admin" changes the admin
               ;; Only one admin per mailbox, so take the first address
               (when-let [new-admin (first addresses)]
                 (d/transact! conn [{:roles/mailbox-email mailbox-email
                                     :roles/admin         new-admin}])
                 (println (str "    → set admin: " new-admin
                               " (for " mailbox-email ")"))
                 true)

               (and is-admin (= command "Remove admin"))
               ;; Cannot remove the admin, only replace — skip
               (do (println "    → cannot remove admin (use Add admin to replace)")
                   false)

               (and is-admin (= command "Remove maintainer"))
               (do (remove-role! conn mailbox-email :roles/maintainers addresses)
                   (println (str "    → remove maintainer: " (str/join " " addresses)
                                 " (for " mailbox-email ")"))
                   true)

               (and is-admin (= command "Unignore"))
               (do (remove-role! conn mailbox-email :roles/ignored addresses)
                   (println (str "    → unignore: " (str/join " " addresses)
                                 " (for " mailbox-email ")"))
                   true)

               ;; Admin or maintainer commands
               (and is-maint (= command "Add maintainer"))
               (do (add-role! conn mailbox-email :roles/maintainers addresses)
                   (println (str "    → add maintainer: " (str/join " " addresses)
                                 " (for " mailbox-email ")"))
                   true)

               (and is-maint (= command "Ignore"))
               (do (add-role! conn mailbox-email :roles/ignored addresses)
                   (println (str "    → ignore: " (str/join " " addresses)
                                 " (for " mailbox-email ")"))
                   true)

               ;; No permission or unknown command
               :else false)]
         (if applied (inc n) n)))
     0
     commands)))

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

(def announcement-types
  "Report types that require admin/maintainer permission to create."
  #{:announcement :release :change})

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

(defn can-create-report?
  "Check if the sender has permission to create this report type.
  Announcements require admin/maintainer for the email's mailbox."
  [roles from-addr report-info]
  (if (announcement-types (:type report-info))
    (admin-or-maintainer? roles from-addr)
    true))

;; ---------------------------------------------------------------------------
;; Trigger detection: body lines that update report state
;;
;; Words are case-sensitive. A punctuation mark among .,;: is mandatory.
;; Triggers are matched at the beginning of a line.
;; ---------------------------------------------------------------------------

(defn- trigger-pattern
  "Build a regex that matches any of the given words at start of line,
  followed by mandatory punctuation [.,;:]."
  [& words]
  (re-pattern (str "(?m)^(" (str/join "|" words) ")[.,;:]")))

(defn- match-triggers
  "Match a map of {key pattern} against body-text.
  Returns a map of {qualified-attr true} for each match."
  [triggers body-text]
  (into {}
        (keep (fn [[state-key pattern]]
                (when (re-find pattern body-text)
                  [(keyword "report" (name state-key)) true])))
        triggers))

(defn- match-unset-triggers
  "Match a map of {key pattern} against body-text.
  Returns a set of qualified attrs for each match."
  [triggers body-text]
  (into #{}
        (keep (fn [[state-key pattern]]
                (when (re-find pattern body-text)
                  (keyword "report" (name state-key)))))
        triggers))

;; --- Set triggers: match → set :report/<state> to email ref ---

(def bug-triggers
  {:acked  (trigger-pattern "Approved" "Confirmed")
   :owned  (trigger-pattern "Handled")
   :closed (trigger-pattern "Canceled" "Fixed")})

(def patch-triggers
  {:acked  (trigger-pattern "Approved" "Reviewed")
   :owned  (trigger-pattern "Handled")
   :closed (trigger-pattern "Canceled" "Applied")})

(def request-triggers
  {:acked  (trigger-pattern "Approved")
   :owned  (trigger-pattern "Handled")
   :closed (trigger-pattern "Canceled" "Done" "Closed")})

(def announcement-triggers
  {:closed (trigger-pattern "Canceled")})

;; --- Unset triggers: match → retract :report/<state> ---
;; "Not urgent" is anchored to ^, so it cannot match "Urgent" alone.

(def report-unset-triggers
  {:urgent    (trigger-pattern "Not urgent")
   :important (trigger-pattern "Not important")})

;; --- Priority triggers: match → set :report/<state> to email ref ---
;; Apply to reports (bug, patch, request) only.

(def report-priority-triggers
  {:urgent    (trigger-pattern "Urgent")
   :important (trigger-pattern "Important")})

;; --- Map from report type to triggers ---

(def set-triggers-by-type
  {:bug          bug-triggers
   :patch        patch-triggers
   :request      request-triggers
   :announcement announcement-triggers
   :release      announcement-triggers
   :change       announcement-triggers})

(def report-types-with-priority
  #{:bug :patch :request})

(defn detect-triggers
  "Given a report type and email body text, return:
  {:set {attr true ...} :unset #{attr ...}}
  where :set attrs should be set to the email ref,
  and :unset attrs should be retracted."
  [report-type body-text]
  (when body-text
    (let [sets     (when-let [trigs (set-triggers-by-type report-type)]
                     (match-triggers trigs body-text))
          priority (when (report-types-with-priority report-type)
                     (match-triggers report-priority-triggers body-text))
          unsets   (when (report-types-with-priority report-type)
                     (match-unset-triggers report-unset-triggers body-text))
          ;; Unset wins over set when both match
          all-sets (into {} (remove (fn [[k _]] (contains? unsets k)))
                         (merge sets priority))]
      (when (or (seq all-sets) (seq unsets))
        {:set   all-sets
         :unset unsets}))))

(defn- ref-eid
  "Extract entity id from a pulled ref value (could be {:db/id N} or N)."
  [v]
  (if (map? v) (:db/id v) v))

(def state-attrs
  [:report/acked :report/owned :report/closed
   :report/urgent :report/important])

(defn apply-triggers!
  "Check if an email triggers state changes on a report.
  Sets use the email eid as the ref value. Unsets retract the attribute."
  [conn report-eid report-type email]
  (let [body-text (or (:email/body-text email)
                      (:email/body-text-from-html email))
        result    (detect-triggers report-type body-text)]
    (when result
      (let [eid     (:db/id email)
            current (d/pull (d/db conn) state-attrs report-eid)
            new-sets (into {}
                          (remove (fn [[k _]] (get current k)))
                          (:set result))
            new-unsets (into #{}
                            (filter (fn [k] (get current k)))
                            (:unset result))
            set-tx   (when (seq new-sets)
                       [(into {:db/id report-eid}
                              (map (fn [[k _]] [k eid]))
                              new-sets)])
            unset-tx (when (seq new-unsets)
                       (mapv (fn [attr]
                               [:db/retract report-eid attr
                                (ref-eid (get current attr))])
                             new-unsets))
            all-tx   (vec (concat set-tx unset-tx))]
        (when (seq all-tx)
          (d/transact! conn all-tx)
          (let [labels (concat
                        (map (fn [[k _]] (name k)) new-sets)
                        (map (fn [k] (str "un-" (name k))) new-unsets))]
            (println (str "    → " (str/join ", " labels)
                          " (by " (:email/message-id email) ")"))))))))

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
      irt           (conj irt)
      (coll? refs)  (into refs))))

;; ---------------------------------------------------------------------------
;; Thread index: message-id → #{set of report entity ids}
;; Type index:   report entity id → report type keyword
;; ---------------------------------------------------------------------------

(defn- index-assoc
  "Associate a message-id with a report eid in the thread index."
  [idx mid rid]
  (update idx mid (fnil conj #{}) rid))

(defn build-indexes
  "Build the thread index and type index from existing reports and descendants.
  Returns {:thread-index {mid → #{rids}} :type-index {rid → type}}."
  [db]
  (let [reports     (d/q '[:find ?rid ?mid ?type
                            :where
                            [?rid :report/message-id ?mid]
                            [?rid :report/type ?type]]
                         db)
        descendants (d/q '[:find ?rid ?dmid
                            :where
                            [?rid :report/descendants ?de]
                            [?de :email/message-id ?dmid]]
                         db)
        thread-idx  (as-> {} idx
                      (reduce (fn [m [rid mid _]] (index-assoc m mid rid)) idx reports)
                      (reduce (fn [m [rid dmid]]  (index-assoc m dmid rid)) idx descendants))
        type-idx    (into {} (map (fn [[rid _ type]] [rid type])) reports)]
    {:thread-index thread-idx
     :type-index   type-idx}))

(defn find-reports-for-email
  "Given an email and the thread index, return the set of report entity ids
  this email is a descendant of, or empty set."
  [email thread-index]
  (let [ancestors (ancestor-mids email)]
    (reduce into #{} (keep thread-index ancestors))))

;; ---------------------------------------------------------------------------
;; Database operations
;; ---------------------------------------------------------------------------

(defn get-last-run [db]
  (d/q '[:find ?t .
         :where
         [?e :digest/id "watermark"]
         [?e :digest/last-run ?t]]
       db))

(defn save-last-run! [conn ts]
  (d/transact! conn [{:digest/id       "watermark"
                      :digest/last-run  ts}]))

(def email-pull-pattern
  '[:db/id :email/uid :email/imap-uid :email/mailbox :email/subject :email/message-id
    :email/in-reply-to :email/references
    :email/from-address :email/date-sent :email/ingested-at
    :email/body-text :email/body-text-from-html
    {:email/attachments [:attachment/filename
                         :attachment/content-type]}])

(defn emails-since [db since-ts]
  (->> (d/q (list :find (list 'pull '?e email-pull-pattern)
                  :in '$ '?since
                  :where
                  ['?e :email/ingested-at '?t]
                  '[(> ?t ?since)])
            db since-ts)
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
                    :email/date-sent :email/imap-uid]}])

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

(defn- format-date [date]
  (let [s (str (or date ""))]
    (subs s 0 (min 16 (count s)))))

(defn- format-report-line [report]
  (let [email (:report/email report)]
    (format "  %-5s %3d %-25s %s  %s"
            (format-flags report)
            (descendant-count report)
            (or (:email/from-address email) "?")
            (format-date (:email/date-sent email))
            (or (:email/subject email) "(no subject)"))))

(defn- format-extra [report]
  (let [parts (remove nil?
                      [(when-let [v (:report/version report)]   (str "(" v ")"))
                       (when-let [t (:report/topic report)]     (str "[" t "]"))
                       (when-let [s (:report/patch-seq report)] (str "(" s ")"))
                       (when-let [sources (:report/patch-source report)]
                         (str "src:" (str/join "," (map name sources))))])]
    (when (seq parts)
      (str " " (str/join " " parts)))))

;; ---------------------------------------------------------------------------
;; Commands
;; ---------------------------------------------------------------------------

(defn cmd-digest!
  [conn mailbox-map process-all?]
  (let [db       (d/db conn)
        last-run (get-last-run db)
        emails   (if process-all?
                   (do (println "Processing ALL emails...")
                       (all-emails db))
                   (if last-run
                     (do (println (str "Processing emails since " last-run "..."))
                         (emails-since db last-run))
                     (do (println "First run — processing ALL emails...")
                         (all-emails db))))
        sorted   (sort-by (fn [e] (or (:email/ingested-at e)
                                       (:email/date-sent e)
                                       (java.util.Date. 0)))
                           emails)
        {:keys [thread-index type-index]} (build-indexes db)]
    (println (str "Found " (count sorted) " email(s) to scan. "
                  "Thread index: " (count thread-index) " entries."))
    (let [{:keys [created threaded skipped]}
          (reduce
           (fn [{:keys [created threaded skipped
                        thread-index type-index]} email]
             (let [message-id    (:email/message-id email)
                   eid           (:db/id email)
                   from-addr     (:email/from-address email)
                   mb-id         (:email/mailbox email)
                   ;; Resolve mailbox-email from mailbox-id
                   mailbox-email (get-in mailbox-map [mb-id :email])
                   ;; Get per-mailbox roles
                   roles         (if mailbox-email
                                   (get-roles (d/db conn) mailbox-email)
                                   {})
                   body-text     (or (:email/body-text email)
                                     (:email/body-text-from-html email))]
               ;; Skip ignored addresses
               (if (and from-addr (ignored? roles from-addr))
                 (do (println (str "  [ignored] " from-addr " — " (:email/subject email)))
                     {:created created :threaded threaded :skipped (inc skipped)
                      :thread-index thread-index :type-index type-index})
                 ;; Process role commands scoped to this mailbox
                 (let [_ (when (and from-addr body-text mailbox-email)
                           (apply-role-commands! conn roles mailbox-email
                                                 from-addr body-text))
                       ;; 1. Does it trigger a new report?
                       report-info (detect-report email)
                       permitted?  (and report-info from-addr
                                        (can-create-report? roles from-addr report-info))
                       new-report? (and permitted?
                                        (not (report-exists? (d/db conn) message-id)))
                       [created thread-index type-index]
                       (if new-report?
                         (do (println (str "  [" (name (:type report-info)) "] "
                                           (:email/subject email)))
                             (create-report! conn eid message-id report-info)
                             (let [report-eid (d/q '[:find ?r .
                                                     :in $ ?mid
                                                     :where [?r :report/message-id ?mid]]
                                                   (d/db conn) message-id)]
                               [(inc created)
                                (index-assoc thread-index message-id report-eid)
                                (assoc type-index report-eid (:type report-info))]))
                         (do (when (and report-info (not permitted?))
                               (println (str "  [denied] " from-addr
                                             " cannot create " (name (:type report-info)))))
                             [created thread-index type-index]))
                       ;; 2. Is it a descendant of existing report(s)?
                       parent-report-eids (find-reports-for-email email thread-index)
                       [threaded thread-index]
                       (if (seq parent-report-eids)
                         (do (doseq [rid parent-report-eids]
                               (add-descendant! conn rid eid)
                               (when-let [rtype (type-index rid)]
                                 (apply-triggers! conn rid rtype email)))
                             [(+ threaded (count parent-report-eids))
                              (reduce #(index-assoc %1 message-id %2)
                                      thread-index parent-report-eids)])
                         [threaded thread-index])]
                   {:created created :threaded threaded :skipped skipped
                    :thread-index thread-index :type-index type-index}))))
           {:created 0 :threaded 0 :skipped 0
            :thread-index thread-index :type-index type-index}
           sorted)]
      (save-last-run! conn (java.util.Date.))
      (println (str "Created " created " report(s), "
                    "threaded " threaded " email(s), "
                    "skipped " skipped " ignored.")))))

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

(defn cmd-roles
  "Display roles for each mailbox."
  [conn mailbox-map]
  (if (empty? mailbox-map)
    (println "No mailboxes configured.")
    (doseq [[mb-id {:keys [email]}] (sort-by key mailbox-map)]
      (let [roles (get-roles (d/db conn) email)
            maints (roles-set roles :roles/maintainers)
            ignored (roles-set roles :roles/ignored)]
        (println (str "\n" email " (" mb-id ")"))
        (println (str "  Admin:       " (or (:roles/admin roles) "(none)")))
        (println (str "  Maintainers: " (if (seq maints)
                                          (str/join ", " (sort maints))
                                          "(none)")))
        (println (str "  Ignored:     " (if (seq ignored)
                                          (str/join ", " (sort ignored))
                                          "(none)")))))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [args    *command-line-args*
      all?    (some #{"--all"} args)
      clean   (remove #{"--all"} args)
      command (or (first clean) "digest")
      db-path (or (System/getenv "BARK_DB") "data/bark-db")
      ;; Read config.edn for mailbox definitions
      config  (let [f (clojure.java.io/file "config.edn")]
                (when (.exists f)
                  (edn/read-string (slurp f))))
      conn    (d/get-conn db-path report-schema)]
  (try
    ;; Seed per-mailbox roles from config
    (when config
      (ensure-mailbox-roles! conn config))
    (let [mailbox-map (if config (build-mailbox-map config) {})]
      (case command
        "digest"        (cmd-digest! conn mailbox-map all?)
        "bugs"          (cmd-list conn :bug)
        "patches"       (cmd-list conn :patch)
        "requests"      (cmd-list conn :request)
        "announcements" (cmd-list conn :announcement)
        "releases"      (cmd-list conn :release)
        "changes"       (cmd-list conn :change)
        "reports"       (cmd-list conn nil)
        "roles"         (cmd-roles conn mailbox-map)
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
            (println "  roles         Show per-mailbox admins, maintainers, ignored")
            (println "")
            (println "Options:")
            (println "  --all         Rescan all emails (with digest)")
            (println "")
            (println "Environment:")
            (println "  BARK_DB       db path (default: ./data/bark-db)"))))
    (finally
      (d/close conn))))
