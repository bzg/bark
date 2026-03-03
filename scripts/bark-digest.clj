#!/usr/bin/env bb

;; bark-digest.clj — Digest emails into reports.
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
;;   bb digest [--all]   — scan new emails (or all with --all)
;;
;; Environment / defaults:
;;   BARK_DB — path to db (default: ./data/bark-db)

(require '[babashka.pods :as pods]
         '[clojure.string :as str]
         '[clojure.edn :as edn])

(load-file "scripts/bark-common.clj")

(pods/load-pod 'huahaiy/datalevin "0.10.5")

(require '[pod.huahaiy.datalevin :as d])

;; ---------------------------------------------------------------------------
;; Schema — loaded from shared bark-schema.edn
;; ---------------------------------------------------------------------------

(def report-schema
  (edn/read-string (slurp "resources/bark-schema.edn")))

;; ---------------------------------------------------------------------------
;; Config
;; ---------------------------------------------------------------------------

;; classify-source, build-source-map, get-header loaded from bark-common.clj

;; ---------------------------------------------------------------------------
;; Per-source roles
;; ---------------------------------------------------------------------------

(defn get-roles [db source-name]
  (or (d/pull db '[:roles/admin :roles/maintainers :roles/ignored]
              [:roles/source source-name])
      {}))

(defn ensure-source-roles! [conn config]
  (let [default-admin (:admin config)]
    (doseq [{:keys [name admin]} (:sources config)]
      (let [admin    (or admin default-admin)
            existing (d/q '[:find ?e .
                            :in $ ?src
                            :where [?e :roles/source ?src]]
                          (d/db conn) name)]
        (d/transact! conn [{:roles/source name
                            :roles/admin  admin}])
        (when-not existing
          (println (str "  Initialized roles for source " name
                        " (admin: " admin ")")))))))

(defn- roles-set [roles attr]
  (let [v (get roles attr)]
    (cond (nil? v) #{} (string? v) #{v} :else (set v))))

(defn admin? [roles addr]
  (= (:roles/admin roles) addr))

(defn maintainer? [roles addr]
  (contains? (roles-set roles :roles/maintainers) addr))

(defn admin-or-maintainer? [roles addr]
  (or (admin? roles addr) (maintainer? roles addr)))

(defn ignored? [roles addr]
  (contains? (roles-set roles :roles/ignored) addr))

(defn- roles-eid [conn source-name]
  (d/q '[:find ?e .
         :in $ ?src
         :where [?e :roles/source ?src]]
       (d/db conn) source-name))

(defn- add-role! [conn source-name attr addresses]
  (when-let [eid (roles-eid conn source-name)]
    (doseq [addr addresses]
      (d/transact! conn [[:db/add eid attr addr]]))))

(defn- remove-role! [conn source-name attr addresses]
  (when-let [eid (roles-eid conn source-name)]
    (doseq [addr addresses]
      (d/transact! conn [[:db/retract eid attr addr]]))))

;; ---------------------------------------------------------------------------
;; Email header helpers
;; ---------------------------------------------------------------------------

;; get-header loaded from bark-common.clj

(defn- from-mailing-list?
  "True if the email was delivered through a mailing list (has List-Id header)."
  [email]
  (some? (get-header (:email/headers-edn email) "List-Id")))

(defn- list-post-address
  "Extract the email address from the List-Post header, e.g.
  \"<mailto:list@example.org>\" → \"list@example.org\".  Returns nil if absent."
  [email]
  (when-let [lp (get-header (:email/headers-edn email) "List-Post")]
    (second (re-find #"<mailto:([^>]+)>" lp))))

;; ---------------------------------------------------------------------------
;; Role commands
;; ---------------------------------------------------------------------------

(def role-command-pattern
  #"(?m)^(Add admin|Remove admin|Add maintainer|Remove maintainer|Ignore|Unignore):\s+(.+)$")

(defn- parse-addresses [s]
  (when s (remove str/blank? (str/split (str/trim s) #"\s+"))))

(defn- parse-role-commands [body-text]
  (when body-text
    (->> (re-seq role-command-pattern body-text)
         (map (fn [[_ cmd addrs]]
                {:command cmd :addresses (parse-addresses addrs)})))))

(def ^:private role-dispatch
  {"Add admin"         {:requires :admin  :action :set-admin}
   "Remove admin"      {:requires :admin  :action :noop :msg "cannot remove admin (use Add admin to replace)"}
   "Remove maintainer" {:requires :admin  :attr :roles/maintainers :action :remove}
   "Unignore"          {:requires :admin  :attr :roles/ignored     :action :remove}
   "Add maintainer"    {:requires :maint  :attr :roles/maintainers :action :add}
   "Ignore"            {:requires :maint  :attr :roles/ignored     :action :add}})

(defn apply-role-commands! [conn roles source-name from-addr body-text]
  (let [commands  (parse-role-commands body-text)
        is-admin  (admin? roles from-addr)
        is-maint  (admin-or-maintainer? roles from-addr)]
    (doseq [{:keys [command addresses]} commands]
      (when-let [{:keys [requires action attr msg]} (role-dispatch command)]
        (when (case requires :admin is-admin :maint is-maint)
          (case action
            :set-admin (when-let [new-admin (first addresses)]
                         (d/transact! conn [{:roles/source source-name
                                             :roles/admin  new-admin}])
                         (println (str "    → set admin: " new-admin " (for " source-name ")")))
            :add       (do (add-role! conn source-name attr addresses)
                           (println (str "    → " (str/lower-case command) ": "
                                         (str/join " " addresses) " (for " source-name ")")))
            :remove    (do (remove-role! conn source-name attr addresses)
                           (println (str "    → " (str/lower-case command) ": "
                                         (str/join " " addresses) " (for " source-name ")")))
            :noop      (println (str "    → " msg))))))))
;; ---------------------------------------------------------------------------
;; Report detection
;; ---------------------------------------------------------------------------

;; Mailing list managers may prepend "[listname] " or similar bracketed
;; prefixes to the subject.  The bark-specific tag is always the last
;; bracketed construct, so we skip zero or more leading "[...] " groups.
(def ^:private ml-prefix "(?:\\[[^\\]]*\\]\\s*)*")

(def bug-pattern          (re-pattern (str "(?i)^" ml-prefix "\\[BUG(?:\\s+([^\\]]*))?\\]")))
(def patch-subject-pattern (re-pattern (str "(?i)^" ml-prefix "\\[PATCH(?:\\s+([^\\]]*))?\\]")))
(def patch-seq-pattern    #"(\d+/\d+)\s*$")
(def request-pattern      (re-pattern (str "(?i)^" ml-prefix "\\[(POLL|FR|FP|RFC|RFE|TASK)\\]")))
(def announcement-pattern (re-pattern (str "(?i)^" ml-prefix "\\[(BLOG|ANN|ANNOUNCEMENT)\\]")))
(def release-pattern      (re-pattern (str "(?i)^" ml-prefix "\\[(REL|RELEASE)(?:\\s+([^\\]]*))?\\]")))
(def change-pattern       (re-pattern (str "(?i)^" ml-prefix "\\[(CHG|CHANGE)(?:\\s+([^\\]]*))?\\]")))

(defn detect-bug [subject]
  (when-let [m (re-find bug-pattern subject)]
    {:type :bug :version (when (second m) (str/trim (second m)))}))

(defn detect-patch-subject [subject]
  (when-let [m (re-find patch-subject-pattern subject)]
    (let [inner   (when (second m) (str/trim (second m)))
          seq-m   (when inner (re-find patch-seq-pattern inner))
          seq-str (when seq-m (first seq-m))
          topic   (when inner
                    (let [t (if seq-str
                              (str/trim (subs inner 0 (- (count inner) (count seq-str))))
                              inner)]
                      (when-not (str/blank? t) t)))]
      (cond-> {:type :patch :patch-source #{:subject}}
        seq-str (assoc :patch-seq seq-str)
        topic   (assoc :topic topic)))))

(defn detect-request [subject] (when (re-find request-pattern subject) {:type :request}))
(defn detect-announcement [subject] (when (re-find announcement-pattern subject) {:type :announcement}))

(defn- detect-versioned-tag [pattern type subject]
  (when-let [m (re-find pattern subject)]
    (let [ver (nth m 2 nil)]
      (cond-> {:type type}
        (and ver (not (str/blank? ver))) (assoc :version (str/trim ver))))))

(defn detect-release [subject] (detect-versioned-tag release-pattern :release subject))
(defn detect-change [subject] (detect-versioned-tag change-pattern :change subject))

;; Attachment & inline patch detection

(def patch-filename-pattern #"(?i)\.(patch|diff)$")

(defn has-patch-attachment? [attachments]
  (some (fn [att] (when-let [f (:attachment/filename att)] (re-find patch-filename-pattern f)))
        attachments))

(def inline-patch-indicators
  [#"(?m)^diff --git " #"(?m)^--- a/" #"(?m)^\+\+\+ b/"
   #"(?m)^@@ [-+]\d+" #"(?m)^index [0-9a-f]+\.\.[0-9a-f]+"])

(defn has-inline-patch? [body-text]
  (when body-text (>= (count (filter #(re-find % body-text) inline-patch-indicators)) 2)))

(defn detect-patch [subject attachments body-text]
  (let [from-subject    (detect-patch-subject subject)
        from-attachment (when (has-patch-attachment? attachments) :attachment)
        from-inline     (when (has-inline-patch? body-text) :inline)
        sources         (cond-> #{}
                          from-subject    (into (:patch-source from-subject))
                          from-attachment (conj :attachment)
                          from-inline     (conj :inline))]
    (when (seq sources)
      (cond-> {:type :patch :patch-source sources}
        (:patch-seq from-subject) (assoc :patch-seq (:patch-seq from-subject))
        (:topic from-subject)     (assoc :topic (:topic from-subject))))))

(def announcement-types #{:announcement :release :change})

(defn detect-report [email]
  (when-let [subject (:email/subject email)]
    (let [attachments (:email/attachments email)
          body-text   (or (:email/body-text email) (:email/body-text-from-html email))]
      (or (detect-bug subject)
          (detect-patch subject attachments body-text)
          (detect-request subject)
          (detect-announcement subject)
          (detect-release subject)
          (detect-change subject)))))

(defn can-create-report?
  "Check if from-addr is allowed to create this report.
  Announcements require admin/maintainer.
  On list-backed sources, non-privileged users must send through
  the list (List-Post header matches :mailing-list-email)."
  [roles from-addr report-info email source-cfg]
  (cond
    (announcement-types (:type report-info))
    (admin-or-maintainer? roles from-addr)

    (admin-or-maintainer? roles from-addr)
    true

    :else
    (let [ml-email (:mailing-list-email source-cfg)]
      (if (nil? ml-email)
        ;; No mailing list configured — anyone can create
        true
        ;; Must have matching List-Post header
        (let [lp (list-post-address email)]
          (when (and lp (not= lp ml-email))
            (println (str "    [list-post mismatch] expected " ml-email " got " lp)))
          (= lp ml-email))))))

;; ---------------------------------------------------------------------------
;; Triggers
;; ---------------------------------------------------------------------------

(defn- trigger-pattern [& words]
  (re-pattern (str "(?m)^(" (str/join "|" words) ")[.,;:]")))

(defn- match-triggers [triggers body-text]
  (into {} (keep (fn [[k p]] (when (re-find p body-text) [(keyword "report" (name k)) true]))) triggers))

(defn- match-unset-triggers [triggers body-text]
  (into #{} (keep (fn [[k p]] (when (re-find p body-text) (keyword "report" (name k))))) triggers))

(def default-trigger-words
  "Default trigger words per report type and action."
  {:bug          {:acked ["Approved" "Confirmed"] :owned ["Handled"] :closed ["Canceled" "Fixed"]}
   :patch        {:acked ["Approved" "Reviewed"]  :owned ["Handled"] :closed ["Canceled" "Applied"]}
   :request      {:acked ["Approved"]             :owned ["Handled"] :closed ["Canceled" "Done" "Closed"]}
   :announcement {:closed ["Canceled"]}
   :release      {:closed ["Canceled"]}
   :change       {:closed ["Canceled"]}})

(defn- compile-trigger-words
  "Compile a map of action→word-lists into action→regex-patterns."
  [action-map]
  (into {} (map (fn [[action words]] [action (apply trigger-pattern words)])) action-map))

(defn- compile-triggers-by-type
  "Compile a full type→action→words map into type→action→pattern."
  [tw]
  (into {} (map (fn [[rtype actions]] [rtype (compile-trigger-words actions)])) tw))

(def default-triggers-by-type (compile-triggers-by-type default-trigger-words))

(defn- build-source-triggers
  "Merge source :triggers config with defaults and compile to patterns."
  [source-cfg]
  (if-let [custom (:triggers source-cfg)]
    (compile-triggers-by-type
     (reduce-kv (fn [acc rtype overrides]
                  (assoc acc rtype (merge (get default-trigger-words rtype) overrides)))
                default-trigger-words custom))
    default-triggers-by-type))

(def report-unset-triggers
  {:urgent (trigger-pattern "Not urgent") :important (trigger-pattern "Not important")})

(def report-priority-triggers
  {:urgent (trigger-pattern "Urgent") :important (trigger-pattern "Important")})

(def report-types-with-priority #{:bug :patch :request})

(def vote-up-pattern   #"(?m)^\s*(?:\+1|1\+)\s*$")
(def vote-down-pattern #"(?m)^\s*(?:-1|1-)\s*$")

(defn- detect-vote [body-text]
  (when body-text
    (cond
      (re-find vote-up-pattern body-text)   :up
      (re-find vote-down-pattern body-text) :down)))

(defn apply-vote! [conn report-eid from-addr body-text]
  ;; NB: read-then-write is safe because cmd-digest! processes emails
  ;; sequentially. If parallelized, this needs a transaction function.
  (when-let [vote (detect-vote body-text)]
    (let [db      (d/db conn)
          current (d/pull db [:report/voters :report/votes-up :report/votes-down] report-eid)
          voters  (set (:report/voters current))]
      (when-not (contains? voters from-addr)
        (let [attr (if (= vote :up) :report/votes-up :report/votes-down)
              n    (or (get current attr) 0)]
          (d/transact! conn [{:db/id report-eid
                              attr (inc n)
                              :report/voters from-addr}])
          (println (str "    → vote " (if (= vote :up) "+1" "-1")
                        " by " from-addr)))))))

(defn detect-triggers [report-type body-text triggers-by-type]
  (when body-text
    (let [sets     (when-let [t (triggers-by-type report-type)] (match-triggers t body-text))
          priority (when (report-types-with-priority report-type) (match-triggers report-priority-triggers body-text))
          unsets   (when (report-types-with-priority report-type) (match-unset-triggers report-unset-triggers body-text))
          all-sets (into {} (remove (fn [[k _]] (contains? unsets k))) (merge sets priority))]
      (when (or (seq all-sets) (seq unsets))
        {:set all-sets :unset unsets}))))

(defn- ref-eid [v] (if (map? v) (:db/id v) v))

(def state-attrs [:report/acked :report/owned :report/closed :report/urgent :report/important])

(defn apply-triggers! [conn report-eid report-type email source-map]
  (let [body-text  (or (:email/body-text email) (:email/body-text-from-html email))
        from-addr  (:email/from-address email)
        src-name   (d/q '[:find ?src . :in $ ?rid :where
                          [?rid :report/email ?e] [?e :email/source ?src]]
                        (d/db conn) report-eid)
        triggers   (build-source-triggers (get source-map src-name))
        result     (detect-triggers report-type body-text triggers)]
    (when (and (= :request report-type) from-addr body-text)
      (apply-vote! conn report-eid from-addr body-text))
    (when result
      (let [eid      (:db/id email)
            current  (d/pull (d/db conn) state-attrs report-eid)
            new-sets (into {} (remove (fn [[k _]] (get current k))) (:set result))
            new-unsets (into #{} (filter current) (:unset result))
            set-tx   (when (seq new-sets)
                       [(into {:db/id report-eid} (map (fn [[k _]] [k eid])) new-sets)])
            unset-tx (when (seq new-unsets)
                       (mapv (fn [attr] [:db/retract report-eid attr (ref-eid (get current attr))]) new-unsets))
            all-tx   (into (vec set-tx) unset-tx)]
        (when (seq all-tx)
          (d/transact! conn all-tx)
          (println (str "    → "
                        (str/join ", " (concat (map (comp name key) new-sets)
                                               (map #(str "un-" (name %)) new-unsets)))
                        " (by " (:email/message-id email) ")")))))))

;; ---------------------------------------------------------------------------
;; Threading
;; ---------------------------------------------------------------------------

(defn ancestor-mids [email]
  (let [irt (:email/in-reply-to email) refs (:email/references email)]
    (cond-> #{} irt (conj irt) (coll? refs) (into refs))))

(defn- index-assoc [idx mid rid] (update idx mid (fnil conj #{}) rid))

(defn build-indexes [db]
  (let [reports     (d/q '[:find ?rid ?mid ?type :where
                            [?rid :report/message-id ?mid] [?rid :report/type ?type]] db)
        descendants (d/q '[:find ?rid ?dmid :where
                            [?rid :report/descendants ?de] [?de :email/message-id ?dmid]] db)
        thread-idx  (as-> {} idx
                      (reduce (fn [m [rid mid _]] (index-assoc m mid rid)) idx reports)
                      (reduce (fn [m [rid dmid]]  (index-assoc m dmid rid)) idx descendants))
        type-idx    (into {} (map (fn [[rid _ type]] [rid type])) reports)]
    {:thread-index thread-idx :type-index type-idx}))

(defn find-reports-for-email [email thread-index]
  (reduce into #{} (keep thread-index (ancestor-mids email))))

;; ---------------------------------------------------------------------------
;; DB operations
;; ---------------------------------------------------------------------------

(defn get-last-run [db]
  (d/q '[:find ?t . :where [?e :digest/id "watermark"] [?e :digest/last-run ?t]] db))

(defn save-last-run! [conn ts]
  (d/transact! conn [{:digest/id "watermark" :digest/last-run ts}]))

(def email-pull-pattern
  '[:db/id :email/uid :email/imap-uid :email/source :email/subject :email/message-id
    :email/in-reply-to :email/references
    :email/from-address :email/date-sent :email/ingested-at
    :email/body-text :email/body-text-from-html :email/headers-edn
    {:email/attachments [:attachment/filename :attachment/content-type]}])

(defn emails-since [db since-ts]
  (let [eids (d/q '[:find [?e ...]
                     :in $ ?since
                     :where [?e :email/ingested-at ?t] [(> ?t ?since)]]
                   db since-ts)]
    (d/pull-many db email-pull-pattern eids)))

(defn all-emails [db]
  (let [eids (d/q '[:find [?e ...]
                     :where [?e :email/uid _]]
                   db)]
    (d/pull-many db email-pull-pattern eids)))

(defn report-exists? [db message-id]
  (some? (d/q '[:find ?r . :in $ ?mid :where [?r :report/message-id ?mid]] db message-id)))

(defn create-report! [conn email-eid message-id report-info]
  (d/transact! conn
               [(into {:report/type (:type report-info) :report/email email-eid
                       :report/message-id message-id :report/digested-at (java.util.Date.)}
                      (remove (comp nil? val))
                      {:report/version (:version report-info) :report/topic (:topic report-info)
                       :report/patch-seq (:patch-seq report-info) :report/patch-source (:patch-source report-info)})]))

(defn add-descendant! [conn report-eid email-eid]
  (d/transact! conn [[:db/add report-eid :report/descendants email-eid]]))

(defn link-related-reports!
  "Link a newly created report to all existing reports it's threaded with.
  The relation is bi-directional: both sides get :report/related."
  [conn new-report-eid parent-report-eids]
  (when (seq parent-report-eids)
    (let [txdata (into []
                       (mapcat (fn [rid]
                                 [[:db/add new-report-eid :report/related rid]
                                  [:db/add rid :report/related new-report-eid]]))
                       parent-report-eids)]
      (d/transact! conn txdata))))

(defn close-changes-for-release!
  "When a [REL x] report is created, close any open [CHG x] with the same version."
  [conn version release-email-eid]
  (when (and version (not (str/blank? version)))
    (let [db      (d/db conn)
          open-chgs (d/q '[:find [?r ...]
                            :in $ ?ver
                            :where
                            [?r :report/type :change]
                            [?r :report/version ?ver]
                            (not [?r :report/closed _])]
                          db version)]
      (when (seq open-chgs)
        (d/transact! conn (mapv (fn [r] {:db/id r :report/closed release-email-eid}) open-chgs))
        (println (str "    → auto-closed " (count open-chgs)
                      " [CHG " version "] (superseded by release)"))))))

;; ---------------------------------------------------------------------------
;; Patch series
;; ---------------------------------------------------------------------------

(defn- parse-seq
  "Parse \"2/5\" into [2 5], or nil."
  [s]
  (when s
    (let [[_ n m] (re-find #"(\d+)/(\d+)" s)]
      (when (and n m)
        [(parse-long n) (parse-long m)]))))

(defn- series-id
  "Compute a stable series identity from topic, sender, and total count."
  [topic sender total]
  (str (or topic "") "|" sender "|" total))

(defn- next-series-id
  "Find the next available series-id by appending a revision suffix.
  E.g. parser|user@test.org|3, parser|user@test.org|3#2, etc."
  [db topic sender total]
  (let [base (series-id topic sender total)
        existing (d/q '[:find [?sid ...]
                        :in $ ?prefix
                        :where
                        [?s :series/id ?sid]
                        [(clojure.string/starts-with? ?sid ?prefix)]]
                      db base)]
    (if (empty? existing)
      base
      (str base "#" (inc (count existing))))))

(defn find-open-series
  "Find an open series matching topic+sender+total."
  [db topic sender total]
  (d/q '[:find ?s .
         :in $ ?topic ?sender ?exp
         :where
         [?s :series/topic ?topic]
         [?s :series/sender ?sender]
         [?s :series/expected ?exp]
         (not [?s :series/closed _])]
       db (or topic "") sender total))

(defn find-open-series-by-topic-sender
  "Find any open series matching topic+sender (any total)."
  [db topic sender]
  (when (and topic sender)
    (d/q '[:find [?s ...]
           :in $ ?topic ?sender
           :where
           [?s :series/topic ?topic]
           [?s :series/sender ?sender]
           (not [?s :series/closed _])]
         db topic sender)))

(defn create-series!
  "Create a new series entity. Returns the series eid."
  [conn topic sender total]
  (let [sid (next-series-id (d/db conn) topic sender total)]
    (d/transact! conn [{:series/id       sid
                        :series/topic    (or topic "")
                        :series/sender   sender
                        :series/expected total}])
    (d/q '[:find ?s . :in $ ?sid :where [?s :series/id ?sid]]
         (d/db conn) sid)))

(defn close-series!
  "Close a series, setting :series/closed to the given email eid."
  [conn series-eid email-eid]
  (d/transact! conn [{:db/id series-eid :series/closed email-eid}]))

(defn add-patch-to-series!
  "Link a patch report to a series and set :report/series back-ref."
  [conn series-eid report-eid]
  (d/transact! conn [[:db/add series-eid :series/patches report-eid]
                      {:db/id report-eid :report/series series-eid}]))

(defn set-cover-letter!
  "Set the cover letter (0/N email) on a series."
  [conn series-eid email-eid]
  (d/transact! conn [{:db/id series-eid :series/cover-letter email-eid}]))

(defn manage-series!
  "After creating a patch report, manage its series membership.
  - Finds or creates the series
  - If this is a cover letter (0/M) or a duplicate 1/M arriving as a
    descendant of an old series, close the old one and start fresh
  - Adds the patch to the series (or sets cover letter for 0/M)"
  [conn report-eid email-eid report-info from-addr parent-report-eids]
  (when-let [[n m] (parse-seq (:patch-seq report-info))]
    (let [topic  (:topic report-info)
          db     (d/db conn)
          ;; Find existing open series for this topic+sender
          existing-series (when topic
                            (find-open-series-by-topic-sender db topic from-addr))
          ;; 0/M always signals a restart (new cover letter).
          ;; 1/M signals restart only if the existing series already
          ;; contains a 1/* patch (i.e. the sequence is truly restarting).
          restart? (and (seq existing-series)
                        (or (zero? n)
                            (and (= 1 n)
                                 (let [existing-seqs
                                       (d/q '[:find [?seq ...]
                                              :in $ [?s ...]
                                              :where
                                              [?s :series/patches ?r]
                                              [?r :report/patch-seq ?seq]]
                                            db existing-series)]
                                   (some #(str/starts-with? % "1/") existing-seqs)))))
          ;; Is this a descendant of any of those existing series' patches?
          ancestor? (when restart?
                      (let [old-mids (into
                                      ;; patch report message-ids
                                      (set (d/q '[:find [?mid ...]
                                                  :in $ [?s ...]
                                                  :where [?s :series/patches ?r]
                                                         [?r :report/message-id ?mid]]
                                                db existing-series))
                                      ;; cover letter email message-ids
                                      (d/q '[:find [?mid ...]
                                             :in $ [?s ...]
                                             :where [?s :series/cover-letter ?e]
                                                    [?e :email/message-id ?mid]]
                                           db existing-series))
                            parent-mids (set (keep (fn [rid]
                                                     (d/q '[:find ?mid .
                                                            :in $ ?r
                                                            :where [?r :report/message-id ?mid]]
                                                          db rid))
                                                   parent-report-eids))]
                        (some old-mids parent-mids)))]
      ;; Close old series if this is a restart and a descendant
      (when (and restart? ancestor?)
        (doseq [sid existing-series]
          (close-series! conn sid email-eid)
          (println (str "    → auto-closed series "
                        (pr-str (:series/id (d/pull (d/db conn) [:series/id] sid)))
                        " (superseded)"))))
      ;; Find or create the series for this patch
      (let [series-eid (or (find-open-series (d/db conn) topic from-addr m)
                           (let [sid (create-series! conn topic from-addr m)]
                             (println (str "    → new series: "
                                          (pr-str (series-id topic from-addr m))
                                          " (expecting " m " patches)"))
                             sid))]
        (if (zero? n)
          (set-cover-letter! conn series-eid email-eid)
          (add-patch-to-series! conn series-eid report-eid))))))

;; ---------------------------------------------------------------------------
;; Digest command
;; ---------------------------------------------------------------------------

(defn- process-email!
  "Process a single email during digest. Returns updated accumulator.
  Resolves the source from :email/source or by classifying headers."
  [conn source-map sources {:keys [created threaded skipped thread-index type-index] :as acc} email]
  (let [message-id    (:email/message-id email)
        eid           (:db/id email)
        from-addr     (:email/from-address email)
        ;; Resolve source: use stored value or classify from headers
        source-name   (or (:email/source email)
                          (classify-source (:email/headers-edn email) sources))
        _             (when (and source-name (not (:email/source email)))
                        (d/transact! conn [{:db/id eid :email/source source-name}]))
        source-cfg    (get source-map source-name)
        roles         (if source-name (get-roles (d/db conn) source-name) {})
        body-text     (or (:email/body-text email) (:email/body-text-from-html email))]
    (if (and from-addr (ignored? roles from-addr))
      (do (println (str "  [ignored] " from-addr " — " (:email/subject email)))
          (assoc acc :skipped (inc skipped)))
      (do (when (and from-addr body-text source-name
                     (not (from-mailing-list? email)))
            (apply-role-commands! conn roles source-name from-addr body-text))
          (let [report-info (detect-report email)
                permitted?  (and report-info from-addr
                                 (can-create-report? roles from-addr report-info
                                                     email source-cfg))
                new-report? (and permitted? (not (report-exists? (d/db conn) message-id)))
                [created thread-index type-index report-eid]
                (if new-report?
                  (do (println (str "  [" (name (:type report-info)) "] " (:email/subject email)))
                      (create-report! conn eid message-id report-info)
                      (when (and (= :release (:type report-info)) (:version report-info))
                        (close-changes-for-release! conn (:version report-info) eid))
                      (let [rid (d/q '[:find ?r . :in $ ?mid :where [?r :report/message-id ?mid]]
                                     (d/db conn) message-id)]
                        [(inc created)
                         (index-assoc thread-index message-id rid)
                         (assoc type-index rid (:type report-info))
                         rid]))
                  (do (when (and report-info (not permitted?))
                        (println (str "  [denied] " from-addr " cannot create " (name (:type report-info)))))
                      [created thread-index type-index nil]))
                parent-report-eids (find-reports-for-email email thread-index)
                [threaded thread-index]
                (if (seq parent-report-eids)
                  (do (doseq [rid parent-report-eids]
                        (add-descendant! conn rid eid)
                        (when-let [rtype (type-index rid)] (apply-triggers! conn rid rtype email source-map)))
                      [(+ threaded (count parent-report-eids))
                       (reduce #(index-assoc %1 message-id %2) thread-index parent-report-eids)])
                  [threaded thread-index])]
            ;; Post-creation: link related reports + manage series
            (when (and report-eid (seq parent-report-eids))
              (link-related-reports! conn report-eid parent-report-eids))
            (when (and report-eid (= :patch (:type report-info))
                       (:patch-seq report-info))
              (manage-series! conn report-eid eid report-info
                              from-addr parent-report-eids))
            {:created created :threaded threaded :skipped skipped
             :thread-index thread-index :type-index type-index})))))

(defn cmd-digest! [conn source-map sources process-all?]
  (let [db       (d/db conn)
        last-run (get-last-run db)
        emails   (if process-all?
                   (do (println "Processing ALL emails...") (all-emails db))
                   (if last-run
                     (do (println (str "Processing emails since " last-run "..."))
                         (emails-since db last-run))
                     (do (println "First run — processing ALL emails...") (all-emails db))))
        sorted   (sort-by (fn [e] (or (:email/ingested-at e) (:email/date-sent e) (java.util.Date. 0))) emails)
        {:keys [thread-index type-index]} (build-indexes db)]
    (println (str "Found " (count sorted) " email(s) to scan. "
                  "Thread index: " (count thread-index) " entries."))
    (let [{:keys [created threaded skipped]}
          (reduce (partial process-email! conn source-map sources)
                  {:created 0 :threaded 0 :skipped 0
                   :thread-index thread-index :type-index type-index}
                  sorted)]
      (save-last-run! conn (java.util.Date.))
      (println (str "Created " created " report(s), threaded " threaded
                    " email(s), skipped " skipped " ignored.")))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(when (= (System/getProperty "babashka.file") *file*)
  (let [args    *command-line-args*
        all?    (some #{"--all"} args)
        db-path (or (System/getenv "BARK_DB") "data/bark-db")
        config  (load-config)
        conn    (d/get-conn db-path report-schema)]
    (try
      (when config (ensure-source-roles! conn config))
      (let [source-map (if config (build-source-map config) {})
            sources    (or (:sources config) [])]
        (cmd-digest! conn source-map sources all?))
      (finally
        (d/close conn)))))
