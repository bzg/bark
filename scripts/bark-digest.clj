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

(pods/load-pod 'huahaiy/datalevin "0.10.5")

(require '[pod.huahaiy.datalevin :as d])

;; ---------------------------------------------------------------------------
;; Schema — loaded from shared bark-schema.edn
;; ---------------------------------------------------------------------------

(def report-schema
  (edn/read-string (slurp "bark-schema.edn")))

;; ---------------------------------------------------------------------------
;; Config
;; ---------------------------------------------------------------------------

(defn- mailbox-id [mb]
  (str (:host mb) ":" (:user mb)))

(defn- build-mailbox-map [config]
  (let [default-admin (:admin config)]
    (into {}
          (map (fn [mb]
                 [(mailbox-id mb)
                  {:email              (:email mb)
                   :admin              (or (:admin mb) default-admin)
                   :mailing-list-email (:mailing-list-email mb)}]))
          (:mailboxes config))))

;; ---------------------------------------------------------------------------
;; Per-mailbox roles
;; ---------------------------------------------------------------------------

(defn get-roles [db mailbox-email]
  (or (d/pull db '[:roles/admin :roles/maintainers :roles/ignored]
              [:roles/mailbox-email mailbox-email])
      {}))

(defn ensure-mailbox-roles! [conn config]
  (let [default-admin (:admin config)]
    (doseq [mb (:mailboxes config)]
      (let [mb-email (:email mb)
            admin    (or (:admin mb) default-admin)
            existing (d/q '[:find ?e .
                            :in $ ?mbe
                            :where [?e :roles/mailbox-email ?mbe]]
                          (d/db conn) mb-email)]
        (d/transact! conn [{:roles/mailbox-email mb-email
                            :roles/admin         admin}])
        (when-not existing
          (println (str "  Initialized roles for " mb-email
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

(defn apply-role-commands! [conn roles mailbox-email from-addr body-text]
  (let [commands (parse-role-commands body-text)
        is-admin (admin? roles from-addr)
        is-maint (admin-or-maintainer? roles from-addr)]
    (doseq [{:keys [command addresses]} commands]
      (cond
        (and is-admin (= command "Add admin"))
        (when-let [new-admin (first addresses)]
          (d/transact! conn [{:roles/mailbox-email mailbox-email
                              :roles/admin         new-admin}])
          (println (str "    → set admin: " new-admin " (for " mailbox-email ")")))

        (and is-admin (= command "Remove admin"))
        (println "    → cannot remove admin (use Add admin to replace)")

        (and is-admin (= command "Remove maintainer"))
        (do (remove-role! conn mailbox-email :roles/maintainers addresses)
            (println (str "    → remove maintainer: " (str/join " " addresses)
                          " (for " mailbox-email ")")))

        (and is-admin (= command "Unignore"))
        (do (remove-role! conn mailbox-email :roles/ignored addresses)
            (println (str "    → unignore: " (str/join " " addresses)
                          " (for " mailbox-email ")")))

        (and is-maint (= command "Add maintainer"))
        (do (add-role! conn mailbox-email :roles/maintainers addresses)
            (println (str "    → add maintainer: " (str/join " " addresses)
                          " (for " mailbox-email ")")))

        (and is-maint (= command "Ignore"))
        (do (add-role! conn mailbox-email :roles/ignored addresses)
            (println (str "    → ignore: " (str/join " " addresses)
                          " (for " mailbox-email ")")))))))

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
(def announcement-pattern (re-pattern (str "(?i)^" ml-prefix "\\[(ANN|ANNOUNCEMENT)\\]")))
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

(defn can-create-report? [roles from-addr report-info]
  (if (announcement-types (:type report-info))
    (admin-or-maintainer? roles from-addr)
    true))

;; ---------------------------------------------------------------------------
;; Triggers
;; ---------------------------------------------------------------------------

(defn- trigger-pattern [& words]
  (re-pattern (str "(?m)^(" (str/join "|" words) ")[.,;:]")))

(defn- match-triggers [triggers body-text]
  (into {} (keep (fn [[k p]] (when (re-find p body-text) [(keyword "report" (name k)) true]))) triggers))

(defn- match-unset-triggers [triggers body-text]
  (into #{} (keep (fn [[k p]] (when (re-find p body-text) (keyword "report" (name k))))) triggers))

(def bug-triggers
  {:acked (trigger-pattern "Approved" "Confirmed") :owned (trigger-pattern "Handled")
   :closed (trigger-pattern "Canceled" "Fixed")})

(def patch-triggers
  {:acked (trigger-pattern "Approved" "Reviewed") :owned (trigger-pattern "Handled")
   :closed (trigger-pattern "Canceled" "Applied")})

(def request-triggers
  {:acked (trigger-pattern "Approved") :owned (trigger-pattern "Handled")
   :closed (trigger-pattern "Canceled" "Done" "Closed")})

(def announcement-triggers {:closed (trigger-pattern "Canceled")})

(def report-unset-triggers
  {:urgent (trigger-pattern "Not urgent") :important (trigger-pattern "Not important")})

(def report-priority-triggers
  {:urgent (trigger-pattern "Urgent") :important (trigger-pattern "Important")})

(def set-triggers-by-type
  {:bug bug-triggers :patch patch-triggers :request request-triggers
   :announcement announcement-triggers :release announcement-triggers :change announcement-triggers})

(def report-types-with-priority #{:bug :patch :request})

(def vote-up-pattern   #"(?m)^\s*(?:\+1|1\+)\s*$")
(def vote-down-pattern #"(?m)^\s*(?:-1|1-)\s*$")

(defn- detect-vote [body-text]
  (when body-text
    (cond
      (re-find vote-up-pattern body-text)   :up
      (re-find vote-down-pattern body-text) :down)))

(defn apply-vote! [conn report-eid from-addr body-text]
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

(defn detect-triggers [report-type body-text]
  (when body-text
    (let [sets     (when-let [t (set-triggers-by-type report-type)] (match-triggers t body-text))
          priority (when (report-types-with-priority report-type) (match-triggers report-priority-triggers body-text))
          unsets   (when (report-types-with-priority report-type) (match-unset-triggers report-unset-triggers body-text))
          all-sets (into {} (remove (fn [[k _]] (contains? unsets k))) (merge sets priority))]
      (when (or (seq all-sets) (seq unsets))
        {:set all-sets :unset unsets}))))

(defn- ref-eid [v] (if (map? v) (:db/id v) v))

(def state-attrs [:report/acked :report/owned :report/closed :report/urgent :report/important])

(defn apply-triggers! [conn report-eid report-type email]
  (let [body-text  (or (:email/body-text email) (:email/body-text-from-html email))
        from-addr  (:email/from-address email)
        result     (detect-triggers report-type body-text)]
    ;; Votes on POLL requests
    (when (and (= :request report-type) from-addr body-text)
      (apply-vote! conn report-eid from-addr body-text))
    ;; Standard triggers
    (when result
      (let [eid      (:db/id email)
            current  (d/pull (d/db conn) state-attrs report-eid)
            new-sets (into {} (remove (fn [[k _]] (get current k))) (:set result))
            new-unsets (into #{} (filter (fn [k] (get current k))) (:unset result))
            set-tx   (when (seq new-sets)
                       [(into {:db/id report-eid} (map (fn [[k _]] [k eid])) new-sets)])
            unset-tx (when (seq new-unsets)
                       (mapv (fn [attr] [:db/retract report-eid attr (ref-eid (get current attr))]) new-unsets))
            all-tx   (vec (concat set-tx unset-tx))]
        (when (seq all-tx)
          (d/transact! conn all-tx)
          (let [labels (concat (map (fn [[k _]] (name k)) new-sets)
                               (map (fn [k] (str "un-" (name k))) new-unsets))]
            (println (str "    → " (str/join ", " labels)
                          " (by " (:email/message-id email) ")"))))))))

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
  '[:db/id :email/uid :email/imap-uid :email/mailbox :email/subject :email/message-id
    :email/in-reply-to :email/references
    :email/from-address :email/date-sent :email/ingested-at
    :email/body-text :email/body-text-from-html
    {:email/attachments [:attachment/filename :attachment/content-type]}])

(defn emails-since [db since-ts]
  (->> (d/q (list :find (list 'pull '?e email-pull-pattern)
                  :in '$ '?since :where ['?e :email/ingested-at '?t] '[(> ?t ?since)])
            db since-ts)
       (map first)))

(defn all-emails [db]
  (->> (d/q (list :find (list 'pull '?e email-pull-pattern) :where ['?e :email/uid '_]) db)
       (map first)))

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
;; Digest command
;; ---------------------------------------------------------------------------

(defn cmd-digest! [conn mailbox-map process-all?]
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
          (reduce
           (fn [{:keys [created threaded skipped thread-index type-index]} email]
             (let [message-id    (:email/message-id email)
                   eid           (:db/id email)
                   from-addr     (:email/from-address email)
                   mb-id         (:email/mailbox email)
                   mailbox-email (get-in mailbox-map [mb-id :email])
                   roles         (if mailbox-email (get-roles (d/db conn) mailbox-email) {})
                   body-text     (or (:email/body-text email) (:email/body-text-from-html email))]
               (if (and from-addr (ignored? roles from-addr))
                 (do (println (str "  [ignored] " from-addr " — " (:email/subject email)))
                     {:created created :threaded threaded :skipped (inc skipped)
                      :thread-index thread-index :type-index type-index})
                 (do (when (and from-addr body-text mailbox-email)
                       (apply-role-commands! conn roles mailbox-email from-addr body-text))
                   (let [report-info (detect-report email)
                       permitted?  (and report-info from-addr
                                        (can-create-report? roles from-addr report-info))
                       new-report? (and permitted? (not (report-exists? (d/db conn) message-id)))
                       [created thread-index type-index]
                       (if new-report?
                         (do (println (str "  [" (name (:type report-info)) "] " (:email/subject email)))
                             (create-report! conn eid message-id report-info)
                             (when (and (= :release (:type report-info)) (:version report-info))
                               (close-changes-for-release! conn (:version report-info) eid))
                             (let [report-eid (d/q '[:find ?r . :in $ ?mid :where [?r :report/message-id ?mid]]
                                                   (d/db conn) message-id)]
                               [(inc created)
                                (index-assoc thread-index message-id report-eid)
                                (assoc type-index report-eid (:type report-info))]))
                         (do (when (and report-info (not permitted?))
                               (println (str "  [denied] " from-addr " cannot create " (name (:type report-info)))))
                             [created thread-index type-index]))
                       parent-report-eids (find-reports-for-email email thread-index)
                       [threaded thread-index]
                       (if (seq parent-report-eids)
                         (do (doseq [rid parent-report-eids]
                               (add-descendant! conn rid eid)
                               (when-let [rtype (type-index rid)] (apply-triggers! conn rid rtype email)))
                             [(+ threaded (count parent-report-eids))
                              (reduce #(index-assoc %1 message-id %2) thread-index parent-report-eids)])
                         [threaded thread-index])]
                   {:created created :threaded threaded :skipped skipped
                    :thread-index thread-index :type-index type-index})))))
           {:created 0 :threaded 0 :skipped 0
            :thread-index thread-index :type-index type-index}
           sorted)]
      (save-last-run! conn (java.util.Date.))
      (println (str "Created " created " report(s), threaded " threaded
                    " email(s), skipped " skipped " ignored.")))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [args    *command-line-args*
      all?    (some #{"--all"} args)
      db-path (or (System/getenv "BARK_DB") "data/bark-db")
      config  (let [f (clojure.java.io/file "config.edn")]
                (when (.exists f) (edn/read-string (slurp f))))
      conn    (d/get-conn db-path report-schema)]
  (try
    (when config (ensure-mailbox-roles! conn config))
    (let [mailbox-map (if config (build-mailbox-map config) {})]
      (cmd-digest! conn mailbox-map all?))
    (finally
      (d/close conn))))
