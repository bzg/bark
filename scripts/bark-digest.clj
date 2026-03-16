#!/usr/bin/env bb

;; bark-digest.clj — Digest emails into reports.
;;
;; BARK: Bug And Report Keeper
;;
;; Orchestrates the digest pipeline:
;;   1. Classify email source
;;   2. Apply role/notify commands
;;   3. Detect report type
;;   4. Create report or thread as descendant
;;   5. Apply triggers to nearest ancestor
;;   6. Manage patch series
;;
;; Detection, triggers, roles, and series logic are in separate modules.
;;
;; Usage:
;;   bb digest [--all]   — scan new emails (or all with --all)
;;
;; Environment / defaults:
;;   BARK_DB — path to db (default: ./data/bark-db)

(require '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[taoensso.timbre :as log])

;; Forward-declared for clj-kondo (provided at runtime by load-file calls below).
(declare ;; bark-common.clj
         load-datalevin-pod! classify-source email-body-text
         load-config build-source-map get-header bark-schema
         days-between
         ;; bark-roles.clj
         get-roles ignored? admin-or-maintainer?
         ensure-source-roles! ensure-notify-defaults!
         apply-role-commands! apply-notify-commands!
         from-mailing-list? can-create-report?
         ;; bark-detect.clj
         detect-report resolve-labels build-patch-entities
         ;; bark-triggers.clj
         apply-triggers-and-directives!
         ;; bark-series.clj
         manage-series!)

(load-file "scripts/bark-common.clj")

(load-datalevin-pod!)

(load-file "scripts/bark-roles.clj")
(load-file "scripts/bark-detect.clj")
(load-file "scripts/bark-triggers.clj")
(load-file "scripts/bark-series.clj")

;; bark-schema is defined in bark-common.clj

;; ---------------------------------------------------------------------------
;; Threading
;; ---------------------------------------------------------------------------

(defn ancestor-mids
  "Return an ordered vector of ancestor message-ids, nearest last.
  Built from References (ordered root->parent per RFC 2822)
  plus In-Reply-To.  Duplicates are removed, order preserved."
  [email]
  (let [raw  (:email/references email)
        refs (if (string? raw)
               (re-seq #"<[^>]+>" raw)
               [])
        irt  (:email/in-reply-to email)
        all  (if (and irt (not (some #{irt} refs)))
               (conj (vec refs) irt)
               (vec refs))]
    (vec (distinct all))))

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

(defn- lookup-reports-by-mid
  "Find report eids matching a message-id, either as report root or descendant."
  [db mid]
  (let [as-root (d/q '[:find [?r ...]
                       :in $ ?mid
                       :where [?r :report/message-id ?mid]]
                     db mid)
        as-desc (d/q '[:find [?r ...]
                       :in $ ?mid
                       :where [?r :report/descendants ?e]
                       [?e :email/message-id ?mid]]
                     db mid)]
    (into (set as-root) as-desc)))

(defn find-reports-for-email
  "Return all report eids threaded with this email (for descendant linking).
  Checks the in-memory batch index first, then falls back to DB lookups."
  [email thread-index db]
  (let [mids (ancestor-mids email)]
    (reduce (fn [acc mid]
              (if-let [from-idx (thread-index mid)]
                (into acc from-idx)
                (into acc (lookup-reports-by-mid db mid))))
            #{} mids)))

(defn find-nearest-report
  "Return the report eids of the nearest ancestor only (for trigger application).
  Walks ancestor-mids from nearest to oldest, checks batch index then DB."
  [email thread-index db]
  (some (fn [mid]
          (or (thread-index mid)
              (let [from-db (lookup-reports-by-mid db mid)]
                (when (seq from-db) from-db))))
        (rseq (ancestor-mids email))))

;; ---------------------------------------------------------------------------
;; DB operations
;; ---------------------------------------------------------------------------

(defn ensure-contributor!
  "Record `from-addr` as a contributor for `source-name` if not already known.
  Called when someone creates a report or threads onto one.
  `date-sent` is the original email date, used as :contributor/since."
  [conn source-name from-addr from-name date-sent]
  (when (and source-name from-addr)
    (let [k (str source-name ":" (str/lower-case from-addr))]
      (when-not (d/q '[:find ?e .
                        :in $ ?k
                        :where [?e :contributor/key ?k]]
                     (d/db conn) k)
        (d/transact! conn [{:contributor/key    k
                            :contributor/source source-name
                            :contributor/email  (str/lower-case from-addr)
                            :contributor/name   (or from-name "")
                            :contributor/since  (or date-sent (java.util.Date.))}])
        (log/info "New contributor:" from-addr "on" source-name)))))

(defn get-last-run [db]
  (d/q '[:find ?t . :where [?e :digest/id "watermark"] [?e :digest/last-run ?t]] db))

(defn save-last-run! [conn ts]
  (d/transact! conn [{:digest/id "watermark" :digest/last-run ts}]))

(def email-pull-pattern
  '[:db/id :email/imap-uid :email/source :email/subject :email/message-id
    :email/in-reply-to :email/references
    :email/from-address :email/from-name :email/date-sent :email/ingested-at
    :email/body-text :email/body-text-from-html :email/headers-edn
    {:email/attachments [:attachment/filename :attachment/content-type :attachment/data]}])

(defn emails-since [db since-ts]
  (let [eids (d/q '[:find [?e ...]
                    :in $ ?since
                    :where [?e :email/ingested-at ?t] [(> ?t ?since)]]
                  db since-ts)]
    (d/pull-many db email-pull-pattern eids)))

(defn all-emails [db]
  (let [eids (d/q '[:find [?e ...]
                    :where [?e :email/message-id _]]
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
  "Link a newly created report to all existing reports it's threaded with."
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
        (d/transact! conn (mapv (fn [r] {:db/id r
                                          :report/closed release-email-eid
                                          :report/close-reason :resolved})
                                open-chgs))
        (log/info "Auto-closed" (count open-chgs)
                      "[CHG" version "] (superseded by release)")))))

(defn- parse-version-number
  "Parse \"v3\" -> 3, or nil."
  [v]
  (when v
    (when-let [[_ n] (re-find #"^v(\d+)$" v)]
      (parse-long n))))

(defn close-patch-previous-version!
  "When a [PATCH v<n> topic ...] report is created, close the nearest
  ancestor [PATCH v<n-1> topic] if it is open and has matching topic.
  When the new patch has no topic, any open v<n-1> patch among the
  nearest ancestors is closed."
  [conn report-info email-eid nearest-report-eids]
  (let [new-version (:version report-info)
        new-topic   (:topic report-info)
        n           (parse-version-number new-version)]
    (when (and n (> n 1))
      (let [prev-version (str "v" (dec n))
            db           (d/db conn)]
        (doseq [rid nearest-report-eids]
          (let [r (d/pull db [:report/type :report/version :report/topic :report/closed
                              :report/message-id] rid)]
            (when (and (= :patch (:report/type r))
                       (= prev-version (:report/version r))
                       (not (:report/closed r))
                       ;; Topic must match when both are present
                       (or (and (nil? new-topic) (nil? (:report/topic r)))
                           (and new-topic
                                (= (str/lower-case new-topic)
                                   (str/lower-case (or (:report/topic r) ""))))))
              (d/transact! conn [{:db/id rid
                                  :report/closed email-eid
                                  :report/close-reason :canceled}])
              (log/info "Auto-closed [PATCH" prev-version
                        (or (:report/topic r) "") "]"
                        (str "(" (:report/message-id r) ")")
                        "(superseded by" new-version ")"))))))))
;; ---------------------------------------------------------------------------
;; Digest orchestration
;; ---------------------------------------------------------------------------

(defn- process-email!
  "Process a single email during digest. Returns updated accumulator."
  [conn source-map sources {:keys [created threaded skipped thread-index type-index] :as acc} email]
  (let [message-id    (:email/message-id email)
        eid           (:db/id email)
        from-addr     (:email/from-address email)
        ;; Resolve source
        source-name   (or (:email/source email)
                          (classify-source (:email/headers-edn email)
                                           (:email/subject email)
                                           sources))
        _             (when (and source-name (not (:email/source email)))
                        (d/transact! conn [{:db/id eid :email/source source-name}]))
        ;; Strip [bark:<list-id>] prefix from subject if present
        email         (if-let [bark-lid (re-find #"(?i)^\[bark:[^\]]+\]\s*" (:email/subject email))]
                        (update email :email/subject #(str/replace-first % bark-lid ""))
                        email)
        source-cfg    (get source-map source-name)
        roles         (if source-name (get-roles (d/db conn) source-name) {})
        body-text     (email-body-text email)
        subj-patterns (resolve-labels (or source-cfg {}))
        allowed-types (:report-types source-cfg)]
    (if (and from-addr (ignored? roles from-addr))
      (do (log/debug "Ignored" from-addr "—" (:email/subject email))
          (assoc acc :skipped (inc skipped)))
      (do ;; Role and notify commands (not from mailing lists — list emails
       ;; have both List-Id and List-Post; a manually added List-Id alone
       ;; does not count).
       (when (and from-addr body-text source-name
                  (not (from-mailing-list? email)))
         (apply-role-commands! conn roles source-name from-addr body-text)
         (apply-notify-commands! conn roles source-name from-addr body-text))
          ;; Detect and create report
          (let [report-info (detect-report email subj-patterns allowed-types)
                permitted?  (and report-info from-addr
                                 (can-create-report? roles from-addr report-info
                                                     email source-cfg))
                new-report? (and permitted? (not (report-exists? (d/db conn) message-id)))
                [created thread-index type-index report-eid]
                (if new-report?
                  (do (log/info (str "[" (name (:type report-info)) "]") (:email/subject email))
                      (create-report! conn eid message-id report-info)
                      (ensure-contributor! conn source-name from-addr
                                           (:email/from-name email) (:email/date-sent email))
                      (when (and (= :release (:type report-info)) (:version report-info))
                        (close-changes-for-release! conn (:version report-info) eid))
                      (let [rid (d/q '[:find ?r . :in $ ?mid :where [?r :report/message-id ?mid]]
                                     (d/db conn) message-id)]
                        [(inc created)
                         (index-assoc thread-index message-id rid)
                         (assoc type-index rid (:type report-info))
                         rid]))
                  (do (when (and report-info (not permitted?))
                        (log/warn "Denied:" from-addr "cannot create" (name (:type report-info))))
                      [created thread-index type-index nil]))
                ;; Threading
                parent-report-eids (find-reports-for-email email thread-index (d/db conn))
                nearest-report-eids (find-nearest-report email thread-index (d/db conn))
                [threaded thread-index]
                (if (seq parent-report-eids)
                  (do (doseq [rid parent-report-eids]
                        (add-descendant! conn rid eid))
                      (ensure-contributor! conn source-name from-addr
                                           (:email/from-name email) (:email/date-sent email))
                      (doseq [rid nearest-report-eids]
                        (when-let [rtype (or (type-index rid)
                                             (d/q '[:find ?t . :in $ ?r
                                                    :where [?r :report/type ?t]]
                                                  (d/db conn) rid))]
                          (let [report-source (d/q '[:find ?src . :in $ ?rid
                                                     :where [?rid :report/email ?e]
                                                     [?e :email/source ?src]]
                                                   (d/db conn) rid)
                                report-roles (if report-source
                                               (get-roles (d/db conn) report-source)
                                               roles)]
                            (apply-triggers-and-directives!
                             conn rid rtype email source-map report-roles))))
                      [(+ threaded (count parent-report-eids))
                       (reduce #(index-assoc %1 message-id %2) thread-index parent-report-eids)])
                  [threaded thread-index])]
            ;; Post-creation: link related reports + manage series + store patches
            (when (and report-eid (seq parent-report-eids))
              (link-related-reports! conn report-eid parent-report-eids))
            ;; Auto-close previous patch version (v<n> supersedes v<n-1>)
            (when (and report-eid
                       (= :patch (:type report-info))
                       (:version report-info)
                       (seq nearest-report-eids))
              (close-patch-previous-version! conn report-info eid nearest-report-eids))
            (when (and report-eid (= :patch (:type report-info))
                       (:patch-seq report-info))
              (manage-series! conn report-eid eid report-info
                              from-addr parent-report-eids))
            (when (and report-eid (= :patch (:type report-info)))
              (let [patches (build-patch-entities email)]
                (when (seq patches)
                  (d/transact! conn [{:db/id report-eid
                                      :report/patches patches}])
                  (log/info (count patches) "patch file(s) stored"))))
            {:created created :threaded threaded :skipped skipped
             :thread-index thread-index :type-index type-index})))))

;; ---------------------------------------------------------------------------
;; Report expiry
;; ---------------------------------------------------------------------------

(def ^:private expirable-types #{:announcement :release :change})

;; days-between is defined in bark-common.clj

(defn expire-reports!
  "Close open reports of expirable types (announcement, release, change)
  when their age exceeds the configured :expiry delay for their source.
  Sets :report/close-reason to :expired."
  [conn source-map]
  (let [now (java.util.Date.)
        db  (d/db conn)
        ;; Find open reports of expirable types with their source and date
        candidates (d/q '[:find ?r ?type ?src ?date
                          :in $ ?types
                          :where
                          [?r :report/type ?type]
                          [(contains? ?types ?type)]
                          [?r :report/email ?e]
                          [?e :email/source ?src]
                          [?e :email/date-sent ?date]
                          (not [?r :report/closed _])]
                        db expirable-types)
        expired (reduce
                 (fn [n [rid rtype src date-sent]]
                   (let [expiry-cfg (:expiry (get source-map src))
                         delay-days (get expiry-cfg (keyword rtype))]
                     (if (and delay-days date-sent (> (days-between date-sent now) delay-days))
                       (let [report-mid (d/q '[:find ?mid . :in $ ?r
                                               :where [?r :report/message-id ?mid]]
                                             (d/db conn) rid)
                             synth-mid  (str "<bark-expired-" report-mid ">")
                             synth-eid  (or (d/q '[:find ?e . :in $ ?mid
                                                   :where [?e :email/message-id ?mid]]
                                                 (d/db conn) synth-mid)
                                            (do (d/transact! conn [{:email/message-id   synth-mid
                                                                    :email/from-address "bark-system"
                                                                    :email/date-sent    now
                                                                    :email/subject      (str "Auto-expired: " report-mid)}])
                                                (d/q '[:find ?e . :in $ ?mid
                                                       :where [?e :email/message-id ?mid]]
                                                     (d/db conn) synth-mid)))]
                         (d/transact! conn [[:db/add rid :report/closed synth-eid]
                                            [:db/add rid :report/close-reason :expired]])
                         (log/info "Expired" (name rtype) "report:" report-mid)
                         (inc n))
                       n)))
                 0 candidates)]
    (when (pos? expired)
      (log/info "Expired" expired "report(s)."))))

(defn cmd-digest! [conn source-map sources process-all?]
  (let [db       (d/db conn)
        last-run (get-last-run db)
        full?    (or process-all? (nil? last-run))
        emails   (if process-all?
                   (do (log/info "Processing ALL emails...") (all-emails db))
                   (if last-run
                     (do (log/info "Processing emails since" last-run "...")
                         (emails-since db last-run))
                     (do (log/info "First run — processing ALL emails...") (all-emails db))))
        sorted   (sort-by (fn [e] (or (:email/ingested-at e) (:email/date-sent e) (java.util.Date. 0))) emails)
        {:keys [thread-index type-index]}
        (if full?
          (do (log/info "Building full thread index...")
              (build-indexes db))
          {:thread-index {} :type-index {}})]
    (if full?
      (log/info "Found" (count sorted) "email(s) to scan. Thread index:" (count thread-index) "entries.")
      (log/info "Found" (count sorted) "email(s) to scan."))
    (let [{:keys [created threaded skipped]}
          (reduce (fn [acc email]
                    (try
                      (process-email! conn source-map sources acc email)
                      (catch Exception e
                        (log/error "Error processing" (:email/message-id email) (.getMessage e))
                        (update acc :skipped inc))))
                  {:created 0 :threaded 0 :skipped 0
                   :thread-index thread-index :type-index type-index}
                  sorted)]
      (save-last-run! conn (java.util.Date.))
      (log/info "Created" created "report(s), threaded" threaded
                    "email(s), skipped" skipped "ignored.")
      ;; --- Expiry pass ---
      (expire-reports! conn source-map))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(when (= (System/getProperty "babashka.file") *file*)
  (let [args    *command-line-args*
        all?    (some #{"--all"} args)
        db-path (or (System/getenv "BARK_DB") "data/bark-db")
        config  (load-config)
        conn    (d/get-conn db-path bark-schema {:wal? false})]
    (try
      (when config (ensure-source-roles! conn config))
      (when config
        (doseq [{:keys [name]} (:sources config)]
          (ensure-notify-defaults! conn name (get-roles (d/db conn) name))))
      (let [source-map (if config (build-source-map config) {})
            sources    (or (:sources config) [])]
        (cmd-digest! conn source-map sources all?))
      (finally
        (d/close conn)))))
