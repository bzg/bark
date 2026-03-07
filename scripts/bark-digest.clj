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
         '[clojure.edn :as edn])

(load-file "scripts/bark-common.clj")

(load-datalevin-pod!)

(load-file "scripts/bark-roles.clj")
(load-file "scripts/bark-detect.clj")
(load-file "scripts/bark-triggers.clj")
(load-file "scripts/bark-series.clj")

;; ---------------------------------------------------------------------------
;; Schema — loaded from shared bark-schema.edn
;; ---------------------------------------------------------------------------

(def report-schema
  (edn/read-string (slurp "resources/bark-schema.edn")))

;; ---------------------------------------------------------------------------
;; Threading
;; ---------------------------------------------------------------------------

(defn ancestor-mids
  "Return an ordered vector of ancestor message-ids, nearest last.
  Built from References (which is ordered root→parent per RFC 2822)
  plus In-Reply-To.  Duplicates are removed, order preserved."
  [email]
  (let [refs (:email/references email)
        refs (cond (nil? refs)    []
                   (string? refs) [refs]
                   (coll? refs)   (vec refs)
                   :else          [])
        irt  (:email/in-reply-to email)
        all  (if (and irt (not (some #{irt} refs)))
               (conj refs irt)
               refs)]
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

(defn get-last-run [db]
  (d/q '[:find ?t . :where [?e :digest/id "watermark"] [?e :digest/last-run ?t]] db))

(defn save-last-run! [conn ts]
  (d/transact! conn [{:digest/id "watermark" :digest/last-run ts}]))

(def email-pull-pattern
  '[:db/id :email/imap-uid :email/source :email/subject :email/message-id
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
        (d/transact! conn (mapv (fn [r] {:db/id r :report/closed release-email-eid}) open-chgs))
        (println (str "    → auto-closed " (count open-chgs)
                      " [CHG " version "] (superseded by release)"))))))

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
                          (classify-source (:email/headers-edn email) sources))
        _             (when (and source-name (not (:email/source email)))
                        (d/transact! conn [{:db/id eid :email/source source-name}]))
        source-cfg    (get source-map source-name)
        roles         (if source-name (get-roles (d/db conn) source-name) {})
        body-text     (or (:email/body-text email) (:email/body-text-from-html email))
        subj-patterns (resolve-labels (or source-cfg {}))]
    (if (and from-addr (ignored? roles from-addr))
      (do (println (str "  [ignored] " from-addr " — " (:email/subject email)))
          (assoc acc :skipped (inc skipped)))
      (do ;; Role and notify commands (only for direct emails, not mailing list)
          (when (and from-addr body-text source-name
                     (not (from-mailing-list-email? email)))
            (apply-role-commands! conn roles source-name from-addr body-text)
            (apply-notify-commands! conn roles source-name from-addr body-text))
          ;; Detect and create report
          (let [report-info (detect-report email subj-patterns)
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
                ;; Threading
                parent-report-eids (find-reports-for-email email thread-index (d/db conn))
                nearest-report-eids (find-nearest-report email thread-index (d/db conn))
                [threaded thread-index]
                (if (seq parent-report-eids)
                  (do (doseq [rid parent-report-eids]
                        (add-descendant! conn rid eid))
                      (doseq [rid nearest-report-eids]
                        (when-let [rtype (or (type-index rid)
                                             (d/q '[:find ?t . :in $ ?r
                                                    :where [?r :report/type ?t]]
                                                  (d/db conn) rid))]
                          (apply-triggers! conn rid rtype email source-map)))
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
        full?    (or process-all? (nil? last-run))
        emails   (if process-all?
                   (do (println "Processing ALL emails...") (all-emails db))
                   (if last-run
                     (do (println (str "Processing emails since " last-run "..."))
                         (emails-since db last-run))
                     (do (println "First run — processing ALL emails...") (all-emails db))))
        sorted   (sort-by (fn [e] (or (:email/ingested-at e) (:email/date-sent e) (java.util.Date. 0))) emails)
        {:keys [thread-index type-index]}
        (if full?
          (do (println "Building full thread index...")
              (build-indexes db))
          {:thread-index {} :type-index {}})]
    (println (str "Found " (count sorted) " email(s) to scan."
                  (when full? (str " Thread index: " (count thread-index) " entries."))))
    (let [{:keys [created threaded skipped]}
          (reduce (fn [acc email]
                    (try
                      (process-email! conn source-map sources acc email)
                      (catch Exception e
                        (println (str "  [error] " (:email/message-id email) ": " (.getMessage e)))
                        (update acc :skipped inc))))
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
        conn    (d/get-conn db-path report-schema {:wal? false})]
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
