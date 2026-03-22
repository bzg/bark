#!/usr/bin/env bb

;; bark-digest.clj — Digest emails into reports.
;;
;; BARK: Bug And Report Keeper
;;
;; Orchestrates the digest pipeline:
;;   1. Classify email source
;;   2. Apply role/notify controls
;;   3. Detect report type
;;   4. Create report or thread as descendant
;;   5. Apply commands to nearest ancestor
;;   6. Manage patch series
;;
;; Detection, commands, roles, and series logic are in separate modules.
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
         bump-report-updated! bump-global-modified!
         ;; bark-roles.clj
         get-roles ignored? admin?
         ensure-source-roles! ensure-notify-defaults!
         apply-role-controls! apply-notify-controls!
         from-mailing-list? can-create-report?
         ;; bark-detect.clj
         detect-report resolve-labels build-patch-entities
         ;; bark-commands.clj
         apply-commands!
         ;; bark-series.clj
         manage-series!)

(load-file "scripts/bark-common.clj")

(load-datalevin-pod!)

(load-file "scripts/bark-roles.clj")
(load-file "scripts/bark-detect.clj")
(load-file "scripts/bark-commands.clj")
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
  "Return the report eids of the nearest ancestor only (for command application).
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
  (d/transact! conn [{:digest/id digest-watermark-id :digest/last-run ts}]))

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
  "When a [REL x] report is created, close any open [CHG x] with the same version
  and mark them as related to the release report.
  This is a particular case of relation: unlike the normal thread-based
  `link-related-reports!`, the [REL] and [CHG] emails may not be in the
  same thread — they are linked because the release logically closes the
  change announcements it ships."
  [conn version release-email-eid release-report-eid]
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
        ;; Link the release report and the closed change reports as related.
        ;; This is a cross-thread relation: the emails may not share a thread,
        ;; but the release semantically encompasses these changes.
        (let [rel-tx (into []
                           (mapcat (fn [chg-rid]
                                     [[:db/add release-report-eid :report/related chg-rid]
                                      [:db/add chg-rid :report/related release-report-eid]]))
                           open-chgs)]
          (d/transact! conn rel-tx))
        (bump-report-updated! conn open-chgs)
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
  nearest ancestors is closed.

  Design note: we iterate `nearest-report-eids` (thread ancestors)
  rather than querying the full DB for v<n-1> patches.  This scopes
  the auto-close to the same conversation thread, avoiding accidental
  closure of an unrelated patch that happens to share version+topic."
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
              (bump-report-updated! conn rid)
              (log/info "Auto-closed [PATCH" prev-version
                        (or (:report/topic r) "") "]"
                        (str "(" (:report/message-id r) ")")
                        "(superseded by" new-version ")"))))))))
;; ---------------------------------------------------------------------------
;; Digest orchestration
;; ---------------------------------------------------------------------------

(defn- source-from-in-reply-to
  "Resolve source by looking up the email referenced by In-Reply-To.
  Returns the source name if the parent email has one, nil otherwise."
  [db in-reply-to]
  (when in-reply-to
    (d/q '[:find ?src .
           :in $ ?mid
           :where [?e :email/message-id ?mid] [?e :email/source ?src]]
         db in-reply-to)))

(defn- resolve-email-source!
  "Classify the email's source. Persists source to DB if newly resolved.
  Strips [bark:list-id] prefix from subject in the returned email map
  (in-memory only — the DB retains the original subject for re-classification
  on --all re-runs).
  Returns [source-name email] where email may have an updated subject."
  [conn email sources]
  (let [eid       (:db/id email)
        mid       (:email/message-id email)
        existing  (:email/source email)
        irt-src   (when-not existing
                    (source-from-in-reply-to (d/db conn) (:email/in-reply-to email)))
        ;; Cross-check: when In-Reply-To resolved a source, also check
        ;; headers to warn on mismatches. irt-src always wins.
        _         (when irt-src
                    (let [hdr-src (classify-source (:email/headers-edn email)
                                                    (:email/subject email) sources)]
                      (when (and hdr-src (not= irt-src hdr-src))
                        (log/warn "Source mismatch for" mid
                                  "— In-Reply-To says" irt-src
                                  "but headers say" hdr-src "(using" irt-src ")"))))
        src-name  (or existing irt-src
                      (classify-source (:email/headers-edn email)
                                       (:email/subject email) sources))
        _         (when (and src-name (not existing))
                    (d/transact! conn [{:db/id eid :email/source src-name}]))
        ;; Strip [bark:<list-id>] prefix from subject (in-memory only).
        ;; Not persisted to DB: the original subject is needed for source
        ;; re-classification on --all re-runs, and persisting triggers a
        ;; Datalevin pod transit serialization issue with Indexable values.
        email     (if-let [bark-lid (re-find #"(?i)^\[bark:[^\]]+\]\s*" (:email/subject email))]
                    (update email :email/subject #(str/replace-first % bark-lid ""))
                    email)]
    [src-name email]))

(defn- try-create-report!
  "Detect report type from email, check permissions, create if new.
  Returns [created thread-index type-index report-eid]."
  [conn email eid message-id from-addr source-name source-cfg roles
   {:keys [created thread-index type-index]}]
  (let [subj-patterns (resolve-labels (or source-cfg {}))
        allowed-types (:report-types source-cfg)
        report-info   (detect-report email subj-patterns allowed-types)
        permitted?    (and report-info from-addr
                           (can-create-report? roles from-addr report-info
                                               email source-cfg))
        new-report?   (and permitted? (not (report-exists? (d/db conn) message-id)))]
    (if new-report?
      (do (log/info (str "[" (name (:type report-info)) "]") (:email/subject email))
          (create-report! conn eid message-id report-info)
          (ensure-contributor! conn source-name from-addr
                               (:email/from-name email) (:email/date-sent email))
          (let [rid (d/q '[:find ?r . :in $ ?mid :where [?r :report/message-id ?mid]]
                         (d/db conn) message-id)]
            (bump-report-updated! conn rid)
            {:created      (inc created)
             :thread-index (index-assoc thread-index message-id rid)
             :type-index   (assoc type-index rid (:type report-info))
             :report-eid   rid
             :report-info  report-info}))
      (do (when (and report-info (not permitted?))
            (log/warn "Denied:" from-addr "cannot create" (name (:type report-info))))
          {:created created :thread-index thread-index :type-index type-index
           :report-eid nil :report-info report-info}))))

(defn- thread-descendants!
  "Link email as descendant of ancestor reports, apply commands to nearest.
  Returns [threaded thread-index]."
  [conn email eid message-id from-addr source-name roles source-map
   {:keys [threaded thread-index type-index]}]
  (let [parent-eids  (find-reports-for-email email thread-index (d/db conn))
        nearest-eids (find-nearest-report email thread-index (d/db conn))]
    (if (seq parent-eids)
      (do (doseq [rid parent-eids]
            (add-descendant! conn rid eid))
          (ensure-contributor! conn source-name from-addr
                               (:email/from-name email) (:email/date-sent email))
          (doseq [rid nearest-eids]
            (when-let [rtype (or (type-index rid)
                                 (d/q '[:find ?t . :in $ ?r
                                        :where [?r :report/type ?t]]
                                      (d/db conn) rid))]
              (let [rsrc  (d/q '[:find ?src . :in $ ?rid
                                 :where [?rid :report/email ?e]
                                 [?e :email/source ?src]]
                               (d/db conn) rid)
                    rroles (if rsrc (get-roles (d/db conn) rsrc) roles)]
                (apply-commands! conn rid rtype email source-map rroles))))
          (bump-report-updated! conn parent-eids)
          {:threaded     (+ threaded (count parent-eids))
           :thread-index (reduce #(index-assoc %1 message-id %2)
                                 thread-index parent-eids)
           :parent-eids  parent-eids
           :nearest-eids nearest-eids})
      {:threaded threaded :thread-index thread-index
       :parent-eids #{} :nearest-eids nil})))

(defn- post-creation-hooks!
  "Run hooks after report creation: related links, auto-close, series, patches."
  [conn email eid report-eid report-info from-addr parent-eids nearest-eids]
  (when report-eid
    (when (seq parent-eids)
      (link-related-reports! conn report-eid parent-eids))
    (let [rtype (:type report-info)]
      ;; Auto-close [CHG x] when [REL x] is created
      (when (and (= :release rtype) (:version report-info))
        (close-changes-for-release! conn (:version report-info) eid report-eid))
      ;; Auto-close previous patch version (v<n> supersedes v<n-1>)
      (when (and (= :patch rtype) (:version report-info) (seq nearest-eids))
        (close-patch-previous-version! conn report-info eid nearest-eids))
      ;; Series management
      (when (and (= :patch rtype) (:patch-seq report-info))
        (manage-series! conn report-eid eid report-info from-addr parent-eids))
      ;; Store patch content
      (when (= :patch rtype)
        (let [patches (build-patch-entities email)]
          (when (seq patches)
            (d/transact! conn [{:db/id report-eid :report/patches patches}])
            (log/info (count patches) "patch file(s) stored")))))))

(defn- process-email!
  "Process a single email during digest. Returns updated accumulator."
  [conn source-map sources {:keys [created threaded skipped thread-index type-index] :as acc} email]
  (let [message-id (:email/message-id email)
        eid        (:db/id email)
        from-addr  (:email/from-address email)
        ;; 1. Classify source
        [source-name email] (resolve-email-source! conn email sources)
        source-cfg (get source-map source-name)
        roles      (if source-name (get-roles (d/db conn) source-name) {})]
    ;; 2. Check ignored
    (if (and from-addr (ignored? roles from-addr))
      (do (log/debug "Ignored" from-addr "—" (:email/subject email))
          (assoc acc :skipped (inc skipped)))
      (do ;; 3. Role & notify controls (blocked on mailing list emails)
          (let [body-text (email-body-text email)]
            (when (and from-addr body-text source-name
                       (not (from-mailing-list? email)))
              (apply-role-controls! conn roles source-name from-addr
                                    body-text (:email/date-sent email))
              (apply-notify-controls! conn roles source-name from-addr body-text)))
          ;; 4. Detect & create report
          (let [{:keys [report-eid report-info] r-created :created
                 r-ti :thread-index r-tyi :type-index}
                (try-create-report! conn email eid message-id from-addr
                                    source-name source-cfg roles acc)
                ;; 5. Thread descendants
                {:keys [parent-eids nearest-eids] t-threaded :threaded
                 t-ti :thread-index}
                (thread-descendants! conn email eid message-id from-addr
                                     source-name roles source-map
                                     {:threaded threaded :thread-index r-ti
                                      :type-index r-tyi})]
            ;; 6. Post-creation hooks
            (post-creation-hooks! conn email eid report-eid report-info
                                  from-addr parent-eids nearest-eids)
            {:created r-created :threaded t-threaded :skipped skipped
             :thread-index t-ti :type-index r-tyi})))))

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
                                                                    :email/source       src
                                                                    :email/date-sent    now
                                                                    :email/subject      (str "Auto-expired: " report-mid)}])
                                                (d/q '[:find ?e . :in $ ?mid
                                                       :where [?e :email/message-id ?mid]]
                                                     (d/db conn) synth-mid)))]
                         (d/transact! conn [[:db/add rid :report/closed synth-eid]
                                            [:db/add rid :report/close-reason :expired]])
                         (bump-report-updated! conn rid)
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
