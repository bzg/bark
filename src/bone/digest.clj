;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.digest
  "Single-email digest orchestration.
  Processes one email at a time: source classification, report detection,
  threading, command application, and series management.
  Called by bone.main/store-and-process! after each email is stored."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [taoensso.timbre :as log]
            [bone.common :as common]
            [bone.lookup :as lookup]
            [bone.tracking :as tracking]
            [bone.detect :as detect]
            [bone.commands :as commands]
            [bone.periods :as periods]
            [bone.relations :as rel]
            [bone.roles :as roles]
            [bone.series :as series])
  (:import [java.util Date]))

;; ---------------------------------------------------------------------------
;; Email pull pattern (for re-loading a stored email)
;; ---------------------------------------------------------------------------

(def email-pull-pattern
  '[:db/id :email/id :email/source :email/subject :email/message-id
    :email/in-reply-to :email/references :email/ancestor-mid-hashes
    :email/pending-thread?
    :email/author-address :email/author-name
    :email/from-address :email/from-name
    :email/date-sent :email/ingested-at
    :email/digested-at
    :email/body-text :email/body-text-from-html :email/headers-edn
    {:email/attachments [:attachment/filename :attachment/content-type :attachment/data]}])

;; ---------------------------------------------------------------------------
;; Threading
;; ---------------------------------------------------------------------------

(defn ancestor-mids
  "Ordered ancestor mids (root first, parent last) recomputed from
  :email/references + :email/in-reply-to.  The cardinality/many
  :email/ancestor-mid-hashes attr loses ordering and is only used for
  the unordered lookup in `retry-pending-in-shared-thread!`."
  [email]
  (common/ancestor-mids-from (:email/references email)
                              (:email/in-reply-to email)))

(defn- reports-by-hash
  "Report eids whose root or any descendant has the mid-hash `h`.
  Resolution via bone.lookup, descendant join eid-bound (see the
  bone.lookup ns docstring)."
  [db h]
  (let [as-root (lookup/report-eid-by-hash db h)
        email-e (lookup/email-eid-by-hash db h)
        as-desc (when email-e
                  (d/q '[:find [?r ...] :in $ ?e :where [?r :report/descendants ?e]]
                       db email-e))]
    (cond-> (set as-desc) as-root (conj as-root))))

(defn- email-ancestors
  "Ancestor mids of the stored email `eid` (root first).  Used by
  `thread-lookup` and `nearest-root-report` to splice through
  stored-but-pending intermediates."
  [db eid]
  (let [pulled (d/pull db [:email/references :email/in-reply-to] eid)]
    (common/ancestor-mids-from (:email/references pulled)
                               (:email/in-reply-to pulled))))

(def ^:private thread-lookup-max-splices
  "Upper bound on transitive ancestor splicing per `thread-lookup` call."
  32)

(defn thread-lookup
  "Walk `email`'s ancestor mids nearest-first.  Returns
  `{:all #{eids} :nearest #{eids}}` -- :all is every report matched
  by any ancestor, :nearest is the closest match (nil if none).
  When an ancestor mid matches a stored email with no report, that
  email's own ancestors are spliced into the walk (bounded by
  `thread-lookup-max-splices`) so pending intermediates don't orphan
  descendants."
  [email db]
  (loop [stack   (vec (ancestor-mids email))  ; root-first; peek = nearest
         seen    #{}
         splices 0
         acc     {:all #{} :nearest nil}]
    (if (empty? stack)
      acc
      (let [mid    (peek stack)
            stack' (pop stack)]
        (if (contains? seen mid)
          (recur stack' seen splices acc)
          (let [seen' (conj seen mid)
                h     (common/mid-hash mid)
                eids  (reports-by-hash db h)
                acc'  (cond-> acc
                        (seq eids)                              (update :all into eids)
                        (and (seq eids) (nil? (:nearest acc))) (assoc :nearest eids))]
            (if (and (empty? eids)
                     (< splices thread-lookup-max-splices))
              (if-let [ancestors (some->> (lookup/email-eid-by-hash db h)
                                          (email-ancestors db))]
                (recur (into stack' ancestors) seen' (inc splices) acc')
                (recur stack' seen' splices acc'))
              (recur stack' seen' splices acc'))))))))

;; ---------------------------------------------------------------------------
;; DB operations
;; ---------------------------------------------------------------------------

(defn- ensure-participant!
  "Record a participant on `source-name`; with `:contributor? true`,
  stamp :participant/contributor-since on first patch (idempotent)."
  [conn source-name from-addr from-name date-sent & {:keys [contributor?]}]
  (when (and source-name from-addr)
    (let [k     (str (common/slugify source-name) ":" (str/lower-case from-addr))
          db    (d/db conn)
          e     (d/entid db [:participant/key k])
          since (or date-sent (Date.))]
      (cond
        ;; Already a participant -- only act if we need to stamp contributor-since.
        e
        (when (and contributor?
                   (not (d/q '[:find ?d . :in $ ?e
                               :where [?e :participant/contributor-since ?d]] db e)))
          (d/transact! conn [{:db/id e :participant/contributor-since since}])
          (log/info "Participant promoted to contributor:" from-addr "on" source-name))

        :else
        (let [payload (cond-> {:participant/key    k
                               :participant/source source-name
                               :participant/email  (str/lower-case from-addr)
                               :participant/name   (or from-name "")
                               :participant/since  since}
                        contributor? (assoc :participant/contributor-since since))]
          (d/transact! conn [payload])
          (if contributor?
            (log/info "New participant:" from-addr "on" source-name "(contributor)")
            (log/info "New participant:" from-addr "on" source-name)))))))

(defn- report-exists? [db message-id]
  (some? (lookup/report-eid db message-id)))

(defn report-entity
  "Build the entity map for a new report from email data.
  Patches are stored here, at creation: they depend only on the email's
  own content, so a reply flagged :email/pending-thread? (Phase 4
  skipped) still surfaces them immediately instead of waiting for the
  TTL flush."
  [email-eid message-id report-info email-date email now]
  (let [attachments (:email/attachments email)
        body-text   (common/email-body-text email)
        ;; ICS is only ever exported for announcements (see bone-export
        ;; dump-events*), so the flag stays false elsewhere rather than
        ;; recording ICS we will never publish.
        has-ics     (and (= :announcement (:type report-info))
                         (or (common/has-ics-attachment? attachments)
                             (common/has-inline-ics? body-text)))
        has-text    (boolean (some common/text-attachment? attachments))
        patches     (detect/build-patch-entities email)]
    (into {:report/type (:type report-info) :report/email email-eid
           :report/message-id message-id
           :report/message-id-hash (common/mid-hash message-id)
           :report/last-activity (or email-date now)}
          (remove (comp nil? val))
          {:report/created-at now
           :report/last-activity-address (:email/author-address email)
           :report/version (:version report-info)
           :report/topic (when (:topic report-info) email-eid)
           :report/topic-value (:topic report-info)
           :report/patch-seq (:patch-seq report-info) :report/patch-source (:patch-source report-info)
           :report/has-ics has-ics :report/has-text-attachments has-text
           :report/patches (when (seq patches) patches)})))

(defn- create-report!
  "Create a new report entity. Returns the entity id of the new report."
  [conn email-eid message-id report-info email-date email]
  (d/transact! conn [(report-entity email-eid message-id report-info email-date email (Date.))])
  (lookup/report-eid (d/db conn) message-id))

(defn descendant-tx
  "Tx-data to add an email as descendant of a report, bumping
  :report/last-activity when `email-date` is newer than `current-activity`."
  [report-eid email-eid email-date from-address current-activity]
  (let [tx [[:db/add report-eid :report/descendants email-eid]]]
    (if email-date
      (if (or (nil? current-activity) (not (.before ^Date email-date ^Date current-activity)))
        (-> tx
            (conj [:db/add report-eid :report/last-activity email-date])
            (cond-> from-address
              (conj [:db/add report-eid :report/last-activity-address from-address])))
        tx)
      tx)))

(defn- add-descendant! [conn report-eid email-eid email-date from-address]
  (let [current (:report/last-activity
                  (d/pull (d/db conn) [:report/last-activity] report-eid))]
    (d/transact! conn (descendant-tx report-eid email-eid email-date from-address current))))

(defn- link-rel!
  "Pose qualified relations between the new report and its threaded
  parents: :related-to to every parent, plus :resolves when the new
  report is a patch and the parent a bug/request."
  [conn new-report-eid new-report-type email parent-report-eids]
  (when (seq parent-report-eids)
    (let [db      (d/db conn)
          parents (d/q '[:find ?r ?t :in $ [?r ...]
                         :where [?r :report/type ?t]]
                       db (vec parent-report-eids))
          pose!   (fn [parent-eid kind]
                    (rel/pose-from-email! conn email {:from-eid new-report-eid
                                                      :to-eid   parent-eid
                                                      :kind     kind}))]
      (doseq [[parent-eid parent-type] parents]
        ;; :related-to (neutral, all type combinations)
        (when (rel/valid-pose? :related-to new-report-eid parent-eid
                               new-report-type parent-type)
          (pose! parent-eid :related-to))
        ;; :resolves (patch -> bug/request only)
        (when (and (= :patch new-report-type)
                   (rel/valid-pose? :resolves new-report-eid parent-eid
                                    new-report-type parent-type))
          (pose! parent-eid :resolves))))))

;; ---------------------------------------------------------------------------
;; Auto-close logic
;; ---------------------------------------------------------------------------

(defn- close-changes-for-release! [conn version release-email release-report-eid]
  (when (and version (not (str/blank? version)))
    (let [db      (d/db conn)
          open-chgs (d/q '[:find [?r ...] :in $ ?ver
                           :where
                           [?r :report/type :change] [?r :report/version ?ver]
                           (not [?r :report/closed _])]
                         db version)
          release-email-eid (:db/id release-email)]
      (when (seq open-chgs)
        (let [close-tx (mapv (fn [r] {:db/id r
                                      :report/closed release-email-eid
                                      :report/close-reason :resolved})
                             open-chgs)]
          (d/transact! conn close-tx))
        (doseq [chg-rid open-chgs]
          (rel/pose-from-email! conn release-email
                                {:from-eid release-report-eid
                                 :to-eid   chg-rid
                                 :kind     :related-to}))
        (tracking/bump-report-updated! conn open-chgs)
        (log/info "Auto-closed" (count open-chgs)
                  "[CHG" version "] (superseded by release)")))))

(defn- parse-version-number [v]
  (when v (when-let [[_ n] (re-find #"^v(\d+)$" v)] (parse-long n))))

(defn- auto-supersede-patch!
  "Close `old-rid` as :superseded by `new-report-eid`, pose the
  :supersedes + :related-to relations, propagate auto-credit transfers."
  [conn old-rid new-report-eid email log-msg]
  (let [email-eid (:db/id email)
        endpoints {:from-eid old-rid :to-eid new-report-eid}]
    (d/transact! conn [{:db/id old-rid
                        :report/closed email-eid
                        :report/close-reason :superseded}])
    (rel/pose-from-email! conn email (assoc endpoints :kind :supersedes))
    (rel/pose-from-email! conn email (assoc endpoints :kind :related-to))
    (rel/propagate-patch-closure! conn old-rid :patch email-eid
                                  :superseded new-report-eid)
    (tracking/bump-report-updated! conn old-rid)
    (log/info "Auto-closed patch" log-msg)))

(defn- close-patch-previous-version! [conn report-eid report-info email nearest-report-eids]
  (let [new-version (:version report-info)
        new-topic   (:topic report-info)
        n           (parse-version-number new-version)]
    (when (and n (>= n 1))
      (let [versions-to-close (cond-> #{new-version}
                                (> n 1) (conj (str "v" (dec n))))
            new-topic-lc      (some-> new-topic str/lower-case)
            ;; Single snapshot is safe: the daemon is single-threaded
            ;; on this section.  Restore per-iteration refresh if that
            ;; invariant breaks.
            db                (d/db conn)
            candidates        (keep
                               (fn [rid]
                                 (let [r (d/pull db
                                                 [:report/type :report/version
                                                  :report/topic-value :report/closed
                                                  :report/message-id]
                                                 rid)]
                                   (when (and (= :patch (:report/type r))
                                              (contains? versions-to-close
                                                         (:report/version r))
                                              (not (:report/closed r))
                                              (= new-topic-lc
                                                 (some-> (:report/topic-value r)
                                                         str/lower-case)))
                                     [rid r])))
                               nearest-report-eids)]
        (doseq [[rid r] candidates]
          (auto-supersede-patch!
           conn rid report-eid email
           (str "[PATCH " (:report/version r)
                (when-let [t (:report/topic-value r)] (str " " t)) "] "
                "(" (:report/message-id r) ") "
                "(superseded by " new-version ")")))))))

(defn- normalize-subject
  "Strip Re:/Fwd: prefixes and bracketed tags to get the base subject."
  [subject]
  (when subject
    (-> subject
        (str/replace #"(?i)^(\s*(Re|Fwd)\s*:\s*)+" "")
        (str/replace #"\[[^\]]*\]\s*" "")
        str/trim
        str/lower-case)))

(defn- close-superseded-thread-patches!
  "Close open ancestor patch reports sharing the new patch's base
  subject (Re:/[TAG] stripped).  Handles unnumbered re-sends."
  [conn report-eid email nearest-report-eids]
  (let [new-subj (normalize-subject (:email/subject email))
        db       (d/db conn)]
    (when (and new-subj (seq nearest-report-eids))
      (doseq [rid nearest-report-eids
              :when (not= rid report-eid)]
        (let [r (d/pull db [:report/type :report/closed :report/message-id
                            {:report/email [:email/subject]}] rid)]
          (when (and (= :patch (:report/type r))
                     (not (:report/closed r))
                     (= new-subj (normalize-subject (get-in r [:report/email :email/subject]))))
            (auto-supersede-patch!
             conn rid report-eid email
             (str (:report/message-id r) " (superseded by same-subject thread patch)"))))))))

;; ---------------------------------------------------------------------------
;; Source resolution
;; ---------------------------------------------------------------------------

(defn- source-from-in-reply-to
  "Source of the email referenced by `in-reply-to`, or nil."
  [db in-reply-to]
  (when-let [e (lookup/email-eid db in-reply-to)]
    (:email/source (d/pull db [:email/source] e))))

(defn- classify-email-source
  "Shared source classification logic. Works on any headers (raw map or edn string).
  Returns {:delivery :src-name :irt-src :hdr-src}."
  [db sources headers in-reply-to]
  (let [irt-src (source-from-in-reply-to db in-reply-to)
        hdr-src (common/classify-source headers sources)]
    {:delivery (common/classify-delivery headers)
     :src-name (or irt-src hdr-src)
     :irt-src  irt-src
     :hdr-src  hdr-src}))

(defn pre-classify-source
  "Pre-storage source classification on a raw mailseq msg.  Returns
  `{:src-name :delivery}`; nil `:src-name` means do not store."
  [db sources msg]
  (let [headers (:headers msg)
        irt     (common/extract-in-reply-to headers)
        {:keys [src-name delivery]} (classify-email-source db sources headers irt)]
    {:src-name src-name :delivery delivery}))

(defn- resolve-email-source!
  "Resolve [src-name email delivery] for processing.  `resolved` (from
  `pre-classify-source`, optional) short-circuits both the DB read and
  re-classification.  Else: read back a stored :email/source, or -- for
  test emails with no source -- classify headers and persist it."
  [conn email sources resolved]
  (let [eid      (:db/id email)
        mid      (:email/message-id email)
        hdrs     (:email/headers-edn email)
        existing (:email/source email)]
    (cond
      resolved
      [(:src-name resolved) email (:delivery resolved)]

      existing
      [existing email (common/classify-delivery hdrs)]

      :else
      (let [irt (:email/in-reply-to email)
            {:keys [delivery src-name irt-src hdr-src]}
            (classify-email-source (d/db conn) sources hdrs irt)]
        (when (and irt-src hdr-src (not= irt-src hdr-src))
          (log/warn "Source mismatch for" mid
                    "-- In-Reply-To says" irt-src
                    "but headers say" hdr-src (str "(using " irt-src ")")))
        (when src-name
          (d/transact! conn [{:db/id eid :email/source src-name}]))
        [src-name email delivery]))))

;; ---------------------------------------------------------------------------
;; Single-email processing -- pure decisions
;; ---------------------------------------------------------------------------

(defn creation-decision
  "Decide whether to create a report.  Returns :create, :denied-channel,
  :denied-role, or nil (no report detected or already exists)."
  [report-info from-addr via-channel? rroles email source-cfg already-exists?]
  (when (and report-info (not already-exists?))
    (let [permitted? (and from-addr via-channel?
                         (roles/can-create-report? rroles from-addr report-info
                                                   email source-cfg))]
      (cond
        permitted?          :create
        (not via-channel?)  :denied-channel
        :else               :denied-role))))

(defn post-creation-plan
  "Given report-info and context, return a set of post-creation
  action keywords to execute."
  [report-info nearest-eids parent-eids patches]
  (let [rtype (:type report-info)]
    (cond-> #{}
      (seq parent-eids)                                       (conj :link-related)
      (and (= :release rtype) (:version report-info))        (conj :close-changes)
      (and (= :patch rtype) (:version report-info)
           (seq nearest-eids))                                (conj :close-previous-version)
      (and (= :patch rtype) (seq nearest-eids))               (conj :close-superseded-thread)
      (and (= :patch rtype) (:patch-seq report-info))         (conj :manage-series)
      ;; Patches are normally stored at creation (see report-entity);
      ;; this hook only heals reports created pending by versions that
      ;; predate creation-time storage.  It applies to ANY report type
      ;; carrying patch content, not just :patch reports.
      (seq patches)                                           (conj :store-patches)
      ;; :auto-series stays patch-only: a synthetic series only makes
      ;; sense for :patch reports (series tracking is patch-specific).
      (and (= :patch rtype) (seq patches)
           (nil? (:patch-seq report-info))
           (> (count patches) 1))                             (conj :auto-series))))

;; ---------------------------------------------------------------------------
;; Single-email processing -- effectful phases
;; ---------------------------------------------------------------------------

(defn- apply-controls!
  "Apply role controls from the email body."
  [conn rroles source-name source-cfg from-addr email via-channel?]
  (let [body-text (common/email-body-text email)
        src-cmds  (commands/build-source-commands source-cfg)
        strict?   (:strict-syntax? src-cmds)]
    (when (and from-addr body-text source-name via-channel?)
      (roles/apply-role-controls! conn rroles source-name from-addr
                                  body-text (:email/date-sent email) strict?))))

(defn- record-creation-denial!
  "Record a denied report-creation attempt.  :report-mid is empty
  (the report wasn't created) so notifications render the subject.
  Audience :maintainers so the lead sees the attempt."
  [source-name from-addr email report-info reason]
  (when (and from-addr source-name)
    (commands/record-failure!
     {:source     source-name
      :from-addr  from-addr
      :email-date (:email/date-sent email)
      :report-mid ""
      :reason     :insufficient-scope
      :audience   :maintainers
      :command    (str "Create " (name (:type report-info))
                       " -- " (name reason)
                       " (subject: " (:email/subject email) ")")})))

(def ^:private denial-reason-labels
  {:denied-channel "not via source channel"
   :denied-role    "not maintainer"})

(defn- maybe-create-report!
  "Detect report type, check permissions, create if allowed.
  Returns [report-eid report-info] or [nil report-info]."
  [conn eid message-id email from-addr source-name source-cfg via-channel? rroles]
  (let [subj-patterns (detect/resolve-labels (or source-cfg {}))
        allowed-types (:report-types source-cfg)
        report-info   (detect/detect-report email subj-patterns allowed-types)
        decision      (creation-decision report-info from-addr via-channel? rroles email
                                         source-cfg (report-exists? (d/db conn) message-id))]
    (case decision
      :create
      (do (log/info (str "[" (name (:type report-info)) "]") (:email/subject email))
          (let [rid (create-report! conn eid message-id report-info
                                    (:email/date-sent email) email)]
            (ensure-participant! conn source-name from-addr
                                 (:email/author-name email) (:email/date-sent email)
                                 :contributor? (= :patch (:type report-info)))
            ;; create-report! stamps :report/created-at, which the cron
            ;; notification uses to report this as a new addition rather than
            ;; a modification -- so bump updated-at only (to drive re-export),
            ;; not state-changed-at.
            (tracking/bump-report-updated! conn rid false)
            [rid report-info]))

      (:denied-channel :denied-role)
      (do (log/warn "Denied:" from-addr "cannot create" (name (:type report-info))
                    (str "(" (denial-reason-labels decision) ")"))
          (record-creation-denial! source-name from-addr email report-info decision)
          [nil report-info])

      ;; nil -- no report detected
      [nil report-info])))

(defn- attach-as-descendant!
  "Record the email under its threaded parents and bump their
  updated-at timestamp.  Threading is independent from command
  dispatch -- it happens whenever the email is a reply, regardless
  of whether the email also created a report."
  [conn eid email from-addr parent-eids]
  (doseq [rid parent-eids]
    (add-descendant! conn rid eid (:email/date-sent email) from-addr))
  ;; A threaded reply grows the report's context but does not change the
  ;; report's own state -- bump updated-at (for re-export) only, not
  ;; state-changed-at.
  (tracking/bump-report-updated! conn parent-eids false))

(defn- rids->type+source
  "Pull `[report-type source-name]` for each report eid.  Returns a
  map {rid [type src]}; rids with no report-type are absent."
  [db rids]
  (reduce (fn [m [r t s]] (assoc m r [t s]))
          {}
          (d/q '[:find ?r ?t ?src
                 :in $ [?r ...]
                 :where
                 [?r :report/type ?t]
                 [?r :report/email ?e]
                 [?e :email/source ?src]]
               db rids)))

(defn- cover-letter-patches
  "If `rid` is the cover-letter report of a series, return the other
  patch report eids of that series (the cover excluded).  Returns
  nil otherwise.  A report is a cover iff its email is the series'
  `:series/cover-letter`."
  [db rid]
  (seq (d/q '[:find [?r ...]
              :in $ ?rid
              :where
              [?rid :report/series ?s]
              [?rid :report/email ?cover-email]
              [?s :series/cover-letter ?cover-email]
              [?s :series/patches ?r]
              [(not= ?r ?rid)]]
            db rid)))

(defn- roles-by-source
  "Tenures per source for the sources appearing in `info` (a map of
  rid -> [type source]).  Reports from another source must be checked
  against that source's roles, not the current email's."
  [db info]
  (into {}
        (map (fn [s] [s (roles/get-tenures db s)]))
        (into #{} (keep (fn [[_ [_t s]]] s)) info)))

(defn- broadcast-cover-commands!
  "When `rid` is a series cover letter, apply the email's commands to
  every patch of the series (filter :no-cross-refs: broadcasting
  Supersedes:/Related-to: would pose N redundant edges; triggers
  propagate so the series shares the cover's state)."
  [conn email source-map rroles delivery rid]
  (when-let [patches (cover-letter-patches (d/db conn) rid)]
    (let [db         (d/db conn)
          info       (rids->type+source db patches)
          src->roles (roles-by-source db info)]
      (doseq [patch-rid patches
              :let [[ptype psrc] (get info patch-rid)]
              :when ptype]
        (let [proles (or (get src->roles psrc) rroles)]
          (commands/apply-commands! conn patch-rid ptype email source-map
                                    proles delivery :no-cross-refs))))))

(defn- apply-commands-on-nearest!
  "Apply commands to the nearest reports of a reply (roles refreshed
  per source), record the author as participant on any match, and
  broadcast cover-letter commands to their series.  `line-filter` is
  forwarded to `apply-commands!`."
  [conn email from-addr source-name rroles source-map delivery nearest-eids line-filter]
  (let [db       (d/db conn)
        info     (rids->type+source db nearest-eids)
        src->roles (roles-by-source db info)
        any-cmd? (reduce (fn [acc rid]
                           (if-let [[rtype rsrc] (get info rid)]
                             (let [r (or (get src->roles rsrc) rroles)]
                               (if (commands/apply-commands! conn rid rtype email source-map
                                                             r delivery line-filter)
                                 true acc))
                             acc))
                         false nearest-eids)]
    (when any-cmd?
      (ensure-participant! conn source-name from-addr
                           (:email/author-name email) (:email/date-sent email)))
    (doseq [rid nearest-eids]
      (broadcast-cover-commands! conn email source-map rroles delivery rid))))

(defn- nearest-root-report
  "Walk `email`'s ancestor mids nearest-first; eid of the first one
  that is a report's own message-id, or nil.  Unlike thread-lookup's
  :nearest, never resolves to reports whose thread the reply merely
  crosses (e.g. the cover letter of a patch).  Ancestors of
  stored-but-reportless intermediates are spliced in (bounded)."
  [db email]
  (loop [stack   (vec (ancestor-mids email))  ; root-first; peek = nearest
         seen    #{}
         splices 0]
    (when-let [mid (peek stack)]
      (let [stack' (pop stack)]
        (if (contains? seen mid)
          (recur stack' seen splices)
          (let [h (common/mid-hash mid)]
            (or (lookup/report-eid-by-hash db h)
                (let [ancestors (when (< splices thread-lookup-max-splices)
                                  (some->> (lookup/email-eid-by-hash db h)
                                           (email-ancestors db)))]
                  (recur (into stack' ancestors) (conj seen mid)
                         (if ancestors (inc splices) splices))))))))))

(defn- collect-trailers!
  "Store the git person trailers of a pure reply on the patch report
  it replies to, broadcasting from a cover letter to the series
  patches (b4 semantics).  Report-creating emails are excluded (a
  patch must not leak its own Signed-off-by upthread).  Set
  semantics: duplicates and replays collapse."
  [conn email]
  (when-let [trailers (seq (common/extract-trailers (common/email-body-text email)))]
    (let [db     (d/db conn)
          target (nearest-root-report db email)
          rids   (when target (into [target] (cover-letter-patches db target)))
          patch-rids (when (seq rids)
                       (d/q '[:find [?r ...] :in $ [?r ...]
                              :where [?r :report/type :patch]]
                            db rids))]
      (when (seq patch-rids)
        (d/transact! conn (vec (for [rid patch-rids, tr trailers]
                                 [:db/add rid :report/trailers tr])))
        (tracking/bump-report-updated! conn patch-rids false)
        (log/info "Collected" (count trailers) "trailer(s) on"
                  (count patch-rids) "patch report(s)")))))

(defn- run-post-creation-hooks!
  "Execute post-creation side effects driven by the plan."
  [conn report-eid eid email from-addr report-info
   parent-eids nearest-eids patches plan]
  (when (:link-related plan)
    (link-rel! conn report-eid (:type report-info) email parent-eids))
  (when (:close-changes plan)
    (close-changes-for-release! conn (:version report-info) email report-eid))
  (when (:close-previous-version plan)
    (close-patch-previous-version! conn report-eid report-info email nearest-eids))
  (when (:close-superseded-thread plan)
    (close-superseded-thread-patches! conn report-eid email nearest-eids))
  (when (:manage-series plan)
    (series/manage-series! conn report-eid email report-info from-addr parent-eids))
  (when (:store-patches plan)
    ;; No-op for reports created since patches moved to creation time
    ;; (report-entity): the guard prevents duplicate :report/patches
    ;; components on retry, while still healing reports created pending
    ;; by older versions whose Phase 4 never ran.
    (when (empty? (:report/patches
                   (d/pull (d/db conn) [{:report/patches [:db/id]}] report-eid)))
      (d/transact! conn [{:db/id report-eid :report/patches patches}])
      (log/info (count patches) "patch file(s) stored")))
  (when (:auto-series plan)
    (let [series-eid (series/create-series! conn (:topic report-info) from-addr 1)]
      (series/add-patch-to-series! conn series-eid report-eid email)
      (series/close-series! conn series-eid eid)
      (log/info "Auto-created single-member series for"
                (count patches) "patch attachments"))))

;; ---------------------------------------------------------------------------
;; Pending-thread retry (out-of-order delivery rescue)
;; ---------------------------------------------------------------------------

(defn- thread-anchorable?
  "True when the email can be threaded now: at least one ancestor mid
  is in the DB, or In-Reply-To is absent (root).
  Accepting any References ancestor (not just the immediate parent)
  approximates public-inbox: a missing intermediate doesn't orphan
  its descendants as long as some ancestor is known."
  [db email]
  (let [irt (:email/in-reply-to email)]
    (or (nil? irt)
        (boolean (some #(lookup/email-eid db %) (ancestor-mids email))))))

;; In-memory pending index: ancestor-mid-hash => #{pending eids}.  A
;; value join on the hash attr is forbidden (see bone.lookup) and
;; scanning every pending email once per processed email is quadratic
;; during an initial build, so the sole DB writer (this JVM daemon;
;; bb scripts are read-only) mirrors the pending set in memory.  The
;; index is seeded lazily from the DB and reseeded when the
;; connection changes (tests open fresh DBs).  Entries can only go
;; stale towards false positives (an eid whose flag was cleared);
;; `retry-pending-in-shared-thread!` re-checks the flag on pull.
(defonce ^:private pending-index (atom nil))  ; {:conn c :index {hash #{eid}}}

(defn- seed-pending-index
  "Index map rebuilt from the pending emails stored in `db`."
  [db]
  (reduce (fn [m [e pulled]]
            (reduce #(update %1 %2 (fnil conj #{}) e)
                    m (:email/ancestor-mid-hashes pulled)))
          {}
          (d/q '[:find ?e (pull ?e [:email/ancestor-mid-hashes])
                 :where [?e :email/pending-thread? true]]
               db)))

(defn- pending-index-map!
  "Current index map for `conn`, seeding it on first use.  `f`, when
  given, is applied to the map in the same atomic swap (seeding is a
  pure DB read, safe to retry)."
  ([conn] (pending-index-map! conn identity))
  ([conn f]
   (:index (swap! pending-index
                  (fn [cur]
                    (update (if (identical? (:conn cur) conn)
                              cur
                              {:conn conn :index (seed-pending-index (d/db conn))})
                            :index f))))))

(defn- pending-index-add!
  "Record `eid` as pending under each of its ancestor mid-hashes."
  [conn eid hashes]
  (pending-index-map!
   conn (fn [m] (reduce #(update %1 %2 (fnil conj #{}) eid) m hashes))))

(defn- pending-index-remove!
  "Drop `eid` from the index (its pending flag was retracted)."
  [conn eid hashes]
  (pending-index-map!
   conn (fn [m]
          (reduce (fn [acc h]
                    (let [s (disj (get acc h #{}) eid)]
                      (if (seq s) (assoc acc h s) (dissoc acc h))))
                  m hashes))))

;; ---------------------------------------------------------------------------
;; Single-email processing -- orchestrator
;; ---------------------------------------------------------------------------

(declare retry-pending-in-shared-thread!)

(defn- dispatch-commands!
  "Route commands carried by `email` between the new report and the
  nearest thread parents.
  - Root mail (no parent): every command applies to the new report.
  - Pure reply: every command applies to the nearest report.
  - Reply opening a new report (e.g. fresh `[BUG] ...` as `Re:` of a
    discussion): carrier-eligible relation lines (Supersedes:,
    Related-to:) land on the *new* report; everything else still flows
    to the thread parent."
  [conn email report-eid report-type nearest-eids from-addr
   source-name source-map rroles delivery]
  (let [creates-report? (some? report-eid)
        has-parents?    (seq nearest-eids)]
    (cond
      (and creates-report? has-parents?)
      (do (commands/apply-commands! conn report-eid report-type
                                    email source-map rroles delivery
                                    :carrier-only)
          (apply-commands-on-nearest! conn email from-addr source-name rroles
                                      source-map delivery nearest-eids
                                      :no-carrier))

      creates-report?
      (commands/apply-commands! conn report-eid report-type
                                email source-map rroles delivery nil)

      has-parents?
      (apply-commands-on-nearest! conn email from-addr source-name rroles
                                  source-map delivery nearest-eids nil))))

(defn- mark-digested-and-rescue!
  "Stamp :email/digested-at then rescue pending siblings in the same
  thread.  The rescue runs AFTER the write so the recursive call sees
  a consistent state."
  [conn eid email source-map sources]
  (d/transact! conn [{:db/id eid :email/digested-at (Date.)}])
  (retry-pending-in-shared-thread! conn email source-map sources))

(defn process-email!
  "Process a stored email: classify source, detect report, thread,
  apply commands, manage series.
  When no ancestor mid is in the DB, Phases 3-4 are skipped and the
  email is flagged :email/pending-thread? true -- a later arrival or
  the TTL flush will retry.
  Opts:
    :force-thread?   -- bypass the pending-thread guard (TTL flush)
    :resolved-source -- pre-classified {:src-name :delivery}, skips re-resolving."
  ([conn source-map sources email] (process-email! conn source-map sources email {}))
  ([conn source-map sources email {:keys [force-thread? resolved-source]}]
  (let [message-id   (:email/message-id email)
        eid          (:db/id email)
        from-addr    (:email/author-address email)
        was-pending? (:email/pending-thread? email)
        [source-name email delivery] (resolve-email-source! conn email sources resolved-source)]
    (if-not source-name
      (log/debug "No matching source for" message-id "-- skipping")
      (let [source-cfg   (when-let [cfg (get source-map source-name)]
                             (periods/source-cfg-at-date cfg (:email/date-sent email)))
            via-channel? (common/sent-via-source-channel? delivery source-cfg)
            rroles       (roles/get-tenures (d/db conn) source-name)]

        ;; Phase 1: apply controls (may mutate roles)
        (apply-controls! conn rroles source-name source-cfg from-addr email via-channel?)

        ;; Phase 2: detect and maybe create report (re-fetch roles after controls)
        (let [rroles      (roles/get-tenures (d/db conn) source-name)
              [report-eid report-info]
              (maybe-create-report! conn eid message-id email from-addr
                                    source-name source-cfg via-channel? rroles)
              db           (d/db conn)]

          (if (or force-thread? (thread-anchorable? db email))
            ;; --- Normal path: threading, commands, post-creation hooks ---
            (do
              (when was-pending?
                (d/transact! conn [[:db/retract eid :email/pending-thread? true]])
                (pending-index-remove! conn eid (:email/ancestor-mid-hashes email))
                (log/info "Cleared pending flag on" message-id))
              (let [{parent-eids :all nearest-eids :nearest} (thread-lookup email db)
                    ;; Recover the existing report-eid on retry so Phase 4
                    ;; hooks (link-related, close-superseded-thread, …) can
                    ;; run for pending emails that created a report on first
                    ;; pass but were skipped past Phase 3/4.
                    report-eid   (or report-eid
                                     (when was-pending?
                                       (lookup/report-eid db message-id)))]

                ;; Channel gating: an email that did not reach the
                ;; source's public channel is excluded from both
                ;; threading and command dispatch -- private replies
                ;; cannot annotate a public thread.
                (when via-channel?
                  (when (seq parent-eids)
                    (attach-as-descendant! conn eid email from-addr parent-eids))
                  (dispatch-commands! conn email report-eid (:type report-info)
                                      nearest-eids from-addr source-name source-map
                                      rroles delivery)
                  ;; Pure replies only: an email that created a report
                  ;; (e.g. a patch) must not leak its own trailers onto
                  ;; its thread parents.
                  (when (nil? report-eid)
                    (collect-trailers! conn email)))

                ;; Phase 4: post-creation hooks (plan is pure, execution is effectful)
                (when report-eid
                  (let [patches (detect/build-patch-entities email)
                        plan    (post-creation-plan report-info nearest-eids parent-eids patches)]
                    (run-post-creation-hooks! conn report-eid eid email from-addr report-info
                                              parent-eids nearest-eids patches plan)))

                (mark-digested-and-rescue! conn eid email source-map sources)))

            ;; --- Pending path: defer threading and commands ---
            (do (d/transact! conn [{:db/id eid
                                    :email/pending-thread? true
                                    :email/digested-at (Date.)}])
                (pending-index-add! conn eid (:email/ancestor-mid-hashes email))
                (log/info "Pending:" message-id "-- no ancestor mid in DB"
                          "(in-reply-to" (:email/in-reply-to email) ")")))))))))

(defn- retry-pending-in-shared-thread!
  "Retry pending emails that share an ancestor mid with `email` (or
  reference its own mid).  Recursive `process-email!` clears the
  pending flag when threading now resolves."
  [conn email source-map sources]
  (let [own-mid  (:email/message-id email)
        hashes   (cond-> (set (:email/ancestor-mid-hashes email))
                   own-mid (conj (common/mid-hash own-mid)))]
    (when (seq hashes)
      (let [index    (pending-index-map! conn)
            pendings (into #{} (mapcat #(get index %)) hashes)]
        (doseq [pending-eid pendings
                :let [pending-email (d/pull (d/db conn) email-pull-pattern
                                            pending-eid)]
                ;; A recursive rescue triggered by an earlier iteration may
                ;; have already processed this email and cleared its flag --
                ;; and the index only re-checks staleness here, on pull.
                :when (:email/pending-thread? pending-email)]
          (log/info "Retrying pending email" (:email/message-id pending-email)
                    "(triggered by" own-mid ")")
          (process-email! conn source-map sources pending-email))))))

;; ---------------------------------------------------------------------------
;; TTL flush -- force-process pending emails older than max-age-days
;; ---------------------------------------------------------------------------

(defn flush-stale-pending!
  "Force-process pending emails older than `max-age-days`.  Returns
  the count of flushed emails."
  [conn source-map sources max-age-days]
  (let [cutoff   (Date. (- (System/currentTimeMillis)
                           (* max-age-days 24 60 60 1000)))
        pendings (d/q '[:find [?e ...] :in $ ?cutoff
                        :where
                        [?e :email/pending-thread? true]
                        [?e :email/ingested-at ?ts]
                        [(.before ^java.util.Date ?ts ^java.util.Date ?cutoff)]]
                      (d/db conn) cutoff)]
    (when (seq pendings)
      (log/info "Flushing" (count pendings)
                "stale pending email(s) older than" max-age-days "day(s)"))
    ;; Keep the pending flag set: process-email! retracts it itself, which
    ;; both enables its report-eid recovery (was-pending?) and leaves the
    ;; email retriable at the next flush if processing throws.
    (doseq [eid pendings
            :let [email (d/pull (d/db conn) email-pull-pattern eid)]
            ;; A rescue triggered by an earlier iteration may have already
            ;; processed this email and cleared its flag.
            :when (:email/pending-thread? email)]
      (log/info "TTL-flush" (:email/message-id email))
      (process-email! conn source-map sources email {:force-thread? true}))
    (count pendings)))
