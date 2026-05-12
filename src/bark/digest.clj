;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.digest
  "Single-email digest orchestration.
  Processes one email at a time: source classification, report detection,
  threading, command application, and series management.
  Called by bark.main/store-and-process! after each email is stored."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [taoensso.timbre :as log]
            [bark.common :as common]
            [bark.tracking :as tracking]
            [bark.detect :as detect]
            [bark.commands :as commands]
            [bark.periods :as periods]
            [bark.relations :as rel]
            [bark.roles :as roles]
            [bark.series :as series])
  (:import [java.util Date]))

;; ---------------------------------------------------------------------------
;; Email pull pattern (for re-loading a stored email)
;; ---------------------------------------------------------------------------

(def email-pull-pattern
  '[:db/id :email/id :email/source :email/subject :email/message-id
    :email/in-reply-to :email/references :email/ancestor-mids
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
  "Ordered vector of ancestor message-ids on `email`.  Order follows
  RFC 2822: root first, immediate parent last (so `rseq` walks
  nearest-first).

  Recomputed from `:email/references` / `:email/in-reply-to` rather
  than read from `:email/ancestor-mids`, which is multi-valued (no
  ordering guarantee) and only suited to the unordered lookup used by
  `retry-pending-in-shared-thread!`."
  [email]
  (common/ancestor-mids-from (:email/references email)
                              (:email/in-reply-to email)))

(defn- lookup-reports-by-mid
  "Find report eids matching a message-id, either as report root or
  descendant.  Returns #{} for mids exceeding the LMDB index limit
  (they cannot have been stored, so the lookup would crash with
  MDB_BAD_VALSIZE for nothing)."
  [db mid]
  (when (common/indexable-mid? mid)
    (let [as-root (d/q '[:find [?r ...] :in $ ?mid :where [?r :report/message-id ?mid]]
                       db mid)
          as-desc (d/q '[:find [?r ...] :in $ ?mid
                         :where [?r :report/descendants ?e] [?e :email/message-id ?mid]]
                       db mid)]
      (into (set as-root) as-desc))))

(defn- email-ancestors-by-mid
  "Ancestor mids of a stored email identified by `mid`, in root-first
  RFC 2822 order. Returns nil when `mid` is not indexable or no
  matching email exists. Used by `thread-lookup` to splice through a
  stored-but-pending intermediate email."
  [db mid]
  (when (common/indexable-mid? mid)
    (when-let [e (d/entid db [:email/message-id mid])]
      (let [pulled (d/pull db [:email/references :email/in-reply-to] e)]
        (common/ancestor-mids-from (:email/references pulled)
                                   (:email/in-reply-to pulled))))))

(def ^:private thread-lookup-max-splices
  "Upper bound on transitive ancestor splicing per `thread-lookup`
  call. Bounds worst-case walks on threads where many intermediate
  emails are stored but unattached to any report."
  32)

(defn thread-lookup
  "Walk an email's ancestor mids nearest-first and return
  `{:all #{eids} :nearest #{eids}}`.
  - `:all`     -- every report matched by any ancestor
  - `:nearest` -- reports matched by the closest matching ancestor,
                  or `nil` if no ancestor matches.

  When an ancestor mid matches a stored email but no report (an
  intermediate email held pending, or simply attached to no report),
  that email's own ancestor mids are spliced into the walk so the
  pending intermediate cannot orphan its descendants. Splicing is
  bounded by `thread-lookup-max-splices`."
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
                eids  (lookup-reports-by-mid db mid)
                acc'  (cond-> acc
                        (seq eids)                              (update :all into eids)
                        (and (seq eids) (nil? (:nearest acc))) (assoc :nearest eids))]
            (if (and (empty? eids)
                     (< splices thread-lookup-max-splices))
              (if-let [ancestors (email-ancestors-by-mid db mid)]
                (recur (into stack' ancestors) seen' (inc splices) acc')
                (recur stack' seen' splices acc'))
              (recur stack' seen' splices acc'))))))))

;; ---------------------------------------------------------------------------
;; DB operations
;; ---------------------------------------------------------------------------

(defn- ensure-participant!
  "Record a participant (and optionally mark as contributor for patches).
  Creates the entity on first encounter; on subsequent calls for a patch,
  stamps :participant/contributor-since if not already set."
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
  (some? (d/entid db [:report/message-id message-id])))

(defn report-entity
  "Build the entity map for a new report from email data."
  [email-eid message-id report-info email-date email now]
  (let [attachments (:email/attachments email)
        body-text   (common/email-body-text email)
        has-ics     (or (common/has-ics-attachment? attachments)
                        (common/has-inline-ics? body-text))
        has-text    (boolean (some common/text-attachment? attachments))]
    (into {:report/type (:type report-info) :report/email email-eid
           :report/message-id message-id :report/digested-at now
           :report/last-activity (or email-date now)}
          (remove (comp nil? val))
          {:report/last-activity-address (:email/author-address email)
           :report/version (:version report-info)
           :report/topic (when (:topic report-info) email-eid)
           :report/topic-value (:topic report-info)
           :report/patch-seq (:patch-seq report-info) :report/patch-source (:patch-source report-info)
           :report/has-ics has-ics :report/has-text-attachments has-text})))

(defn- create-report!
  "Create a new report entity. Returns the entity id of the new report."
  [conn email-eid message-id report-info email-date email]
  (d/transact! conn [(report-entity email-eid message-id report-info email-date email (Date.))])
  (d/entid (d/db conn) [:report/message-id message-id]))

(defn descendant-tx
  "Build transaction data to add an email as descendant of a report.
  `current-activity` is the report's current :report/last-activity (or nil)."
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
  "Post-creation linker: posts qualified relations between the new
  report and its threaded parents.

  - `:related-to` (neutral) is posted between the new report and every
    parent (was the role of the old `link-related-reports!`).
  - `:resolves`/`:resolved-by` is *additionally* posted when the new
    report is a patch and the parent is a bug or request -- this is
    the structural support for auto-crediting and closure propagation."
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
        ;; Close the changes atomically.  Linking via :related-to comes
        ;; right after -- :rel/* posing is non-destructive (rel entities)
        ;; so atomicity with the close is no longer mandatory.
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
  "Close `old-rid` as :superseded by `new-report-eid`, post the
  :supersedes (+ :related-to companion) link, and propagate auto-credit
  transfers.  `log-msg` is the human-readable detail appended to the
  Auto-closed log line.  Idempotent on the relation pose."
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
                                (> n 1) (conj (str "v" (dec n))))]
        (doseq [rid nearest-report-eids]
          ;; Refresh the snapshot per-iteration so prior transacts in
          ;; this loop are visible to the :report/closed check below.
          (let [r (d/pull (d/db conn)
                          [:report/type :report/version :report/topic-value :report/closed
                           :report/message-id]
                          rid)]
            (when (and (= :patch (:report/type r))
                       (contains? versions-to-close (:report/version r))
                       (not (:report/closed r))
                       (or (and (nil? new-topic) (nil? (:report/topic-value r)))
                           (and new-topic
                                (= (str/lower-case new-topic)
                                   (str/lower-case (or (:report/topic-value r) ""))))))
              (auto-supersede-patch!
               conn rid report-eid email
               (str "[PATCH " (:report/version r)
                    (when-let [t (:report/topic-value r)] (str " " t)) "] "
                    "(" (:report/message-id r) ") "
                    "(superseded by " new-version ")")))))))))

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
  "When a new patch report is created in a thread, close open ancestor patch
  reports that share the same base subject (ignoring Re:/[TAG] prefixes).
  This handles re-sent patches/diffs without explicit version numbers."
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
  "Lookup the source of the email referenced by `in-reply-to`, or nil.
  Skips mids exceeding the LMDB index limit; the caller falls back to
  header-based classification."
  [db in-reply-to]
  (when (common/indexable-mid? in-reply-to)
    (d/q '[:find ?src . :in $ ?mid
           :where [?e :email/message-id ?mid] [?e :email/source ?src]]
         db in-reply-to)))

(defn- classify-email-source
  "Shared source classification logic. Works on any headers (raw map or edn string).
  Returns {:delivery :src-name :irt-src :hdr-src}."
  [db sources headers in-reply-to]
  (let [irt-src (source-from-in-reply-to db in-reply-to)
        hdr-src (when-not irt-src (common/classify-source headers sources))]
    {:delivery (common/classify-delivery headers)
     :src-name (or irt-src hdr-src)
     :irt-src  irt-src
     :hdr-src  hdr-src}))

(defn pre-classify-source
  "Pre-storage source classification on a raw mailseq msg.
  Returns source-name or nil. When nil the email should not be stored."
  [db sources msg]
  (let [headers (:headers msg)
        irt     (common/extract-in-reply-to headers)]
    (:src-name (classify-email-source db sources headers irt))))

(defn- resolve-email-source!
  "Classify the email's source and persist to DB.
  For live emails (pre-classified by store-and-process!) the source is already
  set; for test emails this runs the full classification."
  [conn email sources]
  (let [eid      (:db/id email)
        mid      (:email/message-id email)
        hdrs     (:email/headers-edn email)
        existing (:email/source email)]
    (if existing
      (let [delivery (common/classify-delivery hdrs)]
        [existing email delivery])
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
  "Decide whether a report should be created for this email.
  Returns :create, :denied-channel, :denied-role, or nil (no report detected
  or report already exists)."
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
      ;; A patch in reply to a bug/request auto-credits the parent
      ;; (acked + owned).  Runs AFTER :link-related so :resolves exists.
      (and (= :patch rtype) (seq parent-eids))                (conj :auto-credit-resolves)
      (and (= :release rtype) (:version report-info))        (conj :close-changes)
      (and (= :patch rtype) (:version report-info)
           (seq nearest-eids))                                (conj :close-previous-version)
      (and (= :patch rtype) (seq nearest-eids))               (conj :close-superseded-thread)
      (and (= :patch rtype) (:patch-seq report-info))         (conj :manage-series)
      (and (= :patch rtype) (seq patches))                    (conj :store-patches)
      (and (= :patch rtype) (seq patches)
           (nil? (:patch-seq report-info))
           (> (count patches) 1))                             (conj :auto-series))))

;; ---------------------------------------------------------------------------
;; Single-email processing -- effectful phases
;; ---------------------------------------------------------------------------

(defn- apply-controls!
  "Apply role and notify controls from the email body."
  [conn rroles source-name source-cfg from-addr email via-channel?]
  (let [body-text (common/email-body-text email)
        src-cmds  (commands/build-source-commands source-cfg)
        strict?   (:strict-syntax? src-cmds)]
    (when (and from-addr body-text source-name)
      (when via-channel?
        (roles/apply-role-controls! conn rroles source-name from-addr
                                    body-text (:email/date-sent email) strict?))
      (roles/apply-notify-controls! conn rroles source-name from-addr body-text
                                    (:email/date-sent email) strict?))))

(defn- record-creation-denial!
  "Write a failure record for a report-creation attempt that was
  denied.  The report doesn't exist yet, so `:report-mid` is the
  empty string -- the notification renders the subject instead of a
  report link.  Audience is `:maintainers` so the lead sees the
  attempt."
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
            (tracking/bump-report-updated! conn rid)
            [rid report-info]))

      (:denied-channel :denied-role)
      (do (log/warn "Denied:" from-addr "cannot create" (name (:type report-info))
                    (str "(" (denial-reason-labels decision) ")"))
          (record-creation-denial! source-name from-addr email report-info decision)
          [nil report-info])

      ;; nil -- no report detected
      [nil report-info])))

(defn- thread-and-apply-commands!
  "Add email as descendant of parent reports and apply commands on nearest reports.
  Returns true if any command was applied."
  [conn eid email from-addr source-name rroles source-map delivery
   parent-eids nearest-eids new-report?]
  (doseq [rid parent-eids]
    (add-descendant! conn rid eid (:email/date-sent email) from-addr))
  (let [rid-info (when (seq nearest-eids)
                   (reduce (fn [m [r t s]] (assoc m r [t s]))
                           {}
                           (d/q '[:find ?r ?t ?src
                                  :in $ [?r ...]
                                  :where
                                  [?r :report/type ?t]
                                  [?r :report/email ?e]
                                  [?e :email/source ?src]]
                                (d/db conn) nearest-eids)))
        any-cmd? (reduce (fn [acc rid]
                           (if-let [[rtype rsrc] (get rid-info rid)]
                             (let [rroles (if rsrc (roles/get-tenures (d/db conn) rsrc) rroles)]
                               (if (commands/apply-commands! conn rid rtype email source-map rroles delivery)
                                 true acc))
                             acc))
                         false nearest-eids)]
    (when (and any-cmd? (not new-report?))
      (ensure-participant! conn source-name from-addr
                           (:email/author-name email) (:email/date-sent email)))
    (when (seq parent-eids)
      (tracking/bump-report-updated! conn parent-eids))))

(defn- auto-credit-resolved-reports!
  "For each report this patch :resolves (i.e. the bug/request the
  patch was posted in reply to), auto-set :report/acked and :report/owned
  on the parent, unless those attributes are already set explicitly
  (an existing setter is preserved, the patch becomes a validator only).

  Auto-credits posed here will later be detected via `auto-credit?`
  for retraction on cancel, and on supersession transferred to the
  new patch's author."
  [conn patch-report-eid email]
  (let [db        (d/db conn)
        email-eid (:db/id email)
        from-addr (:email/author-address email)
        addr-lc   (some-> from-addr str/lower-case)
        targets   (rel/active-targets db patch-report-eid :resolves)]
    (doseq [bug-eid targets]
      (let [bug-state (d/pull db [:report/acked :report/owned] bug-eid)
            credit    (fn [tx attr addr-attr]
                        (cond-> tx
                          (nil? (get bug-state attr))
                          (conj {:db/id bug-eid attr email-eid addr-attr addr-lc})))
            tx        (-> []
                          (credit :report/acked :report/acked-address)
                          (credit :report/owned :report/owned-address))]
        (when (seq tx)
          (d/transact! conn tx)
          (tracking/bump-report-updated! conn bug-eid)
          (log/info "Auto-credit:" from-addr "credited as acked+owned of"
                    (:report/message-id (d/pull (d/db conn) [:report/message-id] bug-eid))
                    "via patch"
                    (:report/message-id (d/pull (d/db conn) [:report/message-id] patch-report-eid))))))))

(defn- run-post-creation-hooks!
  "Execute post-creation side effects driven by the plan."
  [conn report-eid eid email from-addr report-info source-cfg
   parent-eids nearest-eids patches plan]
  (when (:link-related plan)
    (link-rel! conn report-eid (:type report-info) email parent-eids))
  ;; Auto-credit must run AFTER :link-related so :resolves relations exist.
  ;; Skipped on sources with `:patch-triggers? false`.
  (when (and (:auto-credit-resolves plan)
             (common/patch-triggers? source-cfg))
    (auto-credit-resolved-reports! conn report-eid email))
  (when (:close-changes plan)
    (close-changes-for-release! conn (:version report-info) email report-eid))
  (when (:close-previous-version plan)
    (close-patch-previous-version! conn report-eid report-info email nearest-eids))
  (when (:close-superseded-thread plan)
    (close-superseded-thread-patches! conn report-eid email nearest-eids))
  (when (:manage-series plan)
    (series/manage-series! conn report-eid email report-info from-addr parent-eids))
  (when (:store-patches plan)
    (d/transact! conn [{:db/id report-eid :report/patches patches}])
    (log/info (count patches) "patch file(s) stored"))
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
  (In-Reply-To or any References entry) is already stored in the DB.
  Also true when In-Reply-To is absent (root) or exceeds the LMDB
  index limit (cannot be looked up either way).

  Accepting any References ancestor -- not only the immediate parent
  -- approximates public-inbox's threading: a missing intermediate
  message no longer orphans its descendants, as long as the chain
  reaches some known ancestor.  When no anchor is found the email is
  held pending until an ancestor arrives or the TTL flush forces
  processing."
  [db email]
  (let [irt (:email/in-reply-to email)]
    (or (nil? irt)
        (not (common/indexable-mid? irt))
        (boolean (some #(d/entid db [:email/message-id %])
                       (ancestor-mids email))))))

;; ---------------------------------------------------------------------------
;; Single-email processing -- orchestrator
;; ---------------------------------------------------------------------------

(declare retry-pending-in-shared-thread!)

(defn process-email!
  "Process a single email: classify source, detect report, thread,
  apply commands, manage series. Called after store-email! succeeds.

  When no ancestor mid (In-Reply-To or any References entry) is
  stored in the DB, Phase 3 (threading + commands) and Phase 4
  (post-creation hooks) are skipped and the email is flagged
  `:email/pending-thread? true`.  A later arrival sharing the same
  thread triggers the retry via `retry-pending-in-shared-thread!`,
  or the TTL flush forces processing after N days.

  Opts:
    :force-thread? -- bypass the pending-thread guard, threading the
                     email with whatever ancestors currently exist in
                     the DB.  Used by the TTL flush."
  ([conn source-map sources email] (process-email! conn source-map sources email {}))
  ([conn source-map sources email {:keys [force-thread?]}]
  (let [message-id   (:email/message-id email)
        eid          (:db/id email)
        from-addr    (:email/author-address email)
        was-pending? (:email/pending-thread? email)
        [source-name email delivery] (resolve-email-source! conn email sources)]
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
                (log/info "Cleared pending flag on" message-id))
              (let [{parent-eids :all nearest-eids :nearest} (thread-lookup email db)
                    ;; Recover the existing report-eid on retry so Phase 4
                    ;; hooks (link-related, close-superseded-thread, …) can
                    ;; run for pending emails that created a report on first
                    ;; pass but were skipped past Phase 3/4.
                    report-eid   (or report-eid
                                     (when (and was-pending?
                                                (report-exists? db message-id))
                                       (d/entid db [:report/message-id message-id])))]

                (when (and (seq parent-eids) via-channel?)
                  (thread-and-apply-commands! conn eid email from-addr source-name rroles
                                              source-map delivery parent-eids nearest-eids
                                              (some? report-eid)))

                ;; Initial-mail directives: when the email is the first
                ;; of a new thread (no parents) AND it created a new
                ;; report, apply directives on the new report itself.
                ;; This makes `Supersedes: <mid>` (and other cross-
                ;; report directives) work in the opening mail of a
                ;; new bug/patch/request, not only in replies.
                (when (and report-eid via-channel? (empty? parent-eids))
                  (commands/apply-commands! conn report-eid (:type report-info)
                                            email source-map rroles delivery))

                ;; Phase 4: post-creation hooks (plan is pure, execution is effectful)
                (when report-eid
                  (let [patches (detect/build-patch-entities email)
                        plan    (post-creation-plan report-info nearest-eids parent-eids patches)]
                    (run-post-creation-hooks! conn report-eid eid email from-addr report-info
                                              source-cfg parent-eids nearest-eids patches plan)))

                ;; Mark email as fully digested so future re-fetches can skip it.
                (d/transact! conn [{:db/id eid :email/digested-at (Date.)}])

                ;; Out-of-order rescue: this email may have unblocked pending
                ;; siblings/descendants.  Run AFTER the digested-at write so
                ;; the recursive call sees a consistent state.
                (retry-pending-in-shared-thread! conn email source-map sources)))

            ;; --- Pending path: defer threading and commands ---
            (do (d/transact! conn [{:db/id eid
                                    :email/pending-thread? true
                                    :email/digested-at (Date.)}])
                (log/info "Pending:" message-id "-- no ancestor mid in DB"
                          "(in-reply-to" (:email/in-reply-to email) ")")))))))))

(defn- retry-pending-in-shared-thread!
  "After processing `email` normally, retry any pending email that
  shares at least one ancestor mid with it (or that has `email`'s own
  mid in its ancestors).  The recursive `process-email!` call retracts
  the pending flag if its In-Reply-To is now resolvable."
  [conn email source-map sources]
  (let [own-mid   (:email/message-id email)
        ancestors (cond-> (set (:email/ancestor-mids email))
                    own-mid (conj own-mid))]
    (when (seq ancestors)
      (let [pendings (d/q '[:find [?e ...] :in $ [?mid ...]
                            :where
                            [?e :email/pending-thread? true]
                            [?e :email/ancestor-mids ?mid]]
                          (d/db conn) (vec ancestors))]
        (doseq [pending-eid pendings]
          (let [pending-email (d/pull (d/db conn) email-pull-pattern pending-eid)]
            (log/info "Retrying pending email" (:email/message-id pending-email)
                      "(triggered by" own-mid ")")
            (process-email! conn source-map sources pending-email)))))))

;; ---------------------------------------------------------------------------
;; TTL flush -- force-process pending emails older than max-age-days
;; ---------------------------------------------------------------------------

(defn flush-stale-pending!
  "Force-process pending emails older than `max-age-days`. The pending
  flag is retracted and threading runs against whatever ancestors
  currently exist in the DB. Returns the count of flushed emails."
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
    (doseq [eid pendings]
      (d/transact! conn [[:db/retract eid :email/pending-thread? true]])
      (let [email (d/pull (d/db conn) email-pull-pattern eid)]
        (log/info "TTL-flush" (:email/message-id email))
        (process-email! conn source-map sources email {:force-thread? true})))
    (count pendings)))