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
            [bark.roles :as roles]
            [bark.series :as series])
  (:import [java.util Date]))

;; ---------------------------------------------------------------------------
;; Email pull pattern (for re-loading a stored email)
;; ---------------------------------------------------------------------------

(def email-pull-pattern
  '[:db/id :email/imap-uid :email/source :email/subject :email/message-id
    :email/in-reply-to :email/references
    :email/from-address :email/from-name :email/date-sent :email/ingested-at
    :email/body-text :email/body-text-from-html :email/headers-edn
    {:email/attachments [:attachment/filename :attachment/content-type :attachment/data]}])

;; ---------------------------------------------------------------------------
;; Threading
;; ---------------------------------------------------------------------------

(defn ancestor-mids
  "Return an ordered vector of ancestor message-ids from the References
  and In-Reply-To headers.  Order follows RFC 2822: root thread ancestor
  first, immediate parent last (suitable for `rseq` to walk nearest-first)."
  [email]
  (let [raw  (:email/references email)
        refs (if (string? raw) (re-seq #"<[^>]+>" raw) [])
        irt  (:email/in-reply-to email)
        all  (if (and irt (not (some #{irt} refs)))
               (conj (vec refs) irt)
               (vec refs))]
    (vec (distinct all))))

(defn- lookup-reports-by-mid
  "Find report eids matching a message-id, either as report root or descendant."
  [db mid]
  (let [as-root (d/q '[:find [?r ...] :in $ ?mid :where [?r :report/message-id ?mid]]
                     db mid)
        as-desc (d/q '[:find [?r ...] :in $ ?mid
                       :where [?r :report/descendants ?e] [?e :email/message-id ?mid]]
                     db mid)]
    (into (set as-root) as-desc)))

(defn find-reports-for-email
  "Return all report eids threaded with this email."
  [email db]
  (let [mids (ancestor-mids email)]
    (reduce (fn [acc mid] (into acc (lookup-reports-by-mid db mid)))
            #{} mids)))

(defn find-nearest-report
  "Return the report eids of the nearest ancestor only."
  [email db]
  (some (fn [mid]
          (let [from-db (lookup-reports-by-mid db mid)]
            (when (seq from-db) from-db)))
        (rseq (ancestor-mids email))))

;; ---------------------------------------------------------------------------
;; DB operations
;; ---------------------------------------------------------------------------

(defn- ensure-participant!
  "Record a participant (and optionally mark as contributor for patches).
  Creates the entity on first encounter; on subsequent calls for a patch,
  stamps :participant/contributor-since if not already set."
  [conn source-name from-addr from-name date-sent & {:keys [contributor?]}]
  (when (and source-name from-addr)
    (let [k  (str (common/slugify source-name) ":" (str/lower-case from-addr))
          db (d/db conn)
          e  (d/entid db [:participant/key k])]
      (if e
        ;; Already a participant — stamp contributor-since if needed
        (when (and contributor?
                   (not (d/q '[:find ?d . :in $ ?e
                               :where [?e :participant/contributor-since ?d]] db e)))
          (d/transact! conn [{:db/id e
                              :participant/contributor-since (or date-sent (Date.))}])
          (log/info "Participant promoted to contributor:" from-addr "on" source-name))
        ;; New participant
        (do
          (d/transact! conn [(cond-> {:participant/key    k
                                      :participant/source source-name
                                      :participant/email  (str/lower-case from-addr)
                                      :participant/name   (or from-name "")
                                      :participant/since  (or date-sent (Date.))}
                               contributor? (assoc :participant/contributor-since
                                                   (or date-sent (Date.))))])
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
          {:report/last-activity-address (:email/from-address email)
           :report/version (:version report-info) :report/topic (:topic report-info)
           :report/patch-seq (:patch-seq report-info) :report/patch-source (:patch-source report-info)
           :report/has-ics (boolean has-ics) :report/has-text-attachments has-text})))

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

(defn- link-related-reports! [conn new-report-eid parent-report-eids]
  (when (seq parent-report-eids)
    (let [txdata (into []
                       (mapcat (fn [rid]
                                 [[:db/add new-report-eid :report/related rid]
                                  [:db/add rid :report/related new-report-eid]]))
                       parent-report-eids)]
      (d/transact! conn txdata))))

;; ---------------------------------------------------------------------------
;; Auto-close logic
;; ---------------------------------------------------------------------------

(defn- close-changes-for-release! [conn version release-email-eid release-report-eid]
  (when (and version (not (str/blank? version)))
    (let [db      (d/db conn)
          open-chgs (d/q '[:find [?r ...] :in $ ?ver
                           :where
                           [?r :report/type :change] [?r :report/version ?ver]
                           (not [?r :report/closed _])]
                         db version)]
      (when (seq open-chgs)
        ;; Single transaction: closing the changes and linking them to the
        ;; release must be atomic — otherwise a crash between the two could
        ;; leave changes closed but not linked to their release.
        (let [close-tx (mapv (fn [r] {:db/id r
                                      :report/closed release-email-eid
                                      :report/close-reason :resolved})
                             open-chgs)
              rel-tx   (into []
                             (mapcat (fn [chg-rid]
                                       [[:db/add release-report-eid :report/related chg-rid]
                                        [:db/add chg-rid :report/related release-report-eid]]))
                             open-chgs)]
          (d/transact! conn (into close-tx rel-tx)))
        (tracking/bump-report-updated! conn open-chgs)
        (log/info "Auto-closed" (count open-chgs)
                  "[CHG" version "] (superseded by release)")))))

(defn- parse-version-number [v]
  (when v (when-let [[_ n] (re-find #"^v(\d+)$" v)] (parse-long n))))

(defn- close-patch-previous-version! [conn report-eid report-info email-eid nearest-report-eids]
  (let [new-version (:version report-info)
        new-topic   (:topic report-info)
        n           (parse-version-number new-version)]
    (when (and n (>= n 1))
      (let [versions-to-close (cond-> #{new-version}
                                (> n 1) (conj (str "v" (dec n))))
            db (d/db conn)]
        (doseq [rid nearest-report-eids]
          (let [r (d/pull db [:report/type :report/version :report/topic :report/closed
                              :report/message-id] rid)]
            (when (and (= :patch (:report/type r))
                       (contains? versions-to-close (:report/version r))
                       (not (:report/closed r))
                       (or (and (nil? new-topic) (nil? (:report/topic r)))
                           (and new-topic
                                (= (str/lower-case new-topic)
                                   (str/lower-case (or (:report/topic r) ""))))))
              (d/transact! conn [{:db/id rid
                                  :report/closed email-eid
                                  :report/close-reason :superseded
                                  :report/superseded-by report-eid}])
              (tracking/bump-report-updated! conn rid)
              (log/info "Auto-closed [PATCH" (:report/version r)
                        (or (:report/topic r) "") "]"
                        (str "(" (:report/message-id r) ")")
                        (str "(superseded by " new-version ")")))))))))

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
  [conn report-eid email-eid email nearest-report-eids]
  (let [new-subj (normalize-subject (:email/subject email))
        db       (d/db conn)]
    (when (and new-subj (seq nearest-report-eids))
      (doseq [rid nearest-report-eids]
        (when (not= rid report-eid)
          (let [r (d/pull db [:report/type :report/closed :report/message-id
                              {:report/email [:email/subject]}] rid)]
            (when (and (= :patch (:report/type r))
                       (not (:report/closed r))
                       (= new-subj (normalize-subject (get-in r [:report/email :email/subject]))))
              (d/transact! conn [{:db/id rid
                                  :report/closed email-eid
                                  :report/close-reason :superseded
                                  :report/superseded-by report-eid}])
              (tracking/bump-report-updated! conn rid)
              (log/info "Auto-closed patch" (:report/message-id r)
                        "(superseded by same-subject thread patch)"))))))))

;; ---------------------------------------------------------------------------
;; Source resolution
;; ---------------------------------------------------------------------------

(defn- source-from-in-reply-to [db in-reply-to]
  (when in-reply-to
    (d/q '[:find ?src . :in $ ?mid
           :where [?e :email/message-id ?mid] [?e :email/source ?src]]
         db in-reply-to)))

(defn- classify-email-source
  "Shared source classification logic. Works on any headers (raw map or edn string).
  Returns [delivery src-name irt-src hdr-src]."
  [db sources headers in-reply-to]
  (let [delivery (common/classify-delivery headers)
        ;; 1. In-Reply-To inheritance
        irt-src  (source-from-in-reply-to db in-reply-to)
        ;; 2. Normal header-based match
        hdr-src  (when-not irt-src
                   (common/classify-source headers sources))]
    [delivery (or irt-src hdr-src) irt-src hdr-src]))

(defn pre-classify-source
  "Pre-storage source classification on a raw fetch-imap msg.
  Returns source-name or nil. When nil the email should not be stored."
  [db _source-map sources msg]
  (let [headers (:headers msg)
        irt     (common/extract-in-reply-to headers)
        [_delivery src-name] (classify-email-source db sources headers irt)]
    src-name))

(defn- resolve-email-source!
  "Classify the email's source and persist to DB.
  For live emails (pre-classified by store-and-process!) the source is already
  set; for test emails this runs the full classification."
  [conn email sources _source-map]
  (let [eid      (:db/id email)
        mid      (:email/message-id email)
        hdrs     (:email/headers-edn email)
        existing (:email/source email)]
    (if existing
      ;; Already classified (live path via store-and-process!)
      (let [delivery (common/classify-delivery hdrs)]
        [existing email delivery])
      ;; Not yet classified (test path — process-email! called directly)
      (let [irt (:email/in-reply-to email)
            [delivery src-name irt-src hdr-src] (classify-email-source
                                                  (d/db conn) sources hdrs irt)]
        (when (and irt-src hdr-src (not= irt-src hdr-src))
          (log/warn "Source mismatch for" mid
                    "— In-Reply-To says" irt-src
                    "but headers say" hdr-src (str "(using " irt-src ")")))
        (when src-name
          (d/transact! conn [{:db/id eid :email/source src-name}]))
        [src-name email delivery]))))

;; ---------------------------------------------------------------------------
;; Single-email processing — pure decisions
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
;; Single-email processing — effectful phases
;; ---------------------------------------------------------------------------

(defn- apply-controls!
  "Apply role and notify controls from the email body."
  [conn rroles source-name from-addr email via-channel?]
  (let [body-text (common/email-body-text email)]
    (when (and from-addr body-text source-name)
      (when via-channel?
        (roles/apply-role-controls! conn rroles source-name from-addr
                                    body-text (:email/date-sent email)))
      (roles/apply-notify-controls! conn rroles source-name from-addr body-text))))

(defn- maybe-create-report!
  "Detect report type, check permissions, create if allowed.
  Returns [report-eid report-info] or [nil report-info]."
  [conn eid message-id email from-addr source-name source-cfg via-channel? rroles]
  (let [subj-patterns (detect/resolve-labels (or source-cfg {}))
        allowed-types (:report-types source-cfg)
        report-info   (detect/detect-report email subj-patterns allowed-types)]
    (case (creation-decision report-info from-addr via-channel? rroles email
                             source-cfg (report-exists? (d/db conn) message-id))
      :create
      (do (log/info (str "[" (name (:type report-info)) "]") (:email/subject email))
          (let [rid (create-report! conn eid message-id report-info
                                    (:email/date-sent email) email)]
            (ensure-participant! conn source-name from-addr
                                 (:email/from-name email) (:email/date-sent email)
                                 :contributor? (= :patch (:type report-info)))
            (tracking/bump-report-updated! conn rid)
            [rid report-info]))

      :denied-channel
      (do (log/warn "Denied:" from-addr "cannot create" (name (:type report-info))
                    "(not via source channel)")
          [nil report-info])

      :denied-role
      (do (log/warn "Denied:" from-addr "cannot create" (name (:type report-info))
                    "(not maintainer)")
          [nil report-info])

      ;; nil — no report detected
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
                           (:email/from-name email) (:email/date-sent email)))
    (tracking/bump-report-updated! conn parent-eids)))

(defn- run-post-creation-hooks!
  "Execute post-creation side effects driven by the plan."
  [conn report-eid eid email from-addr report-info
   parent-eids nearest-eids patches plan]
  (when (:link-related plan)
    (link-related-reports! conn report-eid parent-eids))
  (when (:close-changes plan)
    (close-changes-for-release! conn (:version report-info) eid report-eid))
  (when (:close-previous-version plan)
    (close-patch-previous-version! conn report-eid report-info eid nearest-eids))
  (when (:close-superseded-thread plan)
    (close-superseded-thread-patches! conn report-eid eid email nearest-eids))
  (when (:manage-series plan)
    (series/manage-series! conn report-eid eid report-info from-addr parent-eids))
  (when (:store-patches plan)
    (d/transact! conn [{:db/id report-eid :report/patches patches}])
    (log/info (count patches) "patch file(s) stored"))
  (when (:auto-series plan)
    (let [series-eid (series/create-series! conn (:topic report-info) from-addr 1)]
      (series/add-patch-to-series! conn series-eid report-eid)
      (series/close-series! conn series-eid eid)
      (log/info "Auto-created single-member series for"
                (count patches) "patch attachments"))))

;; ---------------------------------------------------------------------------
;; Single-email processing — orchestrator
;; ---------------------------------------------------------------------------

(defn process-email!
  "Process a single email: classify source, detect report, thread,
  apply commands, manage series. Called after store-email! succeeds."
  [conn source-map sources email]
  (let [message-id (:email/message-id email)
        eid        (:db/id email)
        from-addr  (:email/from-address email)
        [source-name email delivery] (resolve-email-source! conn email sources source-map)]
    (if-not source-name
      (log/debug "No matching source for" message-id "— skipping")
      (let [source-cfg   (get source-map source-name)
            via-channel? (common/sent-via-source-channel? delivery source-cfg)
            rroles       (roles/get-tenures (d/db conn) source-name)]

        ;; Phase 1: apply controls (may mutate roles)
        (apply-controls! conn rroles source-name from-addr email via-channel?)

        ;; Phase 2: detect and maybe create report (re-fetch roles after controls)
        (let [rroles      (roles/get-tenures (d/db conn) source-name)
              [report-eid report-info]
              (maybe-create-report! conn eid message-id email from-addr
                                    source-name source-cfg via-channel? rroles)

              ;; Phase 3: threading and commands
              db           (d/db conn)
              parent-eids  (find-reports-for-email email db)
              nearest-eids (find-nearest-report email db)]

          (when (and (seq parent-eids) via-channel?)
            (thread-and-apply-commands! conn eid email from-addr source-name rroles
                                        source-map delivery parent-eids nearest-eids
                                        (some? report-eid)))

          ;; Phase 4: post-creation hooks (plan is pure, execution is effectful)
          (when report-eid
            (let [patches (detect/build-patch-entities email)
                  plan    (post-creation-plan report-info nearest-eids parent-eids patches)]
              (run-post-creation-hooks! conn report-eid eid email from-addr report-info
                                        parent-eids nearest-eids patches plan)))

          ;; Mark email as fully digested so future re-fetches can skip it.
          (when eid
            (d/transact! conn [{:db/id eid :email/digested-at (java.util.Date.)}])))))))