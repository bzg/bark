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

(defn- ensure-contributor! [conn source-name from-addr from-name date-sent]
  (when (and source-name from-addr)
    (let [k (str source-name ":" (str/lower-case from-addr))]
      (when-not (d/q '[:find ?e . :in $ ?k :where [?e :contributor/key ?k]]
                     (d/db conn) k)
        (d/transact! conn [{:contributor/key    k
                            :contributor/source source-name
                            :contributor/email  (str/lower-case from-addr)
                            :contributor/name   (or from-name "")
                            :contributor/since  (or date-sent (Date.))}])
        (log/info "New contributor:" from-addr "on" source-name)))))

(defn- report-exists? [db message-id]
  (some? (d/q '[:find ?r . :in $ ?mid :where [?r :report/message-id ?mid]] db message-id)))

(defn- create-report! [conn email-eid message-id report-info]
  (d/transact! conn
               [(into {:report/type (:type report-info) :report/email email-eid
                       :report/message-id message-id :report/digested-at (Date.)}
                      (remove (comp nil? val))
                      {:report/version (:version report-info) :report/topic (:topic report-info)
                       :report/patch-seq (:patch-seq report-info) :report/patch-source (:patch-source report-info)})]))

(defn- add-descendant! [conn report-eid email-eid]
  (d/transact! conn [[:db/add report-eid :report/descendants email-eid]]))

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
        (d/transact! conn (mapv (fn [r] {:db/id r
                                         :report/closed release-email-eid
                                         :report/close-reason :resolved})
                                open-chgs))
        (let [rel-tx (into []
                           (mapcat (fn [chg-rid]
                                     [[:db/add release-report-eid :report/related chg-rid]
                                      [:db/add chg-rid :report/related release-report-eid]]))
                           open-chgs)]
          (d/transact! conn rel-tx))
        (tracking/bump-report-updated! conn open-chgs)
        (log/info "Auto-closed" (count open-chgs)
                  "[CHG" version "] (superseded by release)")))))

(defn- parse-version-number [v]
  (when v (when-let [[_ n] (re-find #"^v(\d+)$" v)] (parse-long n))))

(defn- close-patch-previous-version! [conn report-info email-eid nearest-report-eids]
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
                       (or (and (nil? new-topic) (nil? (:report/topic r)))
                           (and new-topic
                                (= (str/lower-case new-topic)
                                   (str/lower-case (or (:report/topic r) ""))))))
              (d/transact! conn [{:db/id rid
                                  :report/closed email-eid
                                  :report/close-reason :superseded}])
              (tracking/bump-report-updated! conn rid)
              (log/info "Auto-closed [PATCH" prev-version
                        (or (:report/topic r) "") "]"
                        (str "(" (:report/message-id r) ")")
                        (str "(superseded by " new-version ")")))))))))

;; ---------------------------------------------------------------------------
;; Source resolution
;; ---------------------------------------------------------------------------

(defn- source-from-in-reply-to [db in-reply-to]
  (when in-reply-to
    (d/q '[:find ?src . :in $ ?mid
           :where [?e :email/message-id ?mid] [?e :email/source ?src]]
         db in-reply-to)))

(defn- gate-direct-email
  "Apply the direct-email gate to a candidate source.
  Direct emails targeting :mailing-list or :alias sources require
  a maintainer with a source signal (X-Bark-Source or [<source-name>]).
  The [<source-name>] must match a known source to avoid confusion
  with type labels like [BUG] or [PATCH].
  Returns source-name or nil."
  [candidate-src delivery from-addr headers subject source-map db]
  (when candidate-src
    (let [src-cfg (get source-map candidate-src)]
      (if (and (= :direct delivery)
               (not= :mailbox (:source-type src-cfg)))
        (let [rroles    (roles/get-roles db candidate-src)
              bark-src  (common/extract-bark-source headers subject)
              known-src (when bark-src
                          (let [lc (str/lower-case bark-src)]
                            (some (fn [[k _]] (when (= (str/lower-case k) lc) k))
                                  source-map)))]
          (when (and (roles/maintainer? rroles from-addr)
                     (or (common/get-header headers "X-Bark-Source")
                         known-src))
            candidate-src))
        candidate-src))))

(defn- classify-email-source
  "Shared source classification logic. Works on any headers (raw map or edn string).
  Returns [delivery src-name irt-src hdr-src]."
  [db source-map sources headers subject from-addr in-reply-to]
  (let [delivery (common/classify-delivery headers)
        ;; 1. In-Reply-To inheritance (also gated)
        irt-src  (source-from-in-reply-to db in-reply-to)
        irt-src  (gate-direct-email irt-src delivery from-addr headers subject source-map db)
        ;; 2. Normal header + source-prefix match
        hdr-src  (when-not irt-src
                   (common/classify-source headers subject sources))
        hdr-src  (gate-direct-email hdr-src delivery from-addr headers subject source-map db)]
    [delivery (or irt-src hdr-src) irt-src hdr-src]))

(defn pre-classify-source
  "Pre-storage source classification on a raw fetch-imap msg.
  Returns source-name or nil. When nil the email should not be stored."
  [db source-map sources msg]
  (let [headers   (:headers msg)
        subject   (:subject msg)
        from-addr (:address (first (:from msg)))
        irt       (common/extract-in-reply-to headers)
        [_delivery src-name] (classify-email-source
                              db source-map sources headers subject from-addr irt)]
    src-name))

(defn- strip-source-prefix
  "Remove [<source-name>] prefix from email subject, if present."
  [email source-name]
  (if (and source-name (:email/subject email))
    (let [pat (re-pattern (str "(?i)^\\["
                               (java.util.regex.Pattern/quote source-name)
                               "\\]\\s*"))]
      (if (re-find pat (:email/subject email))
        (update email :email/subject #(str/replace-first % pat ""))
        email))
    email))

(defn- resolve-email-source!
  "Classify the email's source, persist to DB, strip [<source-name>] prefix in-memory.
  For live emails (pre-classified by store-and-process!) the source is already
  set; for test emails this runs the full classification."
  [conn email sources source-map]
  (let [eid      (:db/id email)
        mid      (:email/message-id email)
        hdrs     (:email/headers-edn email)
        existing (:email/source email)]
    (if existing
      ;; Already classified (live path via store-and-process!)
      (let [delivery (common/classify-delivery hdrs)]
        [existing (strip-source-prefix email existing) delivery])
      ;; Not yet classified (test path — process-email! called directly)
      (let [from-addr (:email/from-address email)
            irt       (:email/in-reply-to email)
            [delivery src-name irt-src hdr-src] (classify-email-source
                                                  (d/db conn) source-map sources
                                                  hdrs (:email/subject email) from-addr irt)]
        (when (and irt-src hdr-src (not= irt-src hdr-src))
          (log/warn "Source mismatch for" mid
                    "— In-Reply-To says" irt-src
                    "but headers say" hdr-src (str "(using " irt-src ")")))
        (when src-name
          (d/transact! conn [{:db/id eid :email/source src-name}]))
        [src-name (strip-source-prefix email src-name) delivery]))))

;; ---------------------------------------------------------------------------
;; Single-email processing
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
      (let [source-cfg (get source-map source-name)
            ;; Snapshot for initial permission checks. Will be refreshed after
            ;; mutations (role controls, report creation) that change DB state.
            rroles     (roles/get-roles (d/db conn) source-name)]
        ;; Check ignored
        (if (and from-addr (roles/ignored? rroles from-addr))
          (log/debug "Ignored" from-addr "—" (:email/subject email))
          (do
            ;; Role & notify controls (blocked on mailing list emails)
            (let [body-text (common/email-body-text email)]
              (when (and from-addr body-text source-name
                         (not= :list delivery))
                (roles/apply-role-controls! conn rroles source-name from-addr
                                            body-text (:email/date-sent email))
                (roles/apply-notify-controls! conn rroles source-name from-addr body-text)))

            ;; Re-fetch roles after role controls may have mutated them.
            (let [rroles (roles/get-roles (d/db conn) source-name)
                  subj-patterns (detect/resolve-labels (or source-cfg {}))
                  allowed-types (:report-types source-cfg)
                  report-info   (detect/detect-report email subj-patterns allowed-types)
                  permitted?    (and report-info from-addr
                                     (roles/can-create-report? rroles from-addr report-info
                                                               email source-cfg))
                  new-report?   (and permitted? (not (report-exists? (d/db conn) message-id)))
                  report-eid    (when new-report?
                                  (log/info (str "[" (name (:type report-info)) "]") (:email/subject email))
                                  (create-report! conn eid message-id report-info)
                                  (ensure-contributor! conn source-name from-addr
                                                       (:email/from-name email) (:email/date-sent email))
                                  (let [rid (d/q '[:find ?r . :in $ ?mid :where [?r :report/message-id ?mid]]
                                                 (d/db conn) message-id)]
                                    (tracking/bump-report-updated! conn rid)
                                    rid))]

              (when (and report-info (not permitted?)
                         (not (report-exists? (d/db conn) message-id)))
                (log/warn "Denied:" from-addr "cannot create" (name (:type report-info))))

              ;; Thread descendants
              (let [db          (d/db conn)
                    parent-eids (find-reports-for-email email db)
                    nearest-eids (find-nearest-report email db)]
                (when (seq parent-eids)
                  (when (common/sent-via-source-channel? delivery source-cfg)
                    (doseq [rid parent-eids]
                      (add-descendant! conn rid eid)))
                  ;; Only call ensure-contributor! if we didn't already do it
                  ;; during report creation above (avoids a redundant DB query).
                  (when-not new-report?
                    (ensure-contributor! conn source-name from-addr
                                         (:email/from-name email) (:email/date-sent email)))
                  (doseq [rid nearest-eids]
                    (when-let [rtype (d/q '[:find ?t . :in $ ?r :where [?r :report/type ?t]]
                                          (d/db conn) rid)]
                      (let [rsrc   (d/q '[:find ?src . :in $ ?rid
                                          :where [?rid :report/email ?e] [?e :email/source ?src]]
                                        (d/db conn) rid)
                            rroles (if rsrc (roles/get-roles (d/db conn) rsrc) rroles)]
                        (commands/apply-commands! conn rid rtype email source-map rroles delivery))))
                  (tracking/bump-report-updated! conn parent-eids))

                ;; Post-creation hooks
                (when report-eid
                  (when (seq parent-eids)
                    (link-related-reports! conn report-eid parent-eids))
                  (let [rtype (:type report-info)]
                    (when (and (= :release rtype) (:version report-info))
                      (close-changes-for-release! conn (:version report-info) eid report-eid))
                    (when (and (= :patch rtype) (:version report-info) (seq nearest-eids))
                      (close-patch-previous-version! conn report-info eid nearest-eids))
                    (when (and (= :patch rtype) (:patch-seq report-info))
                      (series/manage-series! conn report-eid eid report-info from-addr parent-eids))
                    (when (= :patch rtype)
                      (let [patches (detect/build-patch-entities email)]
                        (when (seq patches)
                          (d/transact! conn [{:db/id report-eid :report/patches patches}])
                          (log/info (count patches) "patch file(s) stored"))))))))))))))
