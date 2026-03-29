;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.expire
  "Close open reports when their age and state match the configured
  :expiry rules. Runs inside the daemon."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [taoensso.timbre :as log]
            [bark.common :as common]
            [bark.tracking :as tracking])
  (:import [java.util Date]))

;; ---------------------------------------------------------------------------
;; Rule evaluation
;; ---------------------------------------------------------------------------

(defn- parse-expiry-rule
  "Normalize an expiry rule map into a map with :delay-days.
  Expects a map with at least :delay (integer or duration string)."
  [v]
  (when (map? v)
    (when-let [d (common/parse-delay (:delay v))]
      (assoc v :delay-days d))))

(defn- report-activity-score
  "Compute activity score from a pulled report: acked (1) + owned (2).
  Range 0–3. The open/closed bit is excluded because expiry candidates
  are already filtered to open reports only."
  [report]
  (+ (if (:report/owned report) 2 0)
     (if (:report/acked report) 1 0)))

(defn- report-priority-value
  "Compute priority from a pulled report."
  [report]
  (+ (if (:report/urgent report) 2 0)
     (if (:report/important report) 1 0)))

(defn- last-descendant-from
  "Return the :email/from-address of the most recent descendant email (by date)."
  [db report-eid]
  (let [descendants (d/q '[:find ?addr ?date
                           :in $ ?r
                           :where
                           [?r :report/descendants ?e]
                           [?e :email/from-address ?addr]
                           [?e :email/date-sent ?date]]
                         db report-eid)]
    (when (seq descendants)
      (first (last (sort-by second descendants))))))

(defn- op-address
  "Return the from-address of the report's founding email."
  [db report-eid]
  (d/q '[:find ?addr .
         :in $ ?r
         :where [?r :report/email ?e] [?e :email/from-address ?addr]]
       db report-eid))

(defn- rule-matches?
  "Check whether a report matches all expiry rule conditions.
  Returns true if the report should be expired."
  [db report-eid rule report-data now date-sent]
  (let [{:keys [delay-days max-status max-priority op-answered]} rule]
    (and
     ;; Age check
     delay-days date-sent
     (> (common/days-between date-sent now) delay-days)
     ;; Status ceiling (activity score: acked=1 + owned=2, range 0–3)
     (or (nil? max-status)
         (<= (report-activity-score report-data) max-status))
     ;; Priority ceiling
     (or (nil? max-priority)
         (<= (report-priority-value report-data) max-priority))
     ;; OP-answered check
     (or (nil? op-answered)
         (let [op   (op-address db report-eid)
               last (last-descendant-from db report-eid)]
           (if (false? op-answered)
             ;; Expire when the last reply is NOT from the OP
             (or (nil? last) (not= (some-> op str/lower-case)
                                   (some-> last str/lower-case)))
             ;; op-answered true: expire only when last IS from OP
             (and last (= (some-> op str/lower-case)
                          (some-> last str/lower-case)))))))))

;; ---------------------------------------------------------------------------
;; Expiry engine
;; ---------------------------------------------------------------------------

(defn- find-or-create-expiry-email!
  "Look up or create the synthetic email entity for an expiry event.
  Uses a fresh (d/db conn) — NOT the read-only snapshot — so that
  re-runs within the same reduce are idempotent (they see earlier inserts)."
  [conn src report-mid now]
  (let [synth-mid (str "<bark-expired-" report-mid ">")]
    (or (d/q '[:find ?e . :in $ ?mid
               :where [?e :email/message-id ?mid]]
             (d/db conn) synth-mid)
        (let [tempid -1
              tx (d/transact!
                  conn [{:db/id          tempid
                         :email/message-id   synth-mid
                         :email/from-address "bark-system"
                         :email/source       src
                         :email/date-sent    now
                         :email/subject      (str "Auto-expired: " report-mid)}])]
          (get (:tempids tx) tempid)))))

(defn expire-reports!
  "Close open reports whose age and state match the :expiry rules for
  their source. Sets :report/close-reason to :expired."
  [conn source-map]
  (let [now (Date.)
        candidates (d/q '[:find ?r ?type ?src ?date
                          :where
                          [?r :report/type ?type]
                          [?r :report/email ?e]
                          [?e :email/source ?src]
                          [?e :email/date-sent ?date]
                          (not [?r :report/closed _])]
                        (d/db conn))
        ;; Immutable snapshot for all read-only checks (report state, OP-answered, etc.)
        ;; Write operations (find-or-create-expiry-email!) deliberately use (d/db conn)
        ;; to see their own prior inserts within this reduce.
        db-snap (d/db conn)
        expired (reduce
                 (fn [n [rid rtype src date-sent]]
                   (let [report-data (d/pull db-snap [:report/acked :report/owned
                                                      :report/urgent :report/important
                                                      :report/expiry] rid)
                         explicit-expiry (:report/expiry report-data)
                         ;; Per-report expiry date takes precedence over global rules
                         should-expire?
                         (if explicit-expiry
                           (.before ^Date explicit-expiry now)
                           (let [expiry-cfg (:expiry (get source-map src))
                                 rule-raw   (get expiry-cfg (keyword rtype))]
                             (when-let [rule (parse-expiry-rule rule-raw)]
                               (rule-matches? db-snap rid rule report-data now date-sent))))]
                     (if should-expire?
                       (let [report-mid (d/q '[:find ?mid . :in $ ?r
                                               :where [?r :report/message-id ?mid]]
                                             db-snap rid)
                             synth-eid  (find-or-create-expiry-email! conn src report-mid now)]
                         (d/transact! conn [[:db/add rid :report/closed synth-eid]
                                            [:db/add rid :report/close-reason :expired]])
                         (tracking/bump-report-updated! conn rid)
                         (log/info "Expired" (name rtype) "report:" report-mid)
                         (inc n))
                       n)))
                 0 candidates)]
    (when (pos? expired)
      (log/info "Expired" expired "report(s)."))))
