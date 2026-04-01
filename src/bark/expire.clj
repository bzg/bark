;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.expire
  "Close open reports when their age and state match the configured
  :expiry rules. Runs inside the daemon."
  (:require [datalevin.core :as d]
            [taoensso.timbre :as log]
            [bark.common :as common]
            [bark.tracking :as tracking])
  (:import [java.util Date]))

;; ---------------------------------------------------------------------------
;; Rule evaluation
;; ---------------------------------------------------------------------------

(defn- parse-expiry-rule
  "Normalize an expiry rule map.
  Expects a map with :inactive-after (integer, duration string, ISO date, or :deadline)."
  [v]
  (when (map? v)
    (let [after (:inactive-after v)]
      (cond
        (= :deadline after)
        (assoc v :expires-on-deadline true)

        (string? after)
        (if (re-matches #"\d{4}-\d{2}-\d{2}" after)
          (when-let [d (common/parse-iso-date after)]
            (assoc v :expires-on-date d))
          (when-let [d (common/parse-delay after)]
            (assoc v :delay-days d)))

        :else
        (when-let [d (common/parse-delay after)]
          (assoc v :delay-days d))))))

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

(defn- rule-matches?
  "Check whether a report matches all expiry rule conditions.
  Returns true if the report should be expired."
  [rule report-data now]
  (let [{:keys [delay-days expires-on-deadline expires-on-date max-status max-priority]} rule
        last-activity (:report/last-activity report-data)]
    (and
     ;; Age check: deadline-based, fixed date, or delay from last activity
     (cond
       expires-on-deadline
       (when-let [deadline (:report/deadline report-data)]
         (.before ^Date deadline now))
       expires-on-date
       (.before ^Date expires-on-date now)
       :else
       (and delay-days last-activity
            (> (common/days-between last-activity now) delay-days)))
     ;; Status ceiling (activity score: acked=1 + owned=2, range 0–3)
     (or (nil? max-status)
         (<= (report-activity-score report-data) max-status))
     ;; Priority ceiling
     (or (nil? max-priority)
         (<= (report-priority-value report-data) max-priority)))))

;; ---------------------------------------------------------------------------
;; Expiry engine
;; ---------------------------------------------------------------------------

(defn- find-or-create-expiry-email!
  "Look up or create the synthetic email entity for an expiry event.
  Uses a fresh (d/db conn) — NOT the read-only snapshot — so that
  re-runs within the same reduce are idempotent (they see earlier inserts)."
  [conn src report-mid now]
  (let [synth-mid (str "<bark-expired-" report-mid ">")]
    (or (d/entid (d/db conn) [:email/message-id synth-mid])
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
        candidates (d/q '[:find ?r ?type ?src
                          :where
                          [?r :report/type ?type]
                          [?r :report/email ?e]
                          [?e :email/source ?src]
                          (not [?r :report/closed _])]
                        (d/db conn))
        ;; Immutable snapshot for all read-only checks.
        ;; Write operations (find-or-create-expiry-email!) deliberately use (d/db conn)
        ;; to see their own prior inserts within this reduce.
        db-snap (d/db conn)
        expired (reduce
                 (fn [n [rid rtype src]]
                   (let [report-data (d/pull db-snap [:report/acked :report/owned
                                                      :report/urgent :report/important
                                                      :report/expiry :report/deadline
                                                      :report/last-activity] rid)
                         explicit-expiry (:report/expiry report-data)
                         ;; Per-report expiry takes precedence over source-level rules
                         should-expire?
                         (if explicit-expiry
                           (.before ^Date explicit-expiry now)
                           (let [expiry-cfg (:expiry (get source-map src))
                                 rule-raw   (get expiry-cfg (keyword rtype))]
                             (when-let [rule (parse-expiry-rule rule-raw)]
                               (rule-matches? rule report-data now))))]
                     (if should-expire?
                       (let [report-mid (:report/message-id (d/entity db-snap rid))
                             synth-eid  (find-or-create-expiry-email! conn src report-mid now)]
                         (d/transact! conn [[:db/add rid :report/closed synth-eid]
                                            [:db/add rid :report/closed-address "bark-system"]
                                            [:db/add rid :report/close-reason :expired]])
                         (tracking/bump-report-updated! conn rid)
                         (log/info "Expired" (name rtype) "report:" report-mid)
                         (inc n))
                       n)))
                 0 candidates)]
    (when (pos? expired)
      (log/info "Expired" expired "report(s)."))))
