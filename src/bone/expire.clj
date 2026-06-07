;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.expire
  "Close open reports when their age and state match the configured
  :expiry rules. Runs inside the daemon."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [taoensso.timbre :as log]
            [bone.common :as common]
            [bone.tracking :as tracking])
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
  "Activity score: acked=1, owned=2 (range 0-3, open-only)."
  [report]
  (+ (if (:report/owned report) 2 0)
     (if (:report/acked report) 1 0)))

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
       (when-let [deadline (:report/deadline-value report-data)]
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
         (<= (common/report-priority report-data) max-priority)))))

;; ---------------------------------------------------------------------------
;; Expiry engine
;; ---------------------------------------------------------------------------

(defn- find-or-create-expiry-email!
  "Get or create the synthetic email for an expiry event.  Uses a
  fresh (d/db conn) -- not a snapshot -- so re-runs within the same
  reduce see earlier inserts."
  [conn src report-mid now]
  (let [stripped  (str/replace (or report-mid "") #"^<|>$" "")
        synth-mid (str "<bone-expired-" stripped ">")]
    (or (d/entid (d/db conn) [:email/message-id synth-mid])
        (let [tempid -1
              ;; Stamp "bone-system" on both :from-address and
              ;; :author-address so all consumers see the synthetic actor.
              tx (d/transact!
                  conn [{:db/id                tempid
                         :email/message-id     synth-mid
                         :email/from-address   "bone-system"
                         :email/author-address "bone-system"
                         :email/source         src
                         :email/date-sent      now
                         :email/subject        (str "Auto-expired: " report-mid)}])]
          (get (:tempids tx) tempid)))))

(defn should-expire?
  "True when a report matches its explicit :expiry-value or the
  source-level expiry rule for its type."
  [report-data source-map src rtype now]
  (let [explicit-expiry (:report/expiry-value report-data)]
    (if explicit-expiry
      (.before ^Date explicit-expiry now)
      (let [expiry-cfg (:expiry (get source-map src))
            rule-raw   (get expiry-cfg (keyword rtype))]
        (when-let [rule (parse-expiry-rule rule-raw)]
          (rule-matches? rule report-data now))))))

(defn filter-expirable
  "Seq of {:rid :rtype :src :report-mid} for candidates that should
  expire (open + matching the expiry rule)."
  [candidates db-snap source-map now]
  (keep (fn [[rid rtype src]]
          (let [report-data (d/pull db-snap [:report/acked :report/owned
                                             :report/urgent :report/important
                                             :report/closed
                                             :report/expiry-value :report/deadline-value
                                             :report/last-activity] rid)]
            (when (and (nil? (:report/closed report-data))
                       (should-expire? report-data source-map src rtype now))
              {:rid rid :rtype rtype :src src
               :report-mid (:report/message-id (d/entity db-snap rid))})))
        candidates))

(defn expire-reports!
  "Close open reports matching the source's :expiry rule.  Sets
  :report/close-reason :expired."
  [conn source-map]
  (let [now (Date.)
        candidates (d/q '[:find ?r ?type ?src
                          :where
                          [?r :report/type ?type]
                          [?r :report/email ?e]
                          [?e :email/source ?src]
                          (not [?r :report/closed _])]
                        (d/db conn))
        db-snap  (d/db conn)
        to-expire (filter-expirable candidates db-snap source-map now)
        expired  (reduce
                  (fn [n {:keys [rid rtype src report-mid]}]
                    (let [synth-eid (find-or-create-expiry-email! conn src report-mid now)]
                      (d/transact! conn [[:db/add rid :report/closed synth-eid]
                                         [:db/add rid :report/closed-address "bone-system"]
                                         [:db/add rid :report/close-reason :expired]])
                      (tracking/bump-report-updated! conn rid)
                      (log/info "Expired" (name rtype) "report:" report-mid)
                      (inc n)))
                  0 to-expire)]
    (when (pos? expired)
      (log/info "Expired" expired "report(s)."))))
