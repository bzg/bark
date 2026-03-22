;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.expire
  "Close open reports of expirable types when their age exceeds
  the configured :expiry delay. Runs inside the daemon."
  (:require [datalevin.core :as d]
            [taoensso.timbre :as log]
            [bark.common :as common]
            [bark.tracking :as tracking])
  (:import [java.util Date]))

(def ^:private expirable-types #{:announcement :release :change})

(defn expire-reports!
  "Close open reports of expirable types (announcement, release, change)
  when their age exceeds the configured :expiry delay for their source.
  Sets :report/close-reason to :expired."
  [conn source-map]
  (let [now (Date.)
        ;; Candidate query uses a snapshot taken before any writes.
        candidates (d/q '[:find ?r ?type ?src ?date
                          :in $ ?types
                          :where
                          [?r :report/type ?type]
                          [(contains? ?types ?type)]
                          [?r :report/email ?e]
                          [?e :email/source ?src]
                          [?e :email/date-sent ?date]
                          (not [?r :report/closed _])]
                        (d/db conn) expirable-types)
        expired (reduce
                 (fn [n [rid rtype src date-sent]]
                   (let [expiry-cfg (:expiry (get source-map src))
                         delay-days (get expiry-cfg (keyword rtype))]
                     (if (and delay-days date-sent
                              (> (common/days-between date-sent now) delay-days))
                       ;; Fresh db snapshot for each iteration — safe across writes.
                       (let [db         (d/db conn)
                             report-mid (d/q '[:find ?mid . :in $ ?r
                                               :where [?r :report/message-id ?mid]]
                                             db rid)
                             synth-mid  (str "<bark-expired-" report-mid ">")
                             synth-eid  (or (d/q '[:find ?e . :in $ ?mid
                                                   :where [?e :email/message-id ?mid]]
                                                 db synth-mid)
                                            (let [tempid -1
                                                  tx (d/transact!
                                                      conn [{:db/id          tempid
                                                             :email/message-id   synth-mid
                                                             :email/from-address "bark-system"
                                                             :email/source       src
                                                             :email/date-sent    now
                                                             :email/subject      (str "Auto-expired: " report-mid)}])]
                                              (get (:tempids tx) tempid)))]
                         (d/transact! conn [[:db/add rid :report/closed synth-eid]
                                            [:db/add rid :report/close-reason :expired]])
                         (tracking/bump-report-updated! conn rid)
                         (log/info "Expired" (name rtype) "report:" report-mid)
                         (inc n))
                       n)))
                 0 candidates)]
    (when (pos? expired)
      (log/info "Expired" expired "report(s)."))))
