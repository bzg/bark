(ns bark.tracking
  "Change-tracking helpers for incremental export.
  Separated from bark.common so that common stays datalevin-free."
  (:require [datalevin.core :as d])
  (:import [java.util Date]))

(def ^:const meta-ident "global")

(defn bump-report-updated!
  "Set :report/updated-at and :meta/last-modified to now.
  `report-eid` can be a single eid or a collection of eids."
  [conn report-eid]
  (let [now  (Date.)
        eids (if (coll? report-eid) report-eid [report-eid])]
    (d/transact! conn
      (into [{:meta/ident meta-ident :meta/last-modified now}]
            (map (fn [eid] {:db/id eid :report/updated-at now}))
            eids))))

(defn bump-global-modified!
  "Bump :meta/last-modified without targeting a specific report."
  [conn]
  (d/transact! conn [{:meta/ident meta-ident :meta/last-modified (Date.)}]))
