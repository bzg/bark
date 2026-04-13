;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.tracking
  "Change-tracking helpers for incremental export.
  Separated from bark.common so that common stays datalevin-free."
  (:require [datalevin.core :as d])
  (:import [java.util Date]))

(def ^:const meta-ident "global")

(defn report-updated-tx
  "Build transaction data to set :report/updated-at and :meta/last-modified.
  `report-eid` can be a single eid or a collection of eids."
  [report-eid now]
  (let [eids (if (coll? report-eid) report-eid [report-eid])]
    (into [{:meta/ident meta-ident :meta/last-modified now}]
          (map (fn [eid] {:db/id eid :report/updated-at now}))
          eids)))

(defn global-modified-tx
  "Build transaction data to bump :meta/last-modified."
  [now]
  [{:meta/ident meta-ident :meta/last-modified now}])

(defn bump-report-updated!
  "Set :report/updated-at and :meta/last-modified to now.
  `report-eid` can be a single eid or a collection of eids."
  [conn report-eid]
  (d/transact! conn (report-updated-tx report-eid (Date.))))

(defn bump-global-modified!
  "Bump :meta/last-modified without targeting a specific report."
  [conn]
  (d/transact! conn (global-modified-tx (Date.))))
