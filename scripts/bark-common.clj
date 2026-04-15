;; bark-common.clj — Shared utilities for bark bb scripts (read-only).
;;
;; Loads bark.common (pure functions) from src/ via bb.edn :paths,
;; then adds Datalevin pod functions needed by bb scripts.
;;
;; Usage: (load-file "scripts/bark-common.clj")

;; Import pure functions from bark.common — explicit list for clj-kondo
(require 'bark.common)
(refer 'bark.common :only '[bark-format bark-schema slugify mid-hash
                            email-body-text ensure-set format-date format-date-iso
                            days-between parse-delay parse-cutoff-date
                            get-header source-type
                            default-labels default-commands
                            resolve-labels-map resolve-commands-map
                            active-tenures lead-maintainer lead-maintainer?
                            maintainer? load-config build-source-map
                            report-priority report-status report-descendant-count
                            report-pull-pattern attachment-pull-pattern
                            tenure-pull-pattern tenure-map parse-cli-args
                            votes-by-report vote-counts
                            ics-file? text-attachment?])

(require '[taoensso.timbre :as log])

(log/merge-config! {:min-level :info})

;; ---------------------------------------------------------------------------
;; Datalevin pod (bb-only — JVM uses datalevin.core directly)
;; ---------------------------------------------------------------------------

(def datalevin-version "0.10.7")

(defn load-datalevin-pod! []
  (require '[babashka.pods :as pods])
  ((resolve 'pods/load-pod) 'huahaiy/datalevin datalevin-version)
  (require '[pod.huahaiy.datalevin :as d]))

(def ^:private d-q    (delay (resolve 'pod.huahaiy.datalevin/q)))
(def ^:private d-pull (delay (resolve 'pod.huahaiy.datalevin/pull)))
(defn dq   "Resolved d/q"    [& args] (apply @d-q args))
(defn dpull "Resolved d/pull" [& args] (apply @d-pull args))

;; ---------------------------------------------------------------------------
;; Datalevin queries (bb-only — JVM equivalents are inline)
;; ---------------------------------------------------------------------------

(defn all-reports
  "Fetch all reports. Must be called after load-datalevin-pod!."
  [db]
  (->> (dq (list :find (list 'pull '?r report-pull-pattern)
                 :where ['?r :report/type '_])
           db)
       (map first)))

(defn fetch-attachment-data
  "Fetch attachment data for a single report by message-id.
  Returns the attachment-pull-pattern projection, or nil."
  [db message-id]
  (when message-id
    (dpull db attachment-pull-pattern [:report/message-id message-id])))

(defn get-tenures
  "Fetch all maintainer tenures (active and closed) for `source-name`.
  Returns a vector of tenure maps {:eid :email :from :to :order}."
  [db source-name]
  (let [eids (dq '[:find [?e ...]
                   :in $ ?src
                   :where [?e :maint-tenure/source ?src]]
                 db source-name)]
    (mapv #(bark.common/tenure-map
            (dpull db bark.common/tenure-pull-pattern %))
          eids)))

(defn tenures-snapshot
  "Return a serialization-friendly view of all tenures for a source,
  suitable for embedding in JSON/EDN exports. Each entry has:
    :email  lower-cased address
    :from   ISO date string \"yyyy-MM-dd\" or nil (= since the beginning)
    :to     ISO date string or nil (= currently active)
    :order  integer index from config.edn (absent when unknown)
    :lead?  true iff this is the currently-active lead tenure
  The list is sorted active-first (by :from asc, nil first), then closed
  tenures by :to desc — matching how the HTML docs render them."
  [tenures]
  (let [lead   (lead-maintainer tenures)
        sort-k (fn [{:keys [from to]}]
                 [(if to 1 0)
                  (if to
                    (- (.getTime ^java.util.Date to))
                    (if-let [^java.util.Date f from] (.getTime f) 0))])]
    (->> tenures
         (sort-by sort-k)
         (mapv (fn [{:keys [email from to order]}]
                 (cond-> {:email email
                          :from  (format-date-iso from)
                          :to    (format-date-iso to)
                          :lead? (and (nil? to) (= email lead))}
                   order (assoc :order order)))))))

(defn get-last-modified [db]
  (dq '[:find ?t .
        :where [?e :meta/ident "global"] [?e :meta/last-modified ?t]]
      db))

(defn changed-source-types-since
  "Return a map {source-name #{report-type ...}} for reports updated
  after `since-ts`.  Enables both source-level and per-type skip logic."
  [db since-ts]
  (reduce (fn [m [src rtype]]
            (update m src (fnil conj #{}) rtype))
          {}
          (dq '[:find ?src ?t
                 :in $ ?since
                 :where
                 [?r :report/updated-at ?u] [(> ?u ?since)]
                 [?r :report/type ?t]
                 [?r :report/email ?e] [?e :email/source ?src]]
               db since-ts)))
