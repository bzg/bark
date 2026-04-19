;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.periods
  "Per-source time-windowed overrides. A source may carry a :periods
  vector declaring how :maintainers / :commands / :command-syntax /
  :labels change over time. Each period inherits unspecified fields
  from the source level. Periods are contiguous half-open windows
  [from, to). Pure — no DB, no I/O."
  (:require [bark.common :as common]))

(def ^:private overridable-keys
  #{:maintainers :commands :command-syntax :labels})

(defn- iso-date? [s]
  (boolean (and (string? s) (re-matches #"\d{4}-\d{2}-\d{2}" s))))

(defn- normalize-period
  "Return {:from Date|nil :to Date|nil + overridable keys} resolved
  against `source-defaults`."
  [period source-defaults]
  (merge (select-keys source-defaults overridable-keys)
         (select-keys period overridable-keys)
         {:from (common/parse-iso-date (:start period))
          :to   (common/parse-iso-date (:end period))}))

(defn source-periods
  "Chronologically ordered normalized periods for `source`. When
  :periods is absent, returns a single all-time period derived from
  source-level fields."
  [source]
  (if-let [periods (:periods source)]
    (mapv #(normalize-period % source) periods)
    [(normalize-period {} source)]))

(defn period-at-date
  "Return the first period in `periods` whose half-open window
  [from, to) contains `date`. nil bounds are unbounded."
  [periods ^java.util.Date date]
  (when date
    (some (fn [{:keys [^java.util.Date from ^java.util.Date to] :as p}]
            (when (and (or (nil? from) (not (.before date from)))
                       (or (nil? to)   (.before date to)))
              p))
          periods)))

(defn source-cfg-at-date
  "Effective source-cfg for `source` at `date` — source-level fields
  plus the period-level overrides covering `date`. Falls back to the
  last period when no period covers `date` (defensive; contiguous
  periods with an unbounded first :start cover every date)."
  [source ^java.util.Date date]
  (let [periods (source-periods source)
        active  (or (period-at-date periods date)
                    (last periods))]
    (merge source (select-keys active overridable-keys))))

(defn- validate-entry [idx period]
  (if-not (map? period)
    [(str "period " idx ": not a map — " (pr-str period))]
    (cond-> []
      (and (:start period) (not (iso-date? (:start period))))
      (conj (str "period " idx ": :start not ISO yyyy-MM-dd — "
                 (pr-str (:start period))))
      (and (:end period) (not (iso-date? (:end period))))
      (conj (str "period " idx ": :end not ISO yyyy-MM-dd — "
                 (pr-str (:end period))))
      (and (iso-date? (:start period)) (iso-date? (:end period))
           (not (neg? (compare (:start period) (:end period)))))
      (conj (str "period " idx ": :start must be strictly before :end")))))

(defn- validate-contiguity [periods]
  (->> (partition 2 1 (map-indexed vector periods))
       (reduce (fn [errs [[ai a] [bi b]]]
                 (cond
                   (nil? (:end a))
                   (conj errs (str "period " ai ": missing :end "
                                   "(only the last period may omit it)"))
                   (nil? (:start b))
                   (conj errs (str "period " bi ": missing :start "
                                   "(only the first period may omit it)"))
                   (not= (:end a) (:start b))
                   (conj errs (str "gap/overlap between periods " ai
                                   " and " bi ": :end " (:end a)
                                   " ≠ :start " (:start b)))
                   :else errs))
               [])))

(defn validate-periods
  "Return a vector of error strings for `source`'s :periods. Empty vec
  = valid. Returns [] when :periods is absent."
  [source]
  (let [periods (:periods source)]
    (if-not periods
      []
      (let [entry-errs (into [] (mapcat validate-entry (range) periods))
            cont-errs  (if (>= (count periods) 2)
                         (validate-contiguity periods)
                         [])]
        (vec (concat entry-errs cont-errs))))))
