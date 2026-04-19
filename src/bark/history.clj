;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.history
  "Pure helpers for the multi-era history workflow driven by
  `scripts/bark-rebuild-history.clj`.  Split out so they can be unit
  tested under `clj -M:test` (the executable script depends on
  Babashka-only libraries)."
  (:require [clojure.java.io :as io]))

(defn iso-date?
  "True when `s` is a string matching yyyy-MM-dd."
  [s]
  (boolean (and (string? s) (re-matches #"\d{4}-\d{2}-\d{2}" s))))

(defn validate-entry
  "Return a vector of human-readable error strings for a single history
  entry at position `idx`.  Empty vec = entry is well-formed."
  [idx entry]
  (if-not (map? entry)
    [(str "entry " idx ": not a map — " (pr-str entry))]
    (cond-> []
      (not (:config entry))
      (conj (str "entry " idx ": missing :config"))
      (and (:config entry) (not (.exists (io/file (:config entry)))))
      (conj (str "entry " idx ": config file not found — " (:config entry)))
      (and (:start entry) (not (iso-date? (:start entry))))
      (conj (str "entry " idx ": :start not ISO yyyy-MM-dd — " (pr-str (:start entry))))
      (and (:end entry) (not (iso-date? (:end entry))))
      (conj (str "entry " idx ": :end not ISO yyyy-MM-dd — " (pr-str (:end entry))))
      (and (iso-date? (:start entry)) (iso-date? (:end entry))
           (not (neg? (compare (:start entry) (:end entry)))))
      (conj (str "entry " idx ": :start must be strictly before :end")))))

(defn validate-contiguity
  "Check that consecutive entries form a contiguous half-open window:
  (:end of N) == (:start of N+1).  The first entry may omit :start
  (unbounded past); the last entry may omit :end (living config).
  Returns a vector of error strings (empty if contiguous)."
  [entries]
  (->> (partition 2 1 (map-indexed vector entries))
       (reduce (fn [errs [[ai a] [bi b]]]
                 (cond
                   (nil? (:end a))
                   (conj errs (str "entry " ai ": missing :end "
                                   "(only the last entry may omit it)"))
                   (nil? (:start b))
                   (conj errs (str "entry " bi ": missing :start "
                                   "(only the first entry may omit it)"))
                   (not= (:end a) (:start b))
                   (conj errs (str "gap/overlap between entries " ai " and " bi ": "
                                   ":end " (:end a)
                                   " ≠ :start " (:start b)))
                   :else errs))
               [])))

(defn validate-history
  "Run `validate-entry` on every entry, then `validate-contiguity` if
  there are at least two entries.  Returns a non-empty seq of error
  strings, or nil when the plan is valid."
  [entries]
  (let [entry-errs (into [] (mapcat validate-entry (range) entries))
        cont-errs  (if (>= (count entries) 2)
                     (validate-contiguity entries)
                     [])]
    (seq (concat entry-errs cont-errs))))

(defn merge-fetch-window
  "Return `cfg` with :ingest :fetch overridden to reflect the era's
  window.  The era's :start/:end map 1:1 to the :fetch :start/:end."
  [cfg {:keys [start end]}]
  (assoc-in cfg [:ingest :fetch]
            (cond-> {}
              start (assoc :start start)
              end   (assoc :end   end))))
