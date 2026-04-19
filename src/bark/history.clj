;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.history
  "Helpers for the multi-era history workflow driven by
  `scripts/bark-rebuild-history.clj`.  Split out so they can be unit
  tested under `clj -M:test` (the executable script depends on
  Babashka-only libraries).

  Everything here is pure except `validate-entry` (uses `.exists`) and
  `validate-db-paths` (reads each entry's config file) — both do
  validation-level I/O only."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

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

(def ^:private default-db-path "data/bark-db")

(defn- read-config-file
  "Read and parse `path` as EDN, or nil on any failure.  Used by
  `validate-db-paths` to inspect era configs without the caller having
  to handle I/O errors — entry-level validation already catches
  missing/unparsable files."
  [path]
  (try (edn/read-string (slurp (io/file path)))
       (catch Exception _ nil)))

(defn validate-db-paths
  "Return error strings when the entries point to different =:db :path=
  values, which would silently split writes across two LMDB
  directories.  A missing =:db= or =:path= defaults to \"data/bark-db\".
  Entries whose config file can't be read are skipped here —
  `validate-entry` catches them separately."
  [entries]
  (let [pairs (keep (fn [{:keys [config]}]
                      (when-let [cfg (and config (read-config-file config))]
                        [config (or (get-in cfg [:db :path]) default-db-path)]))
                    entries)
        seen  (distinct (map second pairs))]
    (when (> (count seen) 1)
      [(str "eras point to different :db :path values "
            "(would silently split writes across LMDB dirs): "
            (str/join ", " (for [[c p] pairs] (str c " → " p))))])))

(defn validate-history
  "Run `validate-entry` on every entry, then `validate-contiguity` (when
  there are ≥ 2 entries) and `validate-db-paths`.  Returns a non-empty
  seq of error strings, or nil when the plan is valid."
  [entries]
  (let [entry-errs (into [] (mapcat validate-entry (range) entries))
        cont-errs  (if (>= (count entries) 2)
                     (validate-contiguity entries)
                     [])
        db-errs    (validate-db-paths entries)]
    (seq (concat entry-errs cont-errs db-errs))))

(defn merge-fetch-window
  "Return `cfg` with :ingest :fetch overridden to reflect the era's
  window.  The era's :start/:end map 1:1 to the :fetch :start/:end.
  When the era carries neither bound, drop :fetch entirely so the
  era config's own default (or the hard-coded {:limit 50}) applies —
  posting an empty :fetch map would hit parse-fetch's empty-map
  rejection downstream."
  [cfg {:keys [start end]}]
  (if (or start end)
    (assoc-in cfg [:ingest :fetch]
              (cond-> {}
                start (assoc :start start)
                end   (assoc :end   end)))
    (cond-> cfg
      (contains? (:ingest cfg) :fetch) (update :ingest dissoc :fetch))))
