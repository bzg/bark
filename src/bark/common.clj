;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.common
  "Shared utilities for BARK JVM code.
  Pure functions plus Datalevin helpers for change tracking."
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [datalevin.core :as d]
            [taoensso.timbre :as log])
  (:import [java.text Normalizer Normalizer$Form SimpleDateFormat]
           [java.security MessageDigest]
           [java.util Date TimeZone]))

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(def bark-format
  "BARK export format version. Bump when the JSON/Org export shape changes."
  "0.2.2")

(def bark-schema
  (let [f (io/file "resources/bark-schema.edn")]
    (if (.exists f)
      (edn/read-string (slurp f))
      (throw (ex-info "resources/bark-schema.edn not found"
                      {:path (.getAbsolutePath f)})))))

(def ^:const meta-ident "global")

;; ---------------------------------------------------------------------------
;; Pure utilities
;; ---------------------------------------------------------------------------

(defn slugify
  "Normalize a source name for use as a directory name."
  [s]
  (-> (Normalizer/normalize (str s) Normalizer$Form/NFD)
      (str/replace #"\p{InCombiningDiacriticalMarks}+" "")
      str/lower-case
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"^-|-$" "")))

(defn sha256
  "Compute SHA-256 hex digest of a string."
  ^String [^String s]
  (let [digest (MessageDigest/getInstance "SHA-256")
        bytes  (.digest digest (.getBytes s "UTF-8"))]
    (str/join (map #(format "%02x" (Byte/toUnsignedInt %)) bytes))))

(defn mid-hash
  "Compute a stable directory-safe hash from a message-id."
  [message-id]
  (sha256 (str "bark:" message-id)))

(def patch-filename-re #"(?i)\.(patch|diff)$")

(defn patch-file?
  "True if filename looks like a patch/diff file."
  [filename]
  (boolean (and filename (re-find patch-filename-re filename))))

(defn email-body-text
  "Return the plain-text body of an email, preferring :email/body-text
  over :email/body-text-from-html."
  [email]
  (or (:email/body-text email) (:email/body-text-from-html email)))

(defn ensure-set
  "Coerce a Datalevin cardinality/many value to a set."
  [v]
  (cond (nil? v)  #{}
        (coll? v) (set v)
        :else     #{v}))

;; ---------------------------------------------------------------------------
;; Date formatting
;; ---------------------------------------------------------------------------

(defn format-date [date]
  (let [s (str (or date ""))]
    (subs s 0 (min 16 (count s)))))

(defn- iso-date-formatter
  "Create a yyyy-MM-dd formatter in UTC."
  ^SimpleDateFormat []
  (doto (SimpleDateFormat. "yyyy-MM-dd")
    (.setTimeZone (TimeZone/getTimeZone "UTC"))))

(defn format-date-iso
  "Format a java.util.Date as yyyy-MM-dd."
  [date]
  (when date (.format (iso-date-formatter) date)))

(defn days-between
  "Number of whole days between two Dates (absolute)."
  [^Date a ^Date b]
  (when (and a b)
    (quot (Math/abs (- (.getTime b) (.getTime a))) 86400000)))

;; ---------------------------------------------------------------------------
;; Header utilities
;; ---------------------------------------------------------------------------

(defn get-header
  "Case-insensitive header lookup from headers-edn (string or map)."
  [headers-edn header-name]
  (when headers-edn
    (try
      (let [headers (if (string? headers-edn) (edn/read-string headers-edn) headers-edn)
            lname   (str/lower-case header-name)]
        (some (fn [[k v]] (when (= (str/lower-case k) lname) v)) headers))
      (catch Exception e
        (log/warn "Failed to parse headers-edn:" (.getMessage e))
        nil))))

(defn extract-list-id
  "Extract the identifier from a List-Id header value.
  RFC 2919: \"Description <list-id>\" -> \"list-id\"."
  [raw]
  (when raw
    (if-let [[_ id] (re-find #"<([^>]+)>" (str raw))]
      id
      (str raw))))

;; ---------------------------------------------------------------------------
;; Source classification
;; ---------------------------------------------------------------------------

(defn- match-source?
  "Check if headers match a source's :match spec (substring, case-insensitive)."
  [headers-edn match-spec]
  (every? (fn [[k v]]
            (let [header-name (case k
                                :list-id      "List-Id"
                                :delivered-to "Delivered-To"
                                :to           "To"
                                (name k))
                  header-val  (get-header headers-edn header-name)
                  header-val  (if (= k :list-id)
                                (extract-list-id header-val)
                                header-val)]
              (and header-val
                   (str/includes? (str/lower-case (str header-val))
                                  (str/lower-case v)))))
          match-spec))

(def ^:private bark-prefix-pattern
  #"(?i)^\[bark:([^\]]+)\]")

(defn- extract-bark-list-id [subject]
  (when subject (second (re-find bark-prefix-pattern subject))))

(defn classify-source
  "Return the :name of the first matching source, or nil."
  [headers-edn subject sources]
  (let [bark-lid (extract-bark-list-id subject)]
    (some (fn [{:keys [name match]}]
            (when (or (empty? match)
                      (match-source? headers-edn match)
                      (and bark-lid (:list-id match)
                           (str/includes? (str/lower-case bark-lid)
                                          (str/lower-case (:list-id match)))))
              name))
          sources)))

;; ---------------------------------------------------------------------------
;; Label / command defaults and merge logic
;; ---------------------------------------------------------------------------

(def default-labels
  {:bug          ["BUG"]
   :patch        ["PATCH"]
   :request      ["POLL" "FR" "TODO"]
   :announcement ["ANN" "ANNOUNCEMENT"]
   :release      ["REL" "RELEASE"]
   :change       ["CHG" "CHANGE"]})

(def default-commands
  {:acked     ["Acked" "Confirmed" "Reviewed" "Approved"]
   :owned     ["Owned" "Handled" "Assigned"]
   :closed    ["Canceled" "Cancelled" "Resolved" "Applied"
               "Done" "Fixed" "Closed" "Expired"]
   :urgent    ["Urgent"]
   :important ["Important"]})

(def close-reasons
  {"Canceled"  :canceled
   "Cancelled" :canceled
   "Expired"   :expired})

(defn resolve-labels-map [source-cfg]
  (cond-> default-labels
    (:global-labels source-cfg) (merge (:global-labels source-cfg))
    (:labels source-cfg)        (merge (:labels source-cfg))))

(defn- normalize-command-entry [v]
  (if (vector? v) {:words v} v))

(defn resolve-commands-map [source-cfg]
  (let [global (or (:global-commands source-cfg) (:global-triggers source-cfg))
        local  (or (:commands source-cfg) (:triggers source-cfg))
        extract-words (fn [m]
                        (update-vals m (fn [v] (:words (normalize-command-entry v)))))]
    (cond-> default-commands
      global (merge (extract-words global))
      local  (merge (extract-words local)))))

(defn resolve-command-overrides [source-cfg]
  (let [global (or (:global-commands source-cfg) (:global-triggers source-cfg))
        local  (or (:commands source-cfg) (:triggers source-cfg))
        extract (fn [m]
                  (reduce-kv (fn [acc k v]
                               (let [entry (normalize-command-entry v)
                                     overrides (select-keys entry [:scope :report-types])]
                                 (if (seq overrides) (assoc acc k overrides) acc)))
                             {} m))]
    (merge (when global (extract global))
           (when local (extract local)))))

;; ---------------------------------------------------------------------------
;; Maintainer-since parsing
;; ---------------------------------------------------------------------------

(defn parse-maintainer-since-entries
  "Parse :roles/maintainer-since entries (\"email:yyyy-MM-dd\") into a map
  of lower-cased email -> date-string."
  [roles]
  (let [entries (let [v (:roles/maintainer-since roles)]
                  (cond (nil? v) #{} (string? v) #{v} :else (set v)))]
    (into {}
          (keep (fn [entry]
                  (let [idx (str/last-index-of entry ":")]
                    (when (and idx (pos? idx))
                      [(subs entry 0 idx) (subs entry (inc idx))]))))
          entries)))

;; ---------------------------------------------------------------------------
;; Config and source-map
;; ---------------------------------------------------------------------------

(defn load-config
  "Load config.edn if it exists, or nil."
  ([] (load-config "config.edn"))
  ([path]
   (let [f (io/file path)]
     (when (.exists f)
       (edn/read-string (slurp f))))))

(defn build-source-map
  "Build source-name -> config map from config."
  [config]
  (let [default-admin (:admin config)
        global-st     (:labels config)
        global-cmd    (:commands config)
        global-ef     (:export-formats config)
        global-er     (:export-reports config)
        global-expiry (:expiry config)
        global-rt     (:report-types config)]
    (into {}
          (map (fn [src]
                 [(:name src)
                  (merge {:admin (or (:admin src) default-admin)}
                         (select-keys src [:list-post :commands :labels :notifications
                                           :archive-format-string :list-archive :bark-path
                                           :maintainers])
                         (when-let [lid (get-in src [:match :list-id])] {:list-id lid})
                         (when-let [dt (get-in src [:match :delivered-to])] {:delivered-to dt})
                         (when global-st {:global-labels global-st})
                         (when global-cmd {:global-commands global-cmd})
                         {:export-formats (set (or (:export-formats src) global-ef ["json" "org" "rss"]))
                          :export-reports (when-let [er (or (:export-reports src) global-er)]
                                            (set (map keyword er)))
                          :report-types (when-let [rt (or (:report-types src) global-rt)]
                                          (set (map keyword rt)))
                          :expiry (or (:expiry src) global-expiry)})]))
          (:sources config))))

;; ---------------------------------------------------------------------------
;; Report scoring
;; ---------------------------------------------------------------------------

(defn report-priority [report]
  (+ (if (:report/urgent report) 2 0)
     (if (:report/important report) 1 0)))

(defn report-status [report]
  (+ (if-not (:report/closed report) 4 0)
     (if (:report/owned report) 2 0)
     (if (:report/acked report) 1 0)))

(defn report-descendant-count [report]
  (let [d (:report/descendants report)]
    (cond (sequential? d) (count d)
          (map? d)        1
          :else           0)))

;; ---------------------------------------------------------------------------
;; Canonical report pull pattern
;; ---------------------------------------------------------------------------

(def report-pull-pattern
  '[:db/id :report/type :report/version :report/topic
    :report/patch-seq :report/patch-source :report/message-id
    {:report/acked [:email/from-address]}
    {:report/owned [:email/from-address]}
    {:report/closed [:email/from-address :email/date-sent]}
    {:report/acked-proxy [:email/from-address]}
    {:report/owned-proxy [:email/from-address]}
    {:report/closed-proxy [:email/from-address]}
    {:report/urgent [:email/from-address]}
    {:report/important [:email/from-address]}
    {:report/urgent-proxy [:email/from-address]}
    {:report/important-proxy [:email/from-address]}
    :report/close-reason
    :report/votes-up :report/votes-down :report/votes-null
    :report/deadline :report/descendants :report/digested-at :report/updated-at
    {:report/related [:report/type :report/message-id
                      {:report/email [:email/headers-edn]}]}
    {:report/series [:series/id :series/expected :series/closed
                     {:series/patches [:db/id]}
                     {:series/cover-letter [:email/message-id]}]}
    {:report/patches [:patch/filename :patch/source :patch/text
                      :patch/author :patch/subject :patch/date]}
    {:report/email [:email/subject :email/from-address :email/from-name
                    :email/date-sent :email/source :email/imap-uid
                    :email/headers-edn]}])

(defn all-reports [db]
  (->> (d/q '[:find [(pull ?r report-pull-pattern) ...]
              :in $ report-pull-pattern
              :where [?r :report/type _]]
            db report-pull-pattern)))

;; ---------------------------------------------------------------------------
;; Change tracking (for incremental export)
;; ---------------------------------------------------------------------------

(defn bump-report-updated!
  "Set :report/updated-at and :meta/last-modified to now.
  `report-eid` can be a single eid or a collection of eids."
  [conn report-eid]
  (let [now  (Date.)
        eids (if (coll? report-eid) report-eid [report-eid])
        tx   (into [{:meta/ident meta-ident :meta/last-modified now}]
                   (map (fn [eid] {:db/id eid :report/updated-at now}))
                   eids)]
    (d/transact! conn tx)))

(defn bump-global-modified! [conn]
  (d/transact! conn [{:meta/ident meta-ident :meta/last-modified (Date.)}]))

(defn get-last-modified [db]
  (d/q '[:find ?t .
         :where [?e :meta/ident "global"] [?e :meta/last-modified ?t]] db))

(defn changed-report-types-since [db since-ts]
  (set (d/q '[:find [?t ...]
              :in $ ?since
              :where [?r :report/updated-at ?u] [(> ?u ?since)] [?r :report/type ?t]]
            db since-ts)))

;; ---------------------------------------------------------------------------
;; CLI arg parsing
;; ---------------------------------------------------------------------------

(defn parse-cli-args
  "Parse common CLI flags into a map.
  Recognises: -o/--output, -n/--source, -p/--min-priority, -s/--min-status,
  --json, --dir, --force, --only-open.
  Any leading non-flag token is captured as :format."
  [args]
  (loop [opts {} [a & [v & r :as more]] args]
    (cond
      (nil? a)                        opts
      (#{"--force"} a)                (recur (assoc opts :force-all? true) more)
      (#{"--only-open"} a)            (recur (assoc opts :only-open? true) more)
      (#{"-o" "--output"} a)          (if v (recur (assoc opts :out-file v) r) opts)
      (#{"-n" "--source"} a)          (if v (recur (assoc opts :source-name v) r) opts)
      (#{"--json"} a)                 (if v (recur (assoc opts :json-file v) r) opts)
      (#{"--dir"} a)                  (if v (recur (assoc opts :out-dir v) r) opts)
      (#{"-p" "--min-priority"} a)    (if v (recur (assoc opts :min-priority (parse-long v)) r) opts)
      (#{"-s" "--min-status"} a)      (if v (recur (assoc opts :min-status (parse-long v)) r) opts)
      (not (:format opts))            (recur (assoc opts :format a) more)
      :else                           (recur opts more))))
