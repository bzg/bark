;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.common
  "Shared pure utilities for BARK. No datalevin dependency — loadable by both
  JVM and Babashka."
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
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
  (edn/read-string (slurp (io/resource "bark-schema.edn"))))

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

(defn format-date
  "Format a date as 'yyyy-MM-dd HH:mm' in UTC."
  [date]
  (when date
    (.format (doto (SimpleDateFormat. "yyyy-MM-dd HH:mm")
               (.setTimeZone (TimeZone/getTimeZone "UTC")))
             date)))

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

(defn parse-headers
  "Parse headers-edn into a seq of [key value] pairs.
  Accepts a string (EDN) or an already-parsed collection. Returns nil on failure."
  [headers-edn]
  (when headers-edn
    (try
      (if (string? headers-edn) (edn/read-string headers-edn) headers-edn)
      (catch Exception e
        (log/warn "Failed to parse headers-edn:" (.getMessage e))
        nil))))

(defn get-header
  "Case-insensitive header lookup from headers (string, map, or parsed seq).
  Returns the first value (string) or a vector if the header appears multiple times."
  [headers-edn header-name]
  (when-let [headers (parse-headers headers-edn)]
    (let [lname (str/lower-case header-name)]
      (some (fn [[k v]] (when (= (str/lower-case k) lname) v)) headers))))

(defn get-header-values
  "Like get-header but always returns a vector of strings (empty if missing).
  Handles headers stored as a single string or as a vector of strings."
  [headers-edn header-name]
  (let [v (get-header headers-edn header-name)]
    (cond
      (nil? v)    []
      (vector? v) v
      :else       [v])))

(defn extract-list-id
  "Extract the identifier from a List-Id header value.
  RFC 2919: \"Description <list-id>\" -> \"list-id\"."
  [raw]
  (when raw
    (if-let [[_ id] (re-find #"<([^>]+)>" (str raw))]
      id
      (str raw))))

(defn extract-in-reply-to
  "Extract In-Reply-To message-id from a headers map (raw or parsed).
  Handles both string and vector values."
  [headers]
  (when-let [v (or (get-header headers "In-Reply-To")
                   (get headers "In-Reply-To"))]
    (let [s (str/trim (if (vector? v) (first v) (str v)))]
      (when-not (str/blank? s) s))))

;; ---------------------------------------------------------------------------
;; Source classification
;; ---------------------------------------------------------------------------

(defn source-type
  "Infer the source type from its config keys.
  Returns nil (with warning) if no type key is present."
  [source]
  (or (cond
        (:list source)  :mailing-list
        (:alias source) :alias
        (:to source)    :mailbox)
      (do (log/warn "Source has no :list, :alias, or :to key:" (:name source))
          nil)))

(defn original-recipient
  "Extract the original recipient address from MTA headers.
  Checks X-Original-To, Envelope-To, X-Envelope-To in order."
  [headers-edn]
  (or (get-header headers-edn "X-Original-To")
      (get-header headers-edn "Envelope-To")
      (get-header headers-edn "X-Envelope-To")))

(defn- multiple-delivered-to?
  "True when Delivered-To contains 2+ distinct addresses (alias signal)."
  [headers-edn]
  (let [vals (->> (get-header-values headers-edn "Delivered-To")
                  (map str/lower-case)
                  distinct)]
    (> (count vals) 1)))

(defn classify-delivery
  "Heuristic: returns :list, :alias, or :direct based on email headers."
  [headers-edn]
  (let [hdrs (parse-headers headers-edn)]
    (cond
      ;; 1. Any List-Id or X-BeenThere → mailing list
      (or (get-header hdrs "List-Id")
          (get-header hdrs "X-BeenThere"))
      :list

      ;; 2a. Original recipient not in To/Cc → alias
      (let [orig (some-> (original-recipient hdrs) str/lower-case)
            to   (some-> (get-header hdrs "To") str/lower-case)
            cc   (some-> (get-header hdrs "Cc") str/lower-case)]
        (when (and orig
                   (not (some-> to (str/includes? orig)))
                   (not (some-> cc (str/includes? orig))))
          true))
      :alias

      ;; 2b. Multiple distinct Delivered-To → alias
      (multiple-delivered-to? hdrs)
      :alias

      ;; 3. Otherwise → direct
      :else :direct)))

(defn- matches?
  "Case-insensitive substring match."
  [haystack needle]
  (and haystack needle
       (str/includes? (str/lower-case (str haystack))
                      (str/lower-case needle))))

(defn- any-matches?
  "True if any value in coll matches needle (case-insensitive substring)."
  [coll needle]
  (boolean (some #(matches? % needle) coll)))

(defn- match-source?
  "Check if headers match a source based on its inferred type."
  [headers-edn source]
  (case (source-type source)
    :mailing-list (matches? (extract-list-id (get-header headers-edn "List-Id"))
                            (:list source))
    :alias        (or (matches? (original-recipient headers-edn)
                                (:alias source))
                      (any-matches? (get-header-values headers-edn "Delivered-To")
                                    (:alias source)))
    :mailbox      (any-matches? (get-header-values headers-edn "Delivered-To")
                                (:to source))
    false))

(def ^:private bark-prefix-pattern
  #"(?i)^\[bark:([^\]]+)\]")

(defn extract-bark-source
  "Extract source name from [bark:...] subject prefix or X-Bark-Source header."
  [headers-edn subject]
  (or (get-header headers-edn "X-Bark-Source")
      (when subject (second (re-find bark-prefix-pattern subject)))))

(defn classify-source
  "Return the :name of the first matching source, or nil.
  Uses classify-delivery + match-source? for normal matching, then falls back
  to [bark:...] / X-Bark-Source for maintainer direct emails."
  [headers-edn subject sources]
  (or
   ;; 1. Normal header-based match
   (some (fn [source]
           (when (match-source? headers-edn source) (:name source)))
         sources)
   ;; 2. Bark-source fallback (validated by caller for maintainer status)
   (let [bark-src (extract-bark-source headers-edn subject)]
     (when bark-src
       (let [lc (str/lower-case bark-src)]
         (some (fn [source]
                 (when (= (str/lower-case (:name source)) lc) (:name source)))
               sources))))))

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
;; Role checks (pure — operate on a roles map, no DB access)
;; ---------------------------------------------------------------------------

(defn- roles-set [roles attr]
  (ensure-set (get roles attr)))

(defn- has-role? [roles attr addr]
  (boolean (some #(= (str/lower-case %) (str/lower-case addr))
                 (roles-set roles attr))))

(defn admin? [roles addr]
  (and addr (:roles/admin roles)
       (= (str/lower-case (:roles/admin roles))
          (str/lower-case addr))))

(defn- parse-maintainer-since [roles]
  (let [fmt     (doto (SimpleDateFormat. "yyyy-MM-dd")
                  (.setTimeZone (TimeZone/getTimeZone "UTC")))]
    (into {}
          (keep (fn [[email date-str]]
                  (try [email (.parse fmt date-str)]
                       (catch Exception _ nil))))
          (parse-maintainer-since-entries roles))))

(defn maintainer?
  ([roles addr]
   (and addr (has-role? roles :roles/maintainers addr)))
  ([roles addr as-of]
   (and addr
        (has-role? roles :roles/maintainers addr)
        (if as-of
          (let [since (get (parse-maintainer-since roles) (str/lower-case addr))]
            (or (nil? since) (not (.before ^Date as-of since))))
          true))))

(defn admin-or-maintainer? [roles addr]
  (or (admin? roles addr) (maintainer? roles addr)))

(defn ignored? [roles addr]
  (and addr (has-role? roles :roles/ignored addr)))

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
                  (merge {:admin (or (:admin src) default-admin)
                          :source-type (source-type src)}
                         (select-keys src [:list :alias :to :commands :labels :notifications
                                           :archive-format-string :list-archive :bark-path
                                           :maintainers])
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
