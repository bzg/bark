;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.common
  "Shared pure utilities for BARK. No datalevin dependency -- loadable by both
  JVM and Babashka."
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [taoensso.timbre :as log])
  (:import [java.text Normalizer Normalizer$Form]
           [java.security MessageDigest]
           [java.time ZoneId]
           [java.time.format DateTimeFormatter]
           [java.util Date]))

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(def bark-format
  "BARK export format version. Bump when the JSON/Org export shape changes."
  "0.9.1")

(def bark-schema
  (edn/read-string (slurp (io/resource "bark-schema.edn"))))

(def failures-file-path
  "Shared command-failures EDN file: JVM writes, bb scripts read.
   Kept under data/ so the public/ directory only holds files that
   are safe to serve."
  "data/.failures.edn")

(defn read-failures-file
  "Read the failures EDN file at `path`, returning a vector ([] if
  missing or unparseable).  `on-error` is invoked with the exception
  on parse failure."
  ([] (read-failures-file failures-file-path nil))
  ([path] (read-failures-file path nil))
  ([path on-error]
   (let [f (io/file path)]
     (if (.exists f)
       (try (edn/read-string (slurp f))
            (catch Exception e
              (when on-error (on-error e))
              []))
       []))))

;; ---------------------------------------------------------------------------
;; Command failure labels
;; ---------------------------------------------------------------------------

(def reason-labels
  "Human-readable labels for command-failure :reason keys."
  {:unknown-target     "unknown target"
   :type-mismatch      "type mismatch between source and target"
   :self-loop          "target is the same report as the source"
   :insufficient-scope "insufficient permissions"})

;; ---------------------------------------------------------------------------
;; Pure utilities
;; ---------------------------------------------------------------------------

(defn expand-home
  "Expand a leading ~/ to the user's home directory."
  [p]
  (if (and (string? p) (str/starts-with? p "~/"))
    (str (System/getProperty "user.home") (subs p 1))
    p))

;; Shared identifier rule for :sources [{:name ...}] and :mailboxes
;; [{:name ...}].  Starts with alphanum; spaces allowed in the middle
;; to keep current :source/name configs valid; no slash / colon /
;; control chars (those would break paths or watermark ids).
(def config-name-regex #"[a-zA-Z0-9][a-zA-Z0-9 ._-]*")

(defn valid-config-name?
  "True iff `s` is a non-blank string matching `config-name-regex`."
  [s]
  (and (string? s)
       (not (str/blank? s))
       (some? (re-matches config-name-regex s))))

(def singleton-mailbox-error
  "Single source of truth for the rejected-`:mailbox`-key message."
  (str ":mailbox is no longer accepted (even when set to nil) "
       "-- use :mailboxes [{...}] (vector of mailbox maps with a :name each)."))

(defn all-distinct?
  "True iff `coll` contains no duplicates (under `=`).  Prefer over
  `(apply distinct? coll)` -- the latter throws on empty collections
  and depends on splatting through `apply`."
  [coll]
  (= (count coll) (count (distinct coll))))

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

(defn escape-script-payload
  "Neutralize HTML-script-closing and comment sequences in a JSON payload that
   will be inlined inside an HTML <script> tag.  Email-controlled fields
   (subject, from-name, …) reach this path verbatim, so a `</script>` in any
   string would otherwise break out of the script element."
  ^String [^String s]
  (-> s
      (str/replace "</" "<\\/")
      (str/replace "<!--" "<\\!--")
      (str/replace "-->" "--\\>")))

(def patch-filename-re #"(?i)\.(patch|diff)$")

(defn patch-file?
  "True if filename looks like a patch/diff file."
  [filename]
  (boolean (and filename (re-find patch-filename-re filename))))

(def ics-filename-re #"(?i)\.ics$")

(defn ics-file?
  "True if filename looks like an ICS calendar file."
  [filename]
  (boolean (and filename (re-find ics-filename-re filename))))

(def text-content-types
  "Content types whose attachment data is stored as text."
  #{"text/plain" "text/x-log"})

(defn text-attachment?
  "True if an attachment has a text/plain or text/x-log content type."
  [att]
  (boolean (when-let [ct (:attachment/content-type att)]
             (text-content-types (-> ct str/lower-case (str/split #";") first str/trim)))))

(defn has-ics-attachment?
  "True if any attachment has a .ics filename."
  [attachments]
  (boolean (some #(ics-file? (:attachment/filename %)) attachments)))

(defn has-inline-ics?
  "True if body text contains inline ICS/VCALENDAR content."
  [body-text]
  (boolean
   (when body-text
     (and (str/includes? body-text "BEGIN:VCALENDAR")
          (str/includes? body-text "BEGIN:VEVENT")))))

(defn strip-signature
  "Remove the RFC 3676 email signature (everything after a line
  containing exactly \"-- \") from plain-text body."
  [text]
  (if text
    (first (str/split text #"\r?\n-- \s*(?:\r?\n|$)" 2))
    text))

(defn email-body-text
  "Return the plain-text body of an email, preferring :email/body-text
  over :email/body-text-from-html.  The RFC 3676 signature delimiter
  (\"-- \") is stripped so that signature lines are not scanned for
  commands."
  [email]
  (some-> (or (:email/body-text email) (:email/body-text-from-html email))
          strip-signature))

(defn ensure-set
  "Coerce a Datalevin cardinality/many value to a set.
  A single string is wrapped in a set (not split into characters)."
  [v]
  (cond (nil? v)    #{}
        (string? v) #{v}
        (coll? v)   (set v)
        :else       #{v}))

;; ---------------------------------------------------------------------------
;; Date formatting
;; ---------------------------------------------------------------------------

;; Thread-safe formatters (DateTimeFormatter is immutable and safe to share).
(def ^:private datetime-fmt
  (-> (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")
      (.withZone (ZoneId/of "UTC"))))

(def ^:private date-iso-fmt
  (-> (DateTimeFormatter/ofPattern "yyyy-MM-dd")
      (.withZone (ZoneId/of "UTC"))))

(defn format-date
  "Format a java.util.Date as 'yyyy-MM-dd HH:mm' in UTC."
  [^Date date]
  (when date
    (.format datetime-fmt (.toInstant date))))

(defn format-date-iso
  "Format a java.util.Date as yyyy-MM-dd in UTC."
  [^Date date]
  (when date
    (.format date-iso-fmt (.toInstant date))))

(defn days-between
  "Number of whole days between two Dates (absolute)."
  [^Date a ^Date b]
  (when (and a b)
    (quot (Math/abs (- (.getTime b) (.getTime a))) 86400000)))

;; ---------------------------------------------------------------------------
;; Duration parsing
;; ---------------------------------------------------------------------------

(def ^:private duration-unit-days
  {"y" 365 "m" 30 "w" 7 "d" 1})

(defn parse-duration-str
  "Parse \"2y 3m 10d 2w\" into a total number of days.
  Returns nil if no valid tokens. Throws on unrecognized units."
  [s]
  (let [valid     (re-seq #"(\d+)\s*(y|m|w|d)" s)
        bad-units (->> (re-seq #"(\d+)\s*([a-zA-Z]+)" s)
                       (remove (fn [[_ _ u]] (contains? duration-unit-days u)))
                       (map #(nth % 2)))]
    (when (seq bad-units)
      (throw (ex-info (str "Unknown duration unit(s): " (str/join ", " bad-units)
                           " (expected y, m, w, d)")
                      {:value s :bad-units (vec bad-units)})))
    (when (seq valid)
      (reduce (fn [acc [_ n unit]]
                (+ acc (* (parse-long n) (duration-unit-days unit))))
              0 valid))))

(defn parse-delay
  "Parse a duration value into a number of days.
  Accepts an integer (days) or a duration string (\"30d\", \"6w\", \"3m\").
  Note: \"0d\" returns 0, meaning 'expire immediately'."
  [v]
  (cond
    (integer? v) v
    (string? v)  (parse-duration-str v)
    :else        nil))

(defn parse-iso-date
  "Parse an ISO date string (\"2024-01-01\") into a java.util.Date, or nil."
  [^String s]
  (when s
    (try (.parse (java.text.SimpleDateFormat. "yyyy-MM-dd") s)
         (catch Exception _ nil))))

(defn parse-cutoff-date
  "Resolve a retention value to a java.util.Date cutoff in the past.
  Accepts an ISO date (\"2024-01-01\") or a duration string (\"90d\", \"6m\")
  relative to now. Returns nil when the value is not a string or unparseable."
  [v]
  (when (string? v)
    (or (parse-iso-date v)
        (when-let [days (parse-delay v)]
          (Date. (- (System/currentTimeMillis)
                    (* (long days) 24 60 60 1000)))))))

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

;; RFC 5322 §3.6.4 token shape -- msg-id = "<" id-left "@" id-right ">".
;; Reject whitespace and angle brackets inside; tolerate the obs-* set
;; through the broad atext-ish character class.
(def ^:private mid-token-re #"<[^<>\s]+>")

(defn normalize-mid
  "Normalize a bracketed Message-Id by lowercasing the domain part
  (everything after the last `@`), per RFC 5322 §3.6.4 which makes the
  id-right case-insensitive while keeping the id-left case-sensitive.
  Returns nil on nil; leaves mids without `@` untouched."
  [mid]
  (when mid
    (let [at (.lastIndexOf ^String mid (int \@))]
      (if (pos? at)
        (str (subs mid 0 (inc at)) (str/lower-case (subs mid (inc at))))
        mid))))

(defn extract-bracketed-id
  "Extract the first <addr@domain> token from a header value, domain
  lowercased (RFC 5322 §3.6.4).  Returns nil for nil/blank input or
  when no well-formed bracketed token is found (rejects mids with
  whitespace inside)."
  [v]
  (when v
    (let [s (if (vector? v) (first v) (str v))]
      (when-not (str/blank? s)
        (some-> (first (re-seq mid-token-re s))
                normalize-mid)))))

(defn extract-in-reply-to
  "Extract In-Reply-To message-id from a headers map (raw or parsed).
  Handles both string and vector values."
  [headers]
  (extract-bracketed-id (get-header headers "In-Reply-To")))

;; Cap mid length to stay under LMDB's 511-byte AVE key limit (the
;; Datalevin encoding ~doubles the raw string).  Mids above this are
;; dropped from lookups -- pathological ProtonMail mids in the wild
;; exceed 130 chars and trigger MDB_BAD_VALSIZE on insert.
(def ^:const max-indexable-mid-length 200)

(defn indexable-mid?
  "True iff `mid` fits in the LMDB AVE index (string ≤ 200 chars)."
  [mid]
  (and (string? mid) (<= (count mid) max-indexable-mid-length)))

(defn ancestor-mids-from
  "Ordered vector of ancestor mids (root first, parent last) from
  References + In-Reply-To.  Mids are normalized and filtered through
  indexable-mid?."
  [references in-reply-to]
  (let [refs (if (string? references)
               (mapv normalize-mid (re-seq mid-token-re references))
               [])
        irt  (normalize-mid in-reply-to)
        all  (if (and irt (not (some #{irt} refs)))
               (conj (vec refs) irt)
               (vec refs))]
    (into [] (comp (distinct) (filter indexable-mid?)) all)))

;; ---------------------------------------------------------------------------
;; Source classification
;; ---------------------------------------------------------------------------

(defn source-type
  "Infer the source type from its config keys. Returns nil if none present."
  [source]
  (cond
    (:list source)  :mailing-list
    (:alias source) :alias
    (:to source)    :mailbox))

(defn sent-via-source-channel?
  "True when the email was delivered through the source's public channel,
  or when the source is a mailbox (no public/private distinction)."
  [delivery source-cfg]
  (case (:source-type source-cfg)
    :mailing-list (= :list delivery)
    :alias        (= :alias delivery)
    :mailbox      true
    false))

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

(defn- original-recipient-not-in-to-cc?
  "True when the original recipient address appears in neither To nor Cc."
  [hdrs]
  (let [orig (some-> (original-recipient hdrs) str/lower-case)
        to   (some-> (get-header hdrs "To") str/lower-case)
        cc   (some-> (get-header hdrs "Cc") str/lower-case)]
    (boolean (and orig
                  (not (some-> to (str/includes? orig)))
                  (not (some-> cc (str/includes? orig)))))))

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
      (original-recipient-not-in-to-cc? hdrs)
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

(defn classify-source
  "Return the :name of the first matching source, or nil.
  Uses header-based matching only (List-Id, X-Original-To, Delivered-To)."
  [headers-edn sources]
  (some (fn [source]
          (when (match-source? headers-edn source) (:name source)))
        sources))

;; ---------------------------------------------------------------------------
;; Author resolution
;; ---------------------------------------------------------------------------

(defn- list-munged-from?
  "True when From appears to have been rewritten by the list MTA
  (Mailman/DMARC mitigation).  Mailman wraps the original display
  name as `Original Name via List-Display-Name`, so a literal
  ` via ` token in the From-name is the universal munging marker."
  [from-name]
  (boolean (and from-name (re-find #"(?i) via " from-name))))

(defn resolve-author
  "Effective author {:address :name} of an email.  When the From has
  been DMARC-munged by the list (detected by \" via \" in the display
  name) the real author is in Reply-To; otherwise From wins."
  [{:keys [from-address from-name reply-to]}]
  (let [rto-addr (:address (first reply-to))
        rto-name (:name    (first reply-to))]
    (if (and (list-munged-from? from-name)
             rto-addr
             (not= (some-> rto-addr str/lower-case)
                   (some-> from-address str/lower-case)))
      {:address rto-addr
       :name    (or rto-name from-name)}
      {:address from-address
       :name    from-name})))

;; ---------------------------------------------------------------------------
;; Label / command defaults and merge logic
;; ---------------------------------------------------------------------------

(def report-type-spec
  "Ordered spec of each report type.  Vector order drives subject
  detection precedence and per-type export iteration.  :type is the
  keyword identifier, :tags the default subject tokens, :plural the
  noun used in exports, :versioned? whether the bracket's last token
  is parsed as :version.  :patch has a dedicated parser in bark.detect."
  [{:type :bug          :tags ["BUG"]                :plural "bugs"}
   {:type :patch        :tags ["PATCH"]              :plural "patches"}
   {:type :request      :tags ["POLL" "TODO"]        :plural "requests"}
   {:type :announcement :tags ["ANN" "ANNOUNCEMENT"] :plural "announcements"}
   {:type :release      :tags ["REL" "RELEASE"]      :plural "releases"       :versioned? true}
   {:type :change       :tags ["CHG" "CHANGE"]       :plural "changes"        :versioned? true}])

(def default-labels (into {} (map (juxt :type :tags)) report-type-spec))

(def report-type-keywords (into #{} (map :type) report-type-spec))

(def type->plural (into {} (map (juxt :type :plural)) report-type-spec))

(def default-commands
  {:acked     ["Acked" "Confirmed" "Approved"]
   :owned     ["Owned"]
   :closed    ["Canceled" "Cancelled" "Closed" "Expired"
               "Resolved" "Applied" "Completed" "Fixed"]
   :urgent    ["Urgent"]
   :important ["Important"]})

(def close-reasons
  {"Canceled"  :canceled
   "Cancelled" :canceled
   "Expired"   :expired})

(defn bang-prefix
  "Regex fragment for the `!` prefix on Bark instructions.
  Required when `strict-syntax?` is true, optional otherwise."
  [strict-syntax?]
  (if strict-syntax? "!" "!?"))

(defn resolve-labels-map [source-cfg]
  (cond-> default-labels
    (:global-labels source-cfg) (merge (:global-labels source-cfg))
    (:labels source-cfg)        (merge (:labels source-cfg))))

(defn- validate-word-entry
  "Validate a :words entry. Must be a bare string."
  [entry]
  (if (string? entry)
    entry
    (throw (ex-info (str "Invalid :words entry: " (pr-str entry)
                         " (expected a bare string)")
                    {:entry entry}))))

(defn- extract-words-map
  "Return {cmd-id words} for entries that carry a :words vector."
  [commands-map]
  (reduce-kv (fn [acc k v]
               (if-let [w (:words v)] (assoc acc k w) acc))
             {} commands-map))

(defn- merged-raw-words
  "Return {cmd-id [strings]}.  Defaults merged first, then global, then
  local commands."
  [source-cfg]
  (merge default-commands
         (extract-words-map (:global-commands source-cfg))
         (extract-words-map (:commands source-cfg))))

(defn resolve-commands-map
  "Return {cmd-id [strings]} with every entry validated as a bare string."
  [source-cfg]
  (update-vals (merged-raw-words source-cfg)
               #(mapv validate-word-entry %)))

(defn- extract-overrides-map
  "Keep only :scope and :report-types for each command, dropping entries
  with no override."
  [commands-map]
  (reduce-kv (fn [acc k v]
               (let [overrides (select-keys v [:scope :report-types])]
                 (if (seq overrides) (assoc acc k overrides) acc)))
             {} commands-map))

(defn resolve-command-overrides [source-cfg]
  (merge (extract-overrides-map (:global-commands source-cfg))
         (extract-overrides-map (:commands source-cfg))))

(defn resolve-command-syntax
  "Return :loose or :strict for the source."
  [source-cfg]
  (case (:command-syntax source-cfg)
    nil     :loose
    :loose  :loose
    :strict :strict
    (throw (ex-info (str "Invalid :command-syntax: "
                         (pr-str (:command-syntax source-cfg))
                         " (expected :loose or :strict)")
                    {:value (:command-syntax source-cfg)}))))

(defn patch-triggers?
  "True when patches on this source auto-credit acked/owned on the
  parent bug/request and propagate :resolved closure.  Default true;
  source-level \":patch-triggers? false\" opts out."
  [source-cfg]
  (let [v (:patch-triggers? source-cfg)]
    (if (nil? v) true (boolean v))))

;; ---------------------------------------------------------------------------
;; Maintainer tenures (pure -- operate on a seq of tenure maps, no DB access)
;; ---------------------------------------------------------------------------
;;
;; A "tenure" is a map {:email str :from Date|nil :to Date|nil :order long|nil}
;; representing one contiguous period during which an address held maintainer
;; status on a source. :from nil = active since the beginning of time.
;; :to nil = the tenure is currently active. :order encodes the config order
;; used as a tie-break when computing the lead maintainer.

(defn active-tenures
  "Return tenures that are currently active (no :to)."
  [tenures]
  (filter #(nil? (:to %)) tenures))

(defn- tenure-sort-key [t]
  [(if-let [^Date f (:from t)] (.getTime f) 0)
   (or (:order t) Long/MAX_VALUE)
   (str/lower-case (or (:email t) ""))])

(defn lead-maintainer
  "Return the lower-cased email of the lead maintainer -- the active tenure
  with the earliest :from (nil sorts first), tie-broken by :order then email.
  Returns nil if no active tenure."
  [tenures]
  (:email (first (sort-by tenure-sort-key (active-tenures tenures)))))

(defn lead-maintainer?
  "True if addr is the current lead maintainer for this source."
  [tenures addr]
  (and addr
       (when-let [lead (lead-maintainer tenures)]
         (= (str/lower-case addr) lead))))

(defn maintainer?
  "True if `addr` has maintainer status.  2-arity: any active tenure;
  3-arity: tenure covering `as-of` (nil bounds = unbounded).  A nil
  `as-of` falls back to the 2-arity for emails missing a Date header."
  ([tenures addr]
   (and addr
        (let [a (str/lower-case addr)]
          (boolean (some #(and (= a (:email %)) (nil? (:to %))) tenures)))))
  ([tenures addr as-of]
   (if (nil? as-of)
     (maintainer? tenures addr)
     (and addr
          (let [a (str/lower-case addr)]
            (boolean
             (some (fn [{:keys [email from to]}]
                     (and (= a email)
                          (or (nil? from) (not (.before ^Date as-of ^Date from)))
                          (or (nil? to)   (.before ^Date as-of ^Date to))))
                   tenures)))))))

;; ---------------------------------------------------------------------------
;; Config and source-map
;; ---------------------------------------------------------------------------

(def config-edn-readers
  "EDN reader tags accepted in `config.edn`.

   `#bark/env \"VAR\"` resolves to the named environment variable.
   Raises if the variable is unset -- prefer a clear startup failure
   over silently using a nil password."
  {'bark/env (fn [var-name]
               (let [k (str var-name)]
                 (or (System/getenv k)
                     (throw (ex-info (str "Environment variable not set: " k)
                                     {:var k})))))})

(defn- resolve-password-files
  "Replace every `{:password-file path}` in the config with the trimmed
   contents of the file.  Applied across :mailboxes and :notifications
   :smtp.  Raises if both `:password` and `:password-file` are set, or
   if the referenced file is missing."
  [config]
  (walk/postwalk
   (fn [x]
     (if-let [pf (and (map? x) (:password-file x))]
       (do
         (when (:password x)
           (throw (ex-info ":password and :password-file are mutually exclusive"
                           {:offending x})))
         (let [f (io/file (expand-home pf))]
           (when-not (.exists f)
             (throw (ex-info (str ":password-file not found: " pf)
                             {:path pf})))
           (-> x (dissoc :password-file)
               (assoc :password (str/trim (slurp f))))))
       x))
   config))

(defn load-config
  "Load config.edn if it exists, or nil.  With no args, consults the
  BARK_CONFIG env var, falling back to ./config.edn -- so all bb
  scripts honor a single override point without per-script flags.

  Resolves `#bark/env \"VAR\"` reader tags and `:password-file` entries
  before returning, so callers see a fully-materialised config."
  ([] (load-config (or (System/getenv "BARK_CONFIG") "config.edn")))
  ([path]
   (let [f (io/file path)]
     (when (.exists f)
       (-> (edn/read-string {:readers config-edn-readers} (slurp f))
           resolve-password-files)))))

(defn inline-password-locations
  "Read config.edn as it was written -- without resolving #bark/env or
   :password-file -- and return a vector of human-readable location
   labels where :password is a plain inline string.  Used by
   `bb test-config` to remind operators that credentials can live in
   a sidecar file or environment variable instead."
  [path]
  (let [f (io/file path)]
    (when (.exists f)
      (let [env-marker ::env
            raw        (edn/read-string
                        {:readers {'bark/env (constantly env-marker)}}
                        (slurp f))
            inline?    #(and (map? %)
                             (string? (:password %))
                             (not (:password-file %)))
            mailboxes  (for [mb (:mailboxes raw) :when (inline? mb)]
                         (str "mailbox " (pr-str (:name mb))))
            smtp       (when (inline? (get-in raw [:notifications :smtp]))
                         "notifications :smtp")]
        (vec (cond-> (vec mailboxes) smtp (conj smtp)))))))

(defn load-mailmap
  "Load ./mailmap.edn (shape `{\"Canonical Name\" [emails…]}`) and return
  `{email-lc -> canonical-name}` (inverted, flat).  Returns `{}` if
  the file is absent.  Export-only -- never read on the ingest path."
  ([] (load-mailmap "mailmap.edn"))
  ([path]
   (let [f (io/file path)]
     (if (.exists f)
       (let [m (edn/read-string (slurp f))]
         (reduce-kv
          (fn [acc nm emails]
            (reduce (fn [a e] (assoc a (str/lower-case e) nm))
                    acc
                    emails))
          {} m))
       {}))))

(defn db-path
  "Resolve the Datalevin DB path: prefer :db {:path …} from the
  config, fall back to BARK_DB env var, then default \"data/bark-db\"."
  [config]
  (or (get-in config [:db :path])
      (System/getenv "BARK_DB")
      "data/bark-db"))

(defn build-source-map
  "Build source-name -> config map from config."
  [config]
  (let [global-st       (:labels config)
        global-cmd      (:commands config)
        global-ef       (:export-formats config)
        global-expiry   (:expiry config)
        global-rt       (:report-types config)
        global-cs       (:command-syntax config)]
    (into {}
          (keep (fn [src]
                 (if-let [stype (source-type src)]
                   [(:name src)
                    (merge {:source-type stype}
                           (select-keys src [:list :alias :to :commands :labels :notifications
                                             :archive-format-string :list-archive :base-url
                                             :maintainers :awaiting-delay :topics-filter :periods])
                           (when global-st {:global-labels global-st})
                           (when global-cmd {:global-commands global-cmd})
                           {:export-formats (set (or (:export-formats src) global-ef ["json" "org" "rss"]))
                            :report-types (when-let [rt (or (:report-types src) global-rt)]
                                            (set (map keyword rt)))
                            :expiry (or (:expiry src) global-expiry)
                            :command-syntax (or (:command-syntax src) global-cs :loose)})]
                   (log/warn "Source has no :list, :alias, or :to key -- skipping:" (:name src)))))
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
    (if (coll? d) (count d) 0)))

;; ---------------------------------------------------------------------------
;; Canonical report pull pattern
;; ---------------------------------------------------------------------------

(def report-pull-pattern
  ;; Each :report/<state> ref pulls its email's author + date so
  ;; consumers can show "set by X on Y" without a second query.
  ;; Relations come via :rel/_from and :rel/_to (both directions are
  ;; stored for asymmetric kinds, consumer picks the relevant side).
  '[:db/id :report/type :report/version
    :report/patch-seq :report/patch-source :report/message-id
    {:report/acked [:email/author-address :email/date-sent]}
    {:report/owned [:email/author-address :email/date-sent]}
    {:report/closed [:email/author-address :email/date-sent]}
    {:report/urgent [:email/author-address :email/date-sent]}
    {:report/important [:email/author-address :email/date-sent]}
    :report/acked-address :report/owned-address :report/closed-address
    :report/urgent-address :report/important-address
    :report/close-reason
    {:report/topic [:email/author-address :email/date-sent]}
    :report/topic-value
    {:report/deadline [:email/author-address :email/date-sent]}
    :report/deadline-value
    {:report/expiry [:email/author-address :email/date-sent]}
    :report/expiry-value
    :report/has-ics :report/has-text-attachments
    :report/last-activity :report/last-activity-address :report/descendants :report/digested-at :report/updated-at
    {:rel/_from [:rel/kind :rel/active? :rel/setter :rel/posed-at :rel/value
                 {:rel/to [:db/id :report/type :report/message-id
                           {:report/email [:email/subject]}]}]}
    {:rel/_to [:rel/kind :rel/active? :rel/setter :rel/posed-at :rel/value
               {:rel/from [:db/id :report/type :report/message-id
                           {:report/email [:email/subject]}]}]}
    {:report/series [:series/id :series/expected :series/closed
                     {:series/patches [:db/id]}
                     {:series/cover-letter [:email/message-id]}]}
    {:report/patches [:patch/filename :patch/source :patch/text
                      :patch/author :patch/subject :patch/date]}
    {:report/email [:email/subject :email/author-address :email/author-name
                    :email/from-address :email/from-name
                    :email/date-sent :email/source :email/id
                    :email/headers-edn]}])

;; Pull pattern for attachment-heavy operations (event/text export).
;; Used by bb scripts to fetch attachment data only for reports that need it.
(def attachment-pull-pattern
  '[:db/id :report/type :report/message-id
    {:report/email [:email/body-text
                    {:email/attachments [:attachment/filename :attachment/content-type
                                         :attachment/size :attachment/data]}]}])

;; Pull pattern for maintainer tenures. Shared between JVM (roles.clj) and
;; bb (scripts/bark/common_bb.clj) so both sides consume the same attribute list.
(def tenure-pull-pattern
  '[:db/id :maint-tenure/email :maint-tenure/from :maint-tenure/to :maint-tenure/order])

(defn tenure-map
  "Convert a raw Datalevin pull result into a tenure map."
  [m]
  {:eid   (:db/id m)
   :email (:maint-tenure/email m)
   :from  (:maint-tenure/from m)
   :to    (:maint-tenure/to m)
   :order (:maint-tenure/order m)})

;; ---------------------------------------------------------------------------
;; Vote helpers (pure -- no datalevin dependency)
;; ---------------------------------------------------------------------------

(defn votes-by-report
  "Group raw vote tuples into {report-eid [{:value :up :voter \"a@b\" :email-mid \"<…>\"} …]}.
  Expects tuples of [report-eid value voter email-mid], e.g. from a Datalog query."
  [vote-tuples]
  (reduce (fn [acc [r val voter emid]]
            (update acc r (fnil conj []) {:value val :voter voter :email-mid emid}))
          {}
          vote-tuples))

(defn vote-counts
  "Derive {:up n :down n :null n} from a seq of vote maps."
  [votes]
  (reduce (fn [acc {:keys [value]}]
            (update acc value (fnil inc 0)))
          {:up 0 :down 0 :null 0}
          votes))

;; ---------------------------------------------------------------------------
;; CLI arg parsing
;; ---------------------------------------------------------------------------

(defn- flag-like?
  "True if token starts with '-' (looks like a flag rather than a value)."
  [s]
  (and s (str/starts-with? s "-")))

(defn- check-flag-value
  "Validate that flag `a` has a proper value `v`.
  Returns :ok, :missing, or :flag-as-value."
  [a v]
  (cond
    (nil? v)       (do (log/warn "Flag" a "requires a value") :missing)
    (flag-like? v) (do (log/warn "Flag" a "followed by flag" v "-- missing value?") :flag-as-value)
    :else          :ok))

(def ^:private valued-flags
  "Map of flag names to [opt-key transform-fn]. Flags that share
  short and long forms map both names to the same entry."
  {"-o" [:out-file identity] "--output" [:out-file identity]
   "-n" [:source-name identity] "--source" [:source-name identity]
   "--json" [:json-file identity] "--dir" [:out-dir identity]
   "--theme" [:theme identity]
   "-p" [:min-priority parse-long] "--min-priority" [:min-priority parse-long]
   "-s" [:min-status parse-long] "--min-status" [:min-status parse-long]
   "--page-size" [:page-size parse-long]
   "--closed-retention" [:closed-retention identity]
   "--topics-filter" [:topics-filter identity]})

(defn parse-cli-args
  "Parse common CLI flags into a map.
  Recognises: -o/--output, -n/--source, -p/--min-priority, -s/--min-status,
  --json, --dir, --force, --only-open, --theme, --page-size, --closed-retention,
  --topics-filter.
  Any leading non-flag token is captured as :format.
  Warns when a valued flag is missing its argument or followed by another flag."
  [args]
  (loop [opts {} [a & [v & r :as more]] args]
    (cond
      (nil? a)                        opts
      (= "--force" a)                 (recur (assoc opts :force-all? true) more)
      (= "--only-open" a)             (recur (assoc opts :only-open? true) more)
      (= "--index-only" a)            (recur (assoc opts :format "root") more)
      (contains? valued-flags a)      (let [[k xf] (valued-flags a)]
                                        (case (check-flag-value a v)
                                          :ok            (recur (assoc opts k (xf v)) r)
                                          :flag-as-value (recur opts more)
                                          :missing       opts))
      (not (:format opts))            (recur (assoc opts :format a) more)
      :else                           (do (when (flag-like? a)
                                                (log/debug "Ignoring unrecognized flag:" a))
                                          (recur opts more)))))
