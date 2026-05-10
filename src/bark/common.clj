;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.common
  "Shared pure utilities for BARK. No datalevin dependency -- loadable by both
  JVM and Babashka."
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
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
  "0.9.0")

(def bark-schema
  (edn/read-string (slurp (io/resource "bark-schema.edn"))))

(def failures-file-path
  "Path to the shared command-failures EDN file.
  Written by the JVM (`bark.commands/record-failure!`) and read by
  `bark-notify` and `bark-maintenance` to surface denied/failed
  commands to subscribers and operators."
  "public/.failures.edn")

(defn read-failures-file
  "Read the failures EDN file at `path`, returning a vector.
  Returns [] when the file does not exist. On parse failure, invokes
  `on-error` with the exception (for logging) and returns []."
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
  "Human-readable labels for command-failure :reason keys.
  Shared between the notifier and the maintenance CLI so both surface
  the same phrasing."
  {:unknown-target     "unknown target"
   :insufficient-scope "insufficient permissions"})

;; ---------------------------------------------------------------------------
;; Pure utilities
;; ---------------------------------------------------------------------------

(defn expand-home
  "Expand a leading ~/ to the user's home directory.
  Returns the string unchanged when no leading ~/ is present, or when
  the value is nil."
  [p]
  (if (and (string? p) (str/starts-with? p "~/"))
    (str (System/getProperty "user.home") (subs p 1))
    p))

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
  (if-let [idx (str/index-of text "\n-- \n")]
    (subs text 0 idx)
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

(defn extract-bracketed-id
  "Extract the first `<addr@domain>` token from a header value.
  Falls back to the trimmed input when no bracketed token is present,
  so malformed headers are not silently dropped.  Returns nil for nil
  or blank input.

  Normalizes values that reference Message-Ids (Message-Id itself,
  In-Reply-To) so that threading lookups match even when the raw
  header carries parenthetical comments, folded whitespace, or extra
  padding -- cases that show up on the Maildir read path where raw
  MIME bytes are parsed directly rather than via IMAP ENVELOPE."
  [v]
  (when v
    (let [s (if (vector? v) (first v) (str v))]
      (when-not (str/blank? s)
        (or (first (re-seq #"<[^>]+>" s))
            (str/trim s))))))

(defn extract-in-reply-to
  "Extract In-Reply-To message-id from a headers map (raw or parsed).
  Handles both string and vector values."
  [headers]
  (extract-bracketed-id (get-header headers "In-Reply-To")))

;; LMDB key max is 511 bytes; Datalevin's AVE-index encoding (type tag
;; + hex of the value + metadata) roughly doubles the raw string and
;; adds padding.  A conservative cap of 200 chars on the raw mid keeps
;; us comfortably under the limit and rejects pathological mids (some
;; ProtonMail addresses produce 130+ char message-ids that trigger
;; MDB_BAD_VALSIZE on insert/lookup).  Any code that uses a mid as a
;; lookup key or unique attribute MUST filter through `indexable-mid?`.
(def ^:const max-indexable-mid-length 200)

(defn indexable-mid?
  "True iff `mid` is non-nil and short enough to fit in the LMDB AVE
  index for `:email/message-id` / `:report/message-id` lookups.
  Use to short-circuit queries that would otherwise raise
  MDB_BAD_VALSIZE on oversized mids."
  [mid]
  (and (string? mid) (<= (count mid) max-indexable-mid-length)))

(defn ancestor-mids-from
  "Pure helper -- ordered vector of ancestor message-ids from the
  References (space-separated string) and In-Reply-To values.  Order
  follows RFC 2822: root first, immediate parent last.  Mids exceeding
  the LMDB index limit are filtered out -- they cannot be looked up
  anyway, so retaining them in threading queries is pointless."
  [references in-reply-to]
  (let [refs (if (string? references) (re-seq #"<[^>]+>" references) [])
        all  (if (and in-reply-to (not (some #{in-reply-to} refs)))
               (conj (vec refs) in-reply-to)
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
  "Return {:address ... :name ...} for the effective author of an
  email.  When the From header has been rewritten by the list (Mailman
  / DMARC munging -- detected via the ` via ` pattern in the display
  name), the real author lives in Reply-To.  Falls back to From in all
  other cases (including the common Reply-To-different-from-From use
  for personal correspondence).

  `reply-to` is a vector of {:name :address} maps as produced by
  mailseq (may be empty/nil).  Returns {:address from-address :name
  from-name} when no override applies."
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

(def default-labels
  {:bug          ["BUG"]
   :patch        ["PATCH"]
   :request      ["POLL" "TODO"]
   :announcement ["ANN" "ANNOUNCEMENT"]
   :release      ["REL" "RELEASE"]
   :change       ["CHG" "CHANGE"]})

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
  "True if addr has maintainer status on this source.
  2-arity: any currently-active tenure matches.
  3-arity: the tenure covering `as-of` matches (from <= as-of < to,
  with nil bounds meaning unbounded)."
  ([tenures addr]
   (and addr
        (let [a (str/lower-case addr)]
          (boolean (some #(and (= a (:email %)) (nil? (:to %))) tenures)))))
  ([tenures addr as-of]
   (and addr
        (let [a (str/lower-case addr)]
          (boolean
           (some (fn [{:keys [email from to]}]
                   (and (= a email)
                        (or (nil? from) (not (.before ^Date as-of ^Date from)))
                        (or (nil? to)   (.before ^Date as-of ^Date to))))
                 tenures))))))

;; ---------------------------------------------------------------------------
;; Config and source-map
;; ---------------------------------------------------------------------------

(defn load-config
  "Load config.edn if it exists, or nil.  With no args, consults the
  BARK_CONFIG env var, falling back to ./config.edn -- so all bb
  scripts honor a single override point without per-script flags."
  ([] (load-config (or (System/getenv "BARK_CONFIG") "config.edn")))
  ([path]
   (let [f (io/file path)]
     (when (.exists f)
       (edn/read-string (slurp f))))))

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
  ;; Each `:report/<state>` ref carries :email/author-address and
  ;; :email/date-sent so consumers can display "set by X on Y"
  ;; without a second query.  Paired `-value` attrs carry the business
  ;; datum posed alongside the setter identity.
  ;;
  ;; Qualified relations (:resolves, :supersedes, :duplicates,
  ;; :related-to) are pulled via the reverse refs :rel/_from and
  ;; :rel/_to.  Since asymmetric kinds store two datoms (one per
  ;; direction), each report sees both its outgoing and incoming sides;
  ;; consumers pick whichever is appropriate for display.
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
