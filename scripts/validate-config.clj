#!/usr/bin/env bb

;; validate-config.clj — Validate config.edn against spec.
;;
;; Usage:
;;   bb scripts/validate-config.clj [path]
;;   bb config [path]
;;
;; Defaults to ./config.edn if no path given.

(require '[clojure.spec.alpha :as s]
         '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[taoensso.timbre :as log]
         '[bark.common :as common]
         '[bark.commands.registry :as reg])

;; ---------------------------------------------------------------------------
;; Specs
;; ---------------------------------------------------------------------------

;; Primitives
(s/def ::non-blank-string (s/and string? (complement str/blank?)))
(s/def ::pos-int (s/and int? pos?))

;; Email address (basic check: contains @)
(s/def ::email (s/and ::non-blank-string #(str/includes? % "@")))

;; Mailbox connection (IMAP or Maildir)
(s/def :mailbox/type #{:imap :maildir})
(s/def :mailbox/host ::non-blank-string)
(s/def :mailbox/port ::pos-int)
(s/def :mailbox/ssl boolean?)
(s/def :mailbox/user ::non-blank-string)
(s/def :mailbox/password ::non-blank-string)
(s/def :mailbox/oauth2-token ::non-blank-string)
(s/def :mailbox/folder ::non-blank-string)
(s/def :mailbox/path ::non-blank-string)

(s/def :bark/mailbox
  (s/and (s/keys :req-un [:mailbox/type]
                 :opt-un [:mailbox/host :mailbox/port :mailbox/ssl
                          :mailbox/user :mailbox/password :mailbox/oauth2-token
                          :mailbox/folder :mailbox/path])
         (fn [m]
           (case (:type m)
             :imap    (and (:host m) (:user m)
                           (or (:password m) (:oauth2-token m)))
             :maildir (:path m)
             false))))

;; Source match spec
(s/def :match/list-id
  (s/and ::non-blank-string
         ;; Must be the bare identifier, not the full header with angle brackets
         (complement #(re-find #"[<>]" %))))
(s/def :match/alias ::non-blank-string)
(s/def :match/to ::non-blank-string)

;; Source — exactly one of :list, :alias, :to
(defn- exactly-one-source-type? [src]
  (= 1 (count (filter some? (map src [:list :alias :to])))))

;; Source
(s/def :source/name
  (s/and ::non-blank-string
         #(re-matches #"[a-zA-Z0-9][a-zA-Z0-9 ._-]*" %)))
(s/def :source/list :match/list-id)
(s/def :source/alias :match/alias)
(s/def :source/to :match/to)
(s/def :source/list-archive (s/and ::non-blank-string #(re-find #"^https?://" %)))
(s/def :source/base-url ::non-blank-string)
(s/def :source/archive-format-string (s/and ::non-blank-string #(str/includes? % "%s")))

;; Per-source export overrides
(s/def ::export-format #{"json" "rss" "org" "html" "stats" "patches" "text" "events"})
(s/def :source/export-formats (s/coll-of ::export-format :kind vector? :min-count 1))
(s/def :bark/export-formats :source/export-formats)

;; Per-source topics filter
(s/def :source/topics-filter (s/coll-of ::non-blank-string :kind vector? :min-count 1))

;; Per-source notifications (optional) — override global notification gate
(s/def :source-notif/enable boolean?)
(s/def :source/notifications (s/keys :req-un [:source-notif/enable]))

;; Per-source maintainers (optional) — seed maintainers with since-dates.
;; Directive "Add/Remove maintainer:" overrides these at runtime.
(s/def :maintainer/email ::email)
(s/def :maintainer/since (s/and ::non-blank-string #(re-matches #"\d{4}-\d{2}-\d{2}" %)))
(s/def ::maintainer-entry (s/keys :req-un [:maintainer/email]
                                  :opt-un [:maintainer/since]))
(s/def :source/maintainers (s/coll-of ::maintainer-entry :kind vector? :min-count 1))

(s/def ::source
  (s/and (s/keys :req-un [:source/name]
                 :opt-un [:source/list :source/alias :source/to
                          :source/list-archive :source/base-url
                          :source/archive-format-string
                          :source/commands :source/labels
                          :source/report-types
                          :source/maintainers :source/notifications
                          :source/expiry :source/awaiting-delay
                          :source/export-formats :source/topics-filter
                          :source/command-syntax])
         exactly-one-source-type?))

(s/def :bark/sources
  (s/and (s/coll-of ::source :kind vector? :min-count 1)
         (fn [srcs] (= (count srcs) (count (distinct (map :name srcs)))))))

;; DB
(s/def :db/path ::non-blank-string)
(s/def :bark/db (s/keys :req-un [:db/path]))

;; Ingest
(s/def :ingest/initial-fetch
  (s/or :count pos-int?
        :date  (s/and string? #(re-matches #"\d{4}-\d{2}-\d{2}" %))
        :duration (s/and string? #(re-seq #"\d+\s*[ydwm]" %))))
(s/def :ingest/max-size pos-int?)
(s/def :ingest/max-attachment-size pos-int?)
(s/def :bark/ingest (s/keys :opt-un [:ingest/initial-fetch
                                     :ingest/max-size
                                     :ingest/max-attachment-size]))

;; Theme (optional, global only)
(s/def :bark/theme ::non-blank-string)

;; SMTP
(s/def :smtp/host ::non-blank-string)
(s/def :smtp/port ::pos-int)
(s/def :smtp/user ::non-blank-string)
(s/def :smtp/password ::non-blank-string)
(s/def :smtp/from ::email)
(s/def :smtp/tls boolean?)

(s/def :notif/smtp (s/keys :req-un [:smtp/host :smtp/port :smtp/user :smtp/password :smtp/from]
                           :opt-un [:smtp/tls]))
(s/def :notif/enabled boolean?)
(s/def :bark/notifications (s/keys :req-un [:notif/enabled]
                                   :opt-un [:notif/smtp]))

;; Valid report type keywords (shared by commands, export, and report-types specs)
(def valid-report-types #{:bug :patch :request :announcement :release :change})

;; Command IDs (for extended :commands format)
(def valid-command-ids
  #{:acked :owned :closed :urgent :important
    :acked-by :owned-by :closed-by :urgent-by :important-by
    :unacked :unowned :unclosed :unurgent :unimportant
    :deadline :undeadline :expiry :unexpiry
    :topic :untopic :superseded-by :unsuperseded})

;; The :setter-or-maintainer scope is only valid on the unset-style
;; directives whose target attribute is tracked by a ref to the
;; pose-email.  The authoritative set is derived from the shared
;; `bark.commands.registry`.
(def valid-plain-scopes  #{:user :maintainer})
(def valid-setter-scopes #{:user :maintainer :setter-or-maintainer})

;; Per-source commands (optional).
;; Values are maps with any of :words, :scope, :report-types (at least one).
;; Each word in :words is either a bare string (always active) or a
;; [string {:since "yyyy-MM-dd" :until "yyyy-MM-dd"}] tuple restricting
;; it to a half-open date window. :since/:until can be omitted.
(s/def ::iso-date (s/and ::non-blank-string #(re-matches #"\d{4}-\d{2}-\d{2}" %)))
(s/def ::since ::iso-date)
(s/def ::until ::iso-date)
(s/def ::word-window
  (s/and (s/keys :opt-un [::since ::until])
         (fn [{s :since u :until}]
           (or (nil? s) (nil? u) (neg? (compare s u))))))
(s/def ::trigger-word
  (s/or :bare     ::non-blank-string
        :windowed (s/tuple ::non-blank-string ::word-window)))
(s/def ::trigger-words (s/coll-of ::trigger-word :kind vector? :min-count 1))
(s/def ::command-scope valid-setter-scopes)
(s/def ::command-report-types (s/coll-of valid-report-types :kind set? :min-count 1))

(defn valid-command-value?
  "Validate a single command override. `cmd-id` is needed because
  :setter-or-maintainer is only allowed on the commands listed in
  `reg/setter-scoped-command-ids`."
  [cmd-id v]
  (and (map? v)
       (seq v)
       (every? #{:words :scope :report-types} (keys v))
       (some #{:words :scope :report-types} (keys v))
       (if (:words v) (s/valid? ::trigger-words (:words v)) true)
       (if-let [sc (:scope v)]
         (if (contains? reg/setter-scoped-command-ids cmd-id)
           (contains? valid-setter-scopes sc)
           (contains? valid-plain-scopes sc))
         true)
       (if (:report-types v) (s/valid? ::command-report-types (:report-types v)) true)))

(s/def ::commands-map
  (s/and (s/map-of valid-command-ids any?)
         #(every? (fn [[k v]] (valid-command-value? k v)) %)))

(s/def :source/commands ::commands-map)

;; Global commands (optional) — same shape as per-source
(s/def :bark/commands ::commands-map)

;; Command aliases (optional) — maps old syntax to new for backward compatibility
;; e.g. {"Unacked" "Not acked", "Unexpiry" "No expiry"}
(s/def ::command-aliases (s/map-of ::non-blank-string ::non-blank-string))
(s/def :bark/command-aliases ::command-aliases)

;; Subject triggers: map of report-type keyword -> vector of tag strings
;; e.g. {:bug ["BUG" "DEFECT"] :request ["POLL" "FR" "TODO"]}
(s/def ::label-tags (s/coll-of ::non-blank-string :kind vector? :min-count 1))
(s/def ::labels
  (s/map-of #{:bug :patch :request :announcement :release :change}
            ::label-tags))
(s/def :source/labels ::labels)
(s/def :bark/labels ::labels)

;; Report types: filters which report types are detected at ingest
;; AND exported. Default: all types. Per-source overrides global.
(s/def ::report-types
  (s/coll-of valid-report-types :kind set? :min-count 1))
(s/def :source/report-types ::report-types)
(s/def :bark/report-types ::report-types)

;; Expiry rules (optional)
;; Each report type maps to a rule map with :inactive-after and optional conditions.
(s/def :expiry/inactive-after (s/or :deadline #{:deadline}
                                    :date (s/and ::non-blank-string #(re-matches #"\d{4}-\d{2}-\d{2}" %))
                                    :string (s/and ::non-blank-string #(re-seq #"\d+\s*[ydwm]" %))
                                    :int pos-int?))
(s/def :expiry/max-status (s/and int? #(<= 0 % 3)))
(s/def :expiry/max-priority (s/and int? #(<= 0 % 3)))

(s/def ::expiry-rule
  (s/keys :req-un [:expiry/inactive-after]
          :opt-un [:expiry/max-status :expiry/max-priority]))

(s/def ::expiry
  (s/map-of valid-report-types ::expiry-rule))
(s/def :source/expiry ::expiry)
(s/def :bark/expiry ::expiry)

;; Logging (optional)
(s/def :logging/file ::non-blank-string)
(s/def :logging/level #{:debug :info :warn :error})
(s/def :logging/max-size (s/and ::non-blank-string #(re-matches #"\d+[KMG]B" (str/upper-case (str/trim %)))))
(s/def :logging/backlog ::pos-int)

;; Logging :email — sends log entries via :notifications :smtp
(s/def :log-email/to ::email)
(s/def :log-email/level #{:debug :info :warn :error})
(s/def :logging/email (s/keys :req-un [:log-email/to]
                              :opt-un [:log-email/level]))

(s/def :bark/logging (s/keys :opt-un [:logging/file :logging/level :logging/max-size
                                      :logging/backlog :logging/email]))

;; Awaiting-reply delay — same units as parse-duration-str (d/w/m/y)
(s/def :bark/awaiting-delay
  (s/and ::non-blank-string #(re-matches #"\d+[dwmy](?:\s+\d+[dwmy])*" %)))
(s/def :source/awaiting-delay :bark/awaiting-delay)

;; Command syntax mode: :loose (default — ! is optional on every Bark
;; instruction) or :strict (! required on every Bark instruction).
(s/def :bark/command-syntax #{:loose :strict})
(s/def :source/command-syntax :bark/command-syntax)

;; Top-level config
(s/def ::config
  (s/keys :req-un [:bark/mailbox :bark/sources :bark/db]
          :opt-un [:bark/ingest :bark/notifications :bark/labels
                   :bark/commands :bark/command-aliases
                   :bark/report-types :bark/awaiting-delay
                   :bark/expiry :bark/logging
                   :bark/command-syntax :bark/theme
                   :bark/export-formats]))

;; ---------------------------------------------------------------------------
;; Validation
;; ---------------------------------------------------------------------------

(defn- commands-map-errors
  "Walk a :commands (or :global-commands) map and return a seq of
  human-readable error strings.  Used to surface precise, actionable
  messages before falling back to `s/explain-str`."
  [where commands-map]
  (when (map? commands-map)
    (for [[cmd-id v] commands-map
          :let [errs (cond
                       (not (contains? valid-command-ids cmd-id))
                       [(str "unknown command id " (pr-str cmd-id))]

                       (not (map? v))
                       [(str "expected a map with any of :words, :scope, "
                             ":report-types, got " (pr-str v))]

                       (empty? v)
                       [(str "expected at least one of :words, :scope, "
                             ":report-types")]

                       :else
                       (let [bad-keys   (remove #{:words :scope :report-types} (keys v))
                             sc         (:scope v)
                             allows-s-o-m? (contains? reg/setter-scoped-command-ids cmd-id)
                             allowed-scopes (if allows-s-o-m?
                                              valid-setter-scopes
                                              valid-plain-scopes)]
                         (concat
                          (when (seq bad-keys)
                            [(str "unknown key(s): "
                                  (str/join ", " (map pr-str bad-keys)))])
                          (when (and sc (not (contains? allowed-scopes sc)))
                            [(str ":scope " (pr-str sc)
                                  " is not valid for " (pr-str cmd-id)
                                  ". Valid values: "
                                  (str/join ", "
                                            (map pr-str (sort allowed-scopes))))]))))]
          err errs]
      (str where " " (pr-str cmd-id) ": " err))))

(defn- pre-check-commands
  "Return a seq of human-readable errors for all :commands and
  :global-commands maps in the config, or nil if everything is fine."
  [config]
  (let [errs (concat (commands-map-errors ":global-commands" (:global-commands config))
                     (mapcat (fn [src]
                               (commands-map-errors
                                (str ":sources [" (pr-str (:name src)) "] :commands")
                                (:commands src)))
                             (:sources config)))]
    (seq errs)))

(defn validate-config [config]
  (if-let [errs (pre-check-commands config)]
    {:valid? false
     :explanation (str/join "\n" errs)}
    (if (s/valid? ::config config)
      (cond-> {:valid? true}
        (and (get-in config [:logging :email])
             (not (get-in config [:notifications :smtp])))
        (assoc :warnings ["Logging :email is configured but :notifications :smtp is absent."]))
      {:valid? false
       :explanation (s/explain-str ::config config)})))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [path (or (first *command-line-args*) "config.edn")
      file (clojure.java.io/file path)]
  (if-not (.exists file)
    (do (log/error "Config file not found:" path)
        (System/exit 1))
    (let [config (try
                   (edn/read-string (slurp file))
                   (catch Exception e
                     (log/error "Invalid EDN:" (.getMessage e))
                     (System/exit 1)))
          result (validate-config config)]
      (if (:valid? result)
        (do (log/info "✓" path "is valid.")
            (let [mb (:mailbox config)]
              (log/info "  Mailbox:" (pr-str (:type mb))
                        (case (:type mb)
                          :imap    (str (:user mb) "@" (:host mb) "/" (or (:folder mb) "INBOX"))
                          :maildir (str (:path mb) "/" (or (:folder mb) "INBOX"))
                          "")))
            (log/info "  Sources:" (count (:sources config)))
            (doseq [src (:sources config)]
              (let [parts (cond-> []
                            (:list src)          (conj (str "(list: " (:list src) ")"))
                            (:alias src)         (conj (str "(alias: " (:alias src) ")"))
                            (:to src)            (conj (str "(mailbox: " (:to src) ")"))
                            (:list-archive src)  (conj (str "archive: " (:list-archive src)))
                            (:report-types src)  (conj (str "report-types: " (pr-str (:report-types src))))
                            (:command-syntax src) (conj (str "command-syntax: " (name (:command-syntax src))))
                            (seq (:maintainers src))
                            (conj (str "maintainers: "
                                       (str/join ", "
                                                 (map #(str (:email %)
                                                            (when (:since %) (str " (since " (:since %) ")")))
                                                      (:maintainers src)))))
                            (some? (get-in src [:notifications :enable]))
                            (conj (str "notify: " (get-in src [:notifications :enable]))))]
                (log/info "    -" (:name src) (str/join " " parts))))
            (log/info "  DB path:" (get-in config [:db :path]))
            (when-let [ingest (:ingest config)]
              (let [v (or (:initial-fetch ingest) 50)]
                (log/info "  Initial fetch:" (cond
                                               (int? v) (str v " msgs")
                                               (string? v) v))))
            (when-let [notif (:notifications config)]
              (log/info "  Notifications:" (if (:enabled notif) "enabled" "disabled"))
              (when-let [smtp (:smtp notif)]
                (log/info "  SMTP:" (str (:user smtp) "@" (:host smtp)))))
            (when-let [rt (:report-types config)]
              (log/info "  Report types:" (pr-str rt)))
            (when-let [cs (:command-syntax config)]
              (log/info "  Command syntax (global):" (name cs)))
            (when-let [logging (:logging config)]
              (when (:file logging)
                (log/info "  Log file:" (:file logging)
                          "level:" (or (:level logging) :warn)))
              (when-let [em (:email logging)]
                (log/info "  Log email:" (:to em)
                          "level:" (or (:level em) :error))))
            (doseq [w (:warnings result)]
              (log/warn "⚠" w)))
        (do (log/error "✗" path "is invalid:")
            (log/error (:explanation result))
            (System/exit 1))))))
