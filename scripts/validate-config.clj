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
         '[taoensso.timbre :as log])

;; ---------------------------------------------------------------------------
;; Specs
;; ---------------------------------------------------------------------------

;; Primitives
(s/def ::non-blank-string (s/and string? (complement str/blank?)))
(s/def ::pos-int (s/and int? pos?))

;; Email address (basic check: contains @)
(s/def ::email (s/and ::non-blank-string #(str/includes? % "@")))

;; Admin
(s/def :bark/admin ::email)

;; IMAP connection
(s/def :imap/host ::non-blank-string)
(s/def :imap/port ::pos-int)
(s/def :imap/ssl boolean?)
(s/def :imap/user ::non-blank-string)
(s/def :imap/password ::non-blank-string)
(s/def :imap/oauth2-token ::non-blank-string)
(s/def :imap/folder ::non-blank-string)

(s/def :bark/imap
  (s/and (s/keys :req-un [:imap/host :imap/user :imap/folder]
                 :opt-un [:imap/port :imap/ssl :imap/password :imap/oauth2-token])
         (fn [m] (or (:password m) (:oauth2-token m)))))

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
(s/def :source/admin ::email)
(s/def :source/list-archive (s/and ::non-blank-string #(re-find #"^https?://" %)))
(s/def :source/base-url ::non-blank-string)

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
                          :source/admin
                          :source/list-archive :source/commands :source/labels
                          :source/bark-path :source/export-reports
                          :source/report-types :source/maintainers
                          :source/notifications :source/expiry])
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
(s/def :bark/ingest (s/keys :opt-un [:ingest/initial-fetch]))

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
    :deadline :undeadline :topic})

;; Per-source commands (optional)
;; Values can be vectors (word lists) or maps with
;; :words, :scope, :report-types overrides.
(s/def ::trigger-words (s/coll-of ::non-blank-string :kind vector? :min-count 1))
(s/def ::command-scope #{:user :maintainer})
(s/def ::command-report-types (s/coll-of valid-report-types :kind set? :min-count 1))
(s/def ::command-entry
  (s/or :words-only ::trigger-words
        :extended   (s/keys :opt-un [::trigger-words ::command-scope ::command-report-types])))
;; Normalize keys: the extended map uses :words, :scope, :report-types
(s/def ::command-entry-map
  (s/and map?
         (s/keys :opt-un [:cmd/words :cmd/scope :cmd/report-types])))

(defn valid-command-value? [v]
  (or (and (vector? v) (s/valid? ::trigger-words v))
      (and (map? v)
           (every? #{:words :scope :report-types} (keys v))
           (if (:words v) (s/valid? ::trigger-words (:words v)) true)
           (if (:scope v) (contains? #{:user :maintainer} (:scope v)) true)
           (if (:report-types v) (s/valid? ::command-report-types (:report-types v)) true))))

(s/def ::commands-map
  (s/and (s/map-of valid-command-ids any?)
         #(every? valid-command-value? (vals %))))

(s/def :source/commands ::commands-map)

;; Global commands (optional) — same shape as per-source
(s/def :bark/commands ::commands-map)

;; Subject triggers: map of report-type keyword -> vector of tag strings
;; e.g. {:bug ["BUG" "DEFECT"] :request ["POLL" "FR" "TODO"]}
(s/def ::label-tags (s/coll-of ::non-blank-string :kind vector? :min-count 1))
(s/def ::labels
  (s/map-of #{:bug :patch :request :announcement :release :change}
            ::label-tags))
(s/def :source/labels ::labels)
(s/def :bark/labels ::labels)

;; Export reports: set of report type keywords to include in export
(s/def ::export-reports
  (s/coll-of valid-report-types :kind set? :min-count 1))
(s/def :source/export-reports ::export-reports)
(s/def :bark/export-reports ::export-reports)

;; Report types: which report types are detected during digest.
;; Default: all types. Per-source overrides global.
(s/def ::report-types
  (s/coll-of valid-report-types :kind set? :min-count 1))
(s/def :source/report-types ::report-types)
(s/def :bark/report-types ::report-types)

;; Expiry rules (optional)
;; Each report type maps to a rule map with :delay and optional conditions.
(s/def :expiry/delay (s/or :string (s/and ::non-blank-string #(re-seq #"\d+\s*[ydwm]" %))
                           :int pos-int?))
(s/def :expiry/max-status (s/and int? #(<= 0 % 3)))
(s/def :expiry/max-priority (s/and int? #(<= 0 % 3)))
(s/def :expiry/op-answered boolean?)

(s/def ::expiry-rule
  (s/keys :req-un [:expiry/delay]
          :opt-un [:expiry/max-status :expiry/max-priority :expiry/op-answered]))

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

;; Maintenance
(s/def :maintenance/orphan-delay (s/or :str ::non-blank-string :int pos-int?))
(s/def :bark/maintenance (s/keys :opt-un [:maintenance/orphan-delay]))

;; Top-level config
(s/def ::config
  (s/keys :req-un [:bark/admin :bark/imap :bark/sources :bark/db]
          :opt-un [:bark/ingest :bark/notifications :bark/labels
                   :bark/commands :bark/export-reports :bark/report-types
                   :bark/expiry :bark/logging :bark/maintenance]))

;; ---------------------------------------------------------------------------
;; Validation
;; ---------------------------------------------------------------------------

(defn validate-config [config]
  (if (s/valid? ::config config)
    (cond-> {:valid? true}
      (and (get-in config [:logging :email])
           (not (get-in config [:notifications :smtp])))
      (assoc :warnings ["Logging :email is configured but :notifications :smtp is absent."]))
    {:valid? false
     :explanation (s/explain-str ::config config)}))

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
            (log/info "  Default admin:" (:admin config))
            (let [imap (:imap config)]
              (log/info "  IMAP:" (str (:user imap) "@" (:host imap) "/" (:folder imap))))
            (log/info "  Sources:" (count (:sources config)))
            (doseq [src (:sources config)]
              (log/info "    -" (:name src)
                        (cond
                          (:list src)  (str "(list: " (:list src) ")")
                          (:alias src) (str "(alias: " (:alias src) ")")
                          (:to src)    (str "(mailbox: " (:to src) ")"))
                        (when-let [la (:list-archive src)] (str "archive: " la))
                        (when-let [a (:admin src)] (str "admin: " a))
                        (when-let [rt (:report-types src)] (str "report-types: " (pr-str rt)))
                        (when-let [ms (:maintainers src)]
                          (str "maintainers: "
                               (str/join ", " (map #(str (:email %)
                                                         (when (:since %) (str " (since " (:since %) ")")))
                                                   ms))))
                        (when (some? (get-in src [:notifications :enable]))
                          (str "notify: " (get-in src [:notifications :enable])))))
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
