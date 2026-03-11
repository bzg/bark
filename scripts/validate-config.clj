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
         (fn [s] (not (re-find #"[<>]" s)))))
(s/def :match/delivered-to ::non-blank-string)
(s/def :match/to ::non-blank-string)

(s/def ::match
  (s/keys :opt-un [:match/list-id :match/delivered-to :match/to]))

;; Source
(s/def :source/name
  (s/and ::non-blank-string
         #(re-matches #"[a-zA-Z0-9][a-zA-Z0-9 ._-]*" %)))
(s/def :source/match ::match)
(s/def :source/admin ::email)
(s/def :source/list-post ::email)
(s/def :source/list-archive (s/and ::non-blank-string #(re-find #"^https?://" %)))
(s/def :source/bark-path ::non-blank-string)

(s/def ::source
  (s/keys :req-un [:source/name]
          :opt-un [:source/match :source/admin :source/list-post
                   :source/list-archive :source/triggers :source/labels
                   :source/bark-path :source/export-reports]))

(s/def :bark/sources
  (s/and (s/coll-of ::source :kind vector? :min-count 1)
         (fn [srcs] (= (count srcs) (count (distinct (map :name srcs)))))))

;; DB
(s/def :db/path ::non-blank-string)
(s/def :bark/db (s/keys :req-un [:db/path]))

;; Ingest
(s/def :ingest/initial-fetch ::pos-int)
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

;; Per-source triggers (optional)
(s/def ::trigger-words (s/coll-of ::non-blank-string :kind vector? :min-count 1))
(s/def ::action-triggers (s/map-of #{:acked :owned :closed} ::trigger-words))
(s/def :source/triggers (s/map-of keyword? ::action-triggers))

;; Global triggers (optional) — same shape as per-source triggers
(s/def :bark/triggers (s/map-of keyword? ::action-triggers))

;; Subject triggers: map of report-type keyword -> vector of tag strings
;; e.g. {:bug ["BUG" "DEFECT"] :request ["POLL" "FR" "TODO"]}
(s/def ::label-tags (s/coll-of ::non-blank-string :kind vector? :min-count 1))
(s/def ::labels
  (s/map-of #{:bug :patch :request :announcement :release :change}
            ::label-tags))
(s/def :source/labels ::labels)
(s/def :bark/labels ::labels)

;; Export reports: set of report type keywords to include in export
(def valid-report-types #{:bug :patch :request :announcement :release :change})
(s/def ::export-reports
  (s/coll-of valid-report-types :kind set? :min-count 1))
(s/def :source/export-reports ::export-reports)
(s/def :bark/export-reports ::export-reports)

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

;; Top-level config
(s/def ::config
  (s/keys :req-un [:bark/admin :bark/imap :bark/sources :bark/db]
          :opt-un [:bark/ingest :bark/notifications :bark/labels :bark/triggers
                   :bark/export-reports :bark/logging]))

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
                        (when-let [m (:match src)] (str "(match: " (pr-str m) ")"))
                        (when-let [ml (:list-post src)] (str "list: " ml))
                        (when-let [la (:list-archive src)] (str "archive: " la))
                        (when-let [a (:admin src)] (str "admin: " a))))
            (log/info "  DB path:" (get-in config [:db :path]))
            (when-let [ingest (:ingest config)]
              (log/info "  Initial:" (or (:initial-fetch ingest) 50) "msgs"))
            (when-let [notif (:notifications config)]
              (log/info "  Notifications:" (if (:enabled notif) "enabled" "disabled"))
              (when-let [smtp (:smtp notif)]
                (log/info "  SMTP:" (str (:user smtp) "@" (:host smtp)))))
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
