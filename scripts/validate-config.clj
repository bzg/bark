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
         '[clojure.string :as str])

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
(s/def :match/list-id ::non-blank-string)
(s/def :match/delivered-to ::non-blank-string)
(s/def :match/to ::non-blank-string)

(s/def ::match
  (s/keys :opt-un [:match/list-id :match/delivered-to :match/to]))

;; Source
(s/def :source/name ::non-blank-string)
(s/def :source/match ::match)
(s/def :source/admin ::email)
(s/def :source/mailing-list-email ::email)

(s/def ::source
  (s/keys :req-un [:source/name]
          :opt-un [:source/match :source/admin :source/mailing-list-email
                   :source/triggers :source/labels]))

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

;; Subject triggers: map of report-type keyword → vector of tag strings
;; e.g. {:bug ["BUG" "DEFECT"] :request ["POLL" "FR" "TODO"]}
(s/def ::label-tags (s/coll-of ::non-blank-string :kind vector? :min-count 1))
(s/def ::labels
  (s/map-of #{:bug :patch :request :announcement :release :change}
            ::label-tags))
(s/def :source/labels ::labels)
(s/def :bark/labels ::labels)

;; Top-level config
(s/def ::config
  (s/keys :req-un [:bark/admin :bark/imap :bark/sources :bark/db]
          :opt-un [:bark/ingest :bark/notifications :bark/labels]))

;; ---------------------------------------------------------------------------
;; Validation
;; ---------------------------------------------------------------------------

(defn validate-config [config]
  (if (s/valid? ::config config)
    {:valid? true}
    {:valid? false
     :explanation (s/explain-str ::config config)}))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [path (or (first *command-line-args*) "config.edn")
      file (clojure.java.io/file path)]
  (if-not (.exists file)
    (do (println (str "Error: config file not found: " path))
        (System/exit 1))
    (let [config (try
                   (edn/read-string (slurp file))
                   (catch Exception e
                     (println (str "Error: invalid EDN: " (.getMessage e)))
                     (System/exit 1)))
          result (validate-config config)]
      (if (:valid? result)
        (do (println (str "✓ " path " is valid."))
            (println (str "  Default admin: " (:admin config)))
            (let [imap (:imap config)]
              (println (str "  IMAP:          " (:user imap) "@" (:host imap)
                            "/" (:folder imap))))
            (println (str "  Sources:       " (count (:sources config))))
            (doseq [src (:sources config)]
              (println (str "    - " (:name src)
                            (when-let [m (:match src)]
                              (str " (match: " (pr-str m) ")"))
                            (when-let [ml (:mailing-list-email src)]
                              (str " list: " ml))
                            (when-let [a (:admin src)]
                              (str " admin: " a)))))
            (println (str "  DB path:       " (get-in config [:db :path])))
            (when-let [ingest (:ingest config)]
              (println (str "  Initial:       "
                            (or (:initial-fetch ingest) 50) " msgs")))
            (when-let [notif (:notifications config)]
              (println (str "  Notifications: " (if (:enabled notif) "enabled" "disabled")))
              (when-let [smtp (:smtp notif)]
                (println (str "  SMTP:          " (:user smtp) "@" (:host smtp))))))
        (do (println (str "✗ " path " is invalid:\n"))
            (println (:explanation result))
            (System/exit 1))))))
