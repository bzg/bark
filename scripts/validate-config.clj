#!/usr/bin/env bb

;; validate-config.clj — Validate config.edn against spec.
;;
;; Usage:
;;   bb scripts/validate-config.clj [path]
;;   bb validate-config [path]
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

;; Mailbox
(s/def :mailbox/host ::non-blank-string)
(s/def :mailbox/port ::pos-int)
(s/def :mailbox/ssl boolean?)
(s/def :mailbox/user ::non-blank-string)
(s/def :mailbox/password ::non-blank-string)
(s/def :mailbox/oauth2-token ::non-blank-string)
(s/def :mailbox/folder ::non-blank-string)
(s/def :mailbox/email ::email)
(s/def :mailbox/mailing-list-email ::email)
(s/def :mailbox/admin ::email)

(s/def ::mailbox
  (s/keys :req-un [:mailbox/host :mailbox/user :mailbox/folder
                   :mailbox/email]
          :opt-un [:mailbox/port :mailbox/ssl
                   :mailbox/password :mailbox/oauth2-token
                   :mailbox/admin :mailbox/mailing-list-email]))

(s/def :bark/mailboxes
  (s/and (s/coll-of ::mailbox :kind vector? :min-count 1)
         ;; Each mailbox must have either password or oauth2-token
         (fn [mbs] (every? #(or (:password %) (:oauth2-token %)) mbs))))

;; DB
(s/def :db/path ::non-blank-string)
(s/def :bark/db (s/keys :req-un [:db/path]))

;; Ingest
(s/def :ingest/initial-fetch ::pos-int)

(s/def :bark/ingest
  (s/keys :opt-un [:ingest/initial-fetch]))

;; Top-level config
(s/def ::config
  (s/keys :req-un [:bark/admin :bark/mailboxes :bark/db]
          :opt-un [:bark/ingest]))

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

(let [path   (or (first *command-line-args*) "config.edn")
      file   (clojure.java.io/file path)]
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
            (println (str "  Mailboxes:     " (count (:mailboxes config))))
            (doseq [mb (:mailboxes config)]
              (println (str "    - " (:email mb)
                            (when-let [ml (:mailing-list-email mb)]
                              (str " (list: " ml ")"))
                            " (user: " (:user mb) "@" (:host mb) "/" (:folder mb)
                            (when-let [a (:admin mb)] (str ", admin: " a))
                            ")")))
            (println (str "  DB path:       " (get-in config [:db :path])))
            (when-let [ingest (:ingest config)]
              (println (str "  Initial:       " (or (:initial-fetch ingest) 50) " msgs"))))
        (do (println (str "✗ " path " is invalid:\n"))
            (println (:explanation result))
            (System/exit 1))))))
