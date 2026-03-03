#!/usr/bin/env bb

;; bark-notify.clj — Send notification emails to admin/maintainers.
;;
;; BARK: Bug And Report Keeper
;;
;; Queries notification preferences from the database, builds a plain-text
;; summary of open reports, and sends emails via SMTP to subscribers whose
;; interval has elapsed.
;;
;; Usage:
;;   bb notify           — send due notifications
;;   bb notify --dry-run — show what would be sent without sending
;;
;; Environment / defaults:
;;   BARK_DB — path to db (default: ./data/bark-db)

(require '[babashka.pods :as pods]
         '[clojure.string :as str]
         '[clojure.edn :as edn])

(load-file "scripts/bark-common.clj")

(pods/load-pod 'huahaiy/datalevin "0.10.7")
(pods/load-pod 'tzzh/mail "0.0.3")

(require '[pod.huahaiy.datalevin :as d]
         '[pod.tzzh.mail :as mail])

;; ---------------------------------------------------------------------------
;; Schema
;; ---------------------------------------------------------------------------

(def schema
  (edn/read-string (slurp "resources/bark-schema.edn")))

;; ---------------------------------------------------------------------------
;; Report queries (mirrors bark-egest.clj)
;; ---------------------------------------------------------------------------

(def report-pull-pattern
  '[:db/id :report/type :report/version :report/topic
    :report/patch-seq :report/message-id
    {:report/acked [:email/from-address]}
    {:report/owned [:email/from-address]}
    {:report/closed [:email/from-address]}
    :report/urgent :report/important
    :report/votes-up :report/votes-down
    :report/descendants
    {:report/email [:email/subject :email/from-address :email/from-name
                    :email/date-sent :email/source :email/headers-edn]}])

(defn all-reports [db]
  (->> (d/q (list :find (list 'pull '?r report-pull-pattern)
                  :where ['?r :report/type '_])
            db)
       (map first)))

;; ---------------------------------------------------------------------------
;; Report scoring (same logic as bark-egest.clj)
;; ---------------------------------------------------------------------------

(defn- priority [report]
  (+ (if (:report/urgent report) 2 0)
     (if (:report/important report) 1 0)))

(defn- status [report]
  (+ (if-not (:report/closed report) 4 0)
     (if (:report/owned report) 2 0)
     (if (:report/acked report) 1 0)))

(defn- descendant-count [report]
  (let [d (:report/descendants report)]
    (if (coll? d) (count d) 0)))

(defn- format-date [date]
  (let [s (str (or date ""))]
    (subs s 0 (min 10 (count s)))))

;; ---------------------------------------------------------------------------
;; Notification queries
;; ---------------------------------------------------------------------------

(defn all-notify-prefs [db]
  (->> (d/q '[:find (pull ?e [:notify/key :notify/source :notify/email
                               :notify/enabled :notify/interval-days
                               :notify/min-priority :notify/min-status
                               :notify/last-sent])
              :where [?e :notify/key _]]
            db)
       (map first)))

(defn- due?
  "True if enough days have elapsed since last-sent (or never sent)."
  [notify now]
  (let [interval-ms (* (:notify/interval-days notify 30) 86400000)
        last-sent   (:notify/last-sent notify)]
    (or (nil? last-sent)
        (>= (- (.getTime now) (.getTime last-sent)) interval-ms))))

(defn- still-privileged?
  "Confirm the subscriber is still admin or maintainer for the source."
  [db notify]
  (let [roles (or (d/pull db '[:roles/admin :roles/maintainers]
                          [:roles/source (:notify/source notify)])
                  {})
        addr-lc (str/lower-case (:notify/email notify))
        admin-lc (some-> (:roles/admin roles) str/lower-case)
        maints-lc (let [v (:roles/maintainers roles)]
                    (cond (nil? v) #{} (string? v) #{(str/lower-case v)}
                          :else (set (map str/lower-case v))))]
    (or (= addr-lc admin-lc)
        (contains? maints-lc addr-lc))))

;; ---------------------------------------------------------------------------
;; Report formatting
;; ---------------------------------------------------------------------------

(def ^:private actionable-types #{:bug :patch :request})

(defn- open? [report]
  (nil? (:report/closed report)))

(defn- owned-by? [report email]
  (when-let [owner (get-in report [:report/owned :email/from-address])]
    (= (str/lower-case owner) (str/lower-case email))))

(defn- unacked? [report]
  (nil? (:report/acked report)))

(defn- unowned? [report]
  (nil? (:report/owned report)))

(defn- format-report-line
  "Format a single report as a text line."
  [report]
  (let [email   (:report/email report)
        type    (name (:report/type report))
        subject (or (:email/subject email) "(no subject)")
        date    (format-date (:email/date-sent email))
        from    (or (:email/from-address email) "?")
        pri     (priority report)
        replies (descendant-count report)
        arch    (get-header (:email/headers-edn email) "Archived-At")]
    (str "  [" type "] " subject "\n"
         "    from: " from " — " date
         " — priority:" pri " replies:" replies
         (when arch (str "\n    " arch)))))

(defn- section
  "Build a text section with a header and report lines, or nil if empty."
  [title reports]
  (when (seq reports)
    (str title "\n"
         (str/join "\n\n" (map format-report-line reports))
         "\n")))

(defn build-email-body
  "Build the notification email body for a given subscriber."
  [reports notify debug?]
  (let [email      (:notify/email notify)
        source     (:notify/source notify)
        min-pri    (:notify/min-priority notify 0)
        min-sts    (:notify/min-status notify 0)
        by-type    (filter #(contains? actionable-types (:report/type %)) reports)
        by-open    (filter open? by-type)
        by-source  (filter #(= source (get-in % [:report/email :email/source])) by-open)
        by-pri     (filter #(>= (priority %) min-pri) by-source)
        by-sts     (filter #(>= (status %) min-sts) by-pri)
        _          (when debug?
                     (println (str "  [debug] build-email-body for " email " (source: " source ")"))
                     (println (str "  [debug]   all reports: " (count reports)))
                     (println (str "  [debug]   by type (bug/patch/request): " (count by-type)))
                     (println (str "  [debug]   by open: " (count by-open)))
                     (println (str "  [debug]   by source =" (pr-str source) ": " (count by-source)))
                     (when (and (pos? (count by-open)) (zero? (count by-source)))
                       (println (str "  [debug]   report sources: "
                                     (pr-str (set (map #(get-in % [:report/email :email/source]) by-open))))))
                     (println (str "  [debug]   by min-priority>=" min-pri ": " (count by-pri)))
                     (println (str "  [debug]   by min-status>=" min-sts ": " (count by-sts))))
        relevant   (sort-by (juxt #(- (priority %))
                                  #(- (descendant-count %)))
                            compare by-sts)
        owned      (filter #(owned-by? % email) relevant)
        unacked    (->> relevant
                        (filter unacked?)
                        (filter unowned?))
        sec-owned  (section
                    (str "== Open bugs/patches/requests owned by you (" source ") ==")
                    owned)
        sec-unack  (section
                    (str "== Unacked & unowned bugs/patches/requests (" source ") ==")
                    unacked)]
    (if (or sec-owned sec-unack)
      (str (or sec-owned "")
           (when (and sec-owned sec-unack) "\n")
           (or sec-unack "")
           "\n--\nSent by Bark. Reply with \"Notify: off\" to unsubscribe.")
      nil)))

;; ---------------------------------------------------------------------------
;; SMTP
;; ---------------------------------------------------------------------------

(defn send-notification!
  "Send a plain-text notification email via SMTP."
  [smtp-config to-addr body]
  (let [{:keys [host port tls user password from]} smtp-config]
    (mail/send-mail {:host     host
                     :port     port
                     :tls      (boolean tls)
                     :username user
                     :password password
                     :from     from
                     :to       [to-addr]
                     :subject  "[Bark] Reports"
                     :text     body})))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(when (= (System/getProperty "babashka.file") *file*)
  (let [args     *command-line-args*
        dry-run? (some #{"--dry-run"} args)
        force?   (some #{"--force"} args)
        debug?   (some #{"--debug"} args)
        db-path  (or (System/getenv "BARK_DB") "data/bark-db")
        config   (load-config)
        notif    (:notifications config)]
    (when-not (and notif (:enabled notif))
      (println "Notifications disabled in config.")
      (System/exit 0))
    (let [smtp (or (:smtp notif)
                   (do (println "No :smtp config under :notifications.")
                       (System/exit 1)))
          conn (d/get-conn db-path schema)]
      (try
        (let [db       (d/db conn)
              now      (java.util.Date.)
              reports  (all-reports db)
              prefs    (all-notify-prefs db)
              _        (when debug?
                         (println (str "  [debug] " (count prefs) " notify pref(s) found"))
                         (doseq [p prefs]
                           (println (str "  [debug]   " (:notify/key p)
                                         " enabled=" (:notify/enabled p)
                                         " last-sent=" (:notify/last-sent p)
                                         " interval=" (:notify/interval-days p)))))
              enabled  (filter :notify/enabled prefs)
              _        (when debug?
                         (println (str "  [debug] " (count enabled) " enabled")))
              on-time  (if force? enabled (filter #(due? % now) enabled))
              _        (when debug?
                         (println (str "  [debug] " (count on-time) " due"
                                       (when force? " (--force, skipped interval check)"))))
              due      (filter #(still-privileged? db %) on-time)
              _        (when debug?
                         (println (str "  [debug] " (count due) " still privileged"))
                         (when (< (count due) (count on-time))
                           (doseq [p on-time
                                   :when (not (still-privileged? db p))]
                             (println (str "  [debug]   DROPPED " (:notify/email p)
                                           " — not admin/maintainer for "
                                           (:notify/source p))))))
              sent     (atom 0)]
          (if (empty? due)
            (println "No notifications due.")
            (doseq [notify due]
              (let [addr (:notify/email notify)
                    body (build-email-body reports notify debug?)]
                (if body
                  (do (println (str (if dry-run? "[dry-run] " "")
                                    "Notifying " addr
                                    " (source: " (:notify/source notify) ")"))
                      (when-not dry-run?
                        (send-notification! smtp addr body)
                        (d/transact! conn [{:notify/key       (:notify/key notify)
                                            :notify/last-sent now}])
                        (swap! sent inc))
                      (when dry-run?
                        (println "---")
                        (println body)
                        (println "---")))
                  (println (str "  No open items for " addr
                                " (source: " (:notify/source notify) "), skipping."))))))
          (println (str "Done. " (if dry-run? "Dry run, no emails sent." (str @sent " email(s) sent.")))))
        (finally
          (d/close conn))))))
