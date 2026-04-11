#!/usr/bin/env bb

;; bark-notify.clj — Send notification emails to maintainers.
;;
;; BARK: Bug And Report Keeper
;;
;; Queries notification preferences from the database (read-only),
;; builds plain-text summaries, and sends emails via SMTP.
;; Last-sent timestamps are stored in public/.last-notify.edn (not the DB).
;;
;; Usage:
;;   bb notify           — send due notifications
;;   bb notify --dry-run — show what would be sent without sending
;;
;; Environment / defaults:
;;   BARK_DB — path to db (default: ./data/bark-db)

(require '[babashka.pods :as pods]
         '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io])

;; Forward-declared for clj-kondo (provided at runtime by load-file below).
(declare load-datalevin-pod! get-header format-date format-date-iso
         report-priority report-status report-descendant-count
         all-reports report-pull-pattern load-config build-source-map bark-schema
         get-tenures maintainer?)

(load-file "scripts/bark-common.clj")

(load-datalevin-pod!)
(pods/load-pod 'tzzh/mail "0.0.3")

(require '[pod.tzzh.mail :as mail])

;; ---------------------------------------------------------------------------
;; File-based last-sent timestamps (replaces DB :notify/last-sent)
;; ---------------------------------------------------------------------------

(def ^:private last-notify-file "public/.last-notify.edn")

(defn- load-last-sent
  "Read {notify-key -> epoch-millis} from .last-notify.edn, or {}."
  []
  (let [f (io/file last-notify-file)]
    (if (.exists f)
      (try (edn/read-string (slurp f)) (catch Exception _ {}))
      {})))

(defn- save-last-sent!
  "Write the last-sent map to .last-notify.edn."
  [m]
  (io/make-parents last-notify-file)
  (spit last-notify-file (pr-str m)))

;; ---------------------------------------------------------------------------
;; Report queries (all-reports and report-pull-pattern loaded from bark-common.clj)
;; ---------------------------------------------------------------------------

;; format-date and format-date-iso are defined in bark-common.clj

;; ---------------------------------------------------------------------------
;; Notification queries
;; ---------------------------------------------------------------------------

(defn- load-failures
  "Read the failures file, returning a vector of failure maps."
  []
  (let [f (io/file "public/.failures.edn")]
    (if (.exists f)
      (try (edn/read-string (slurp f)) (catch Exception _ []))
      [])))

(defn- failures-for-subscriber
  "Return failures relevant to `email-addr` on `source` since `since-date`.

  Routing is driven by the `:audience` field on each failure entry:
  - `:author`      — shown only to the address that triggered the failure
                     (someone seeing their own typo); default for legacy
                     entries that predate the field.
  - `:maintainers` — shown to every maintainer subscriber on the source
                     (the notification loop already gates on
                     `still-privileged?`, so we don't re-check here)."
  [all-failures email-addr source since-date]
  (let [addr (str/lower-case email-addr)]
    (->> all-failures
         (filter (fn [{:keys [from audience date] src :source}]
                   (and (= source src)
                        (or (nil? since-date)
                            (and date (.after ^java.util.Date date since-date)))
                        (case (or audience :author)
                          :author      (= addr from)
                          :maintainers true
                          false)))))))

(defn- report-subject-by-mid
  "Look up the report's email subject from its message-id."
  [db mid]
  (when (and mid (not (str/blank? mid)))
    (d/q '[:find ?subj .
           :in $ ?mid
           :where [?r :report/message-id ?mid]
                  [?r :report/email ?e]
                  [?e :email/subject ?subj]]
         db mid)))

(defn all-notify-prefs [db]
  (->> (d/q '[:find (pull ?e [:notify/key :notify/source :notify/email
                              :notify/enabled :notify/interval-days
                              :notify/min-priority :notify/min-status
                              :notify/subject-match :notify/topic])
              :where [?e :notify/key _]]
            db)
       (map first)))

(defn- due?
  "True if enough days have elapsed since last-sent (or never sent).
  Reads last-sent from the file-based map, not the DB."
  [notify now last-sent-map]
  (let [interval-ms (* (:notify/interval-days notify 30) 86400000)
        last-ms     (get last-sent-map (:notify/key notify))]
    (or (nil? last-ms)
        (>= (- (.getTime now) last-ms) interval-ms))))

(defn- still-privileged?
  "Confirm the subscriber is still maintainer for the source.
  Uses the non-temporal check (current state, not as-of a specific date)."
  [db notify]
  (let [roles (get-tenures db (:notify/source notify))]
    (maintainer? roles (:notify/email notify))))

;; ---------------------------------------------------------------------------
;; Report formatting
;; ---------------------------------------------------------------------------

(def ^:private actionable-types #{:bug :patch :request})

(defn- open? [report]
  (nil? (:report/closed report)))

(defn- owned-by? [report email]
  (when-let [owner (:report/owned-address report)]
    (= (str/lower-case owner) (str/lower-case email))))

(defn- unacked? [report]
  (nil? (:report/acked report)))

(defn- unowned? [report]
  (nil? (:report/owned report)))

(def ^:private reason-labels
  {:unknown-target     "unknown target"
   :insufficient-scope "insufficient permissions"})

(defn- format-failure-line
  "Format a single command failure as a text line.
  `subjects` is a pre-loaded {message-id -> subject} map."
  [subjects failure]
  (let [date    (format-date (:date failure))
        reason  (get reason-labels (:reason failure)
                     (some-> (:reason failure) name))
        command (:command failure)
        from    (:from failure)
        mid     (:report-mid failure)
        subject (or (get subjects mid) mid)]
    (str "  [" date "] " command " — " reason "\n"
         "    by: " from "\n"
         "    on: " subject)))

(defn- format-report-line
  "Format a single report as a text line."
  [report]
  (let [email   (:report/email report)
        type    (name (:report/type report))
        subject (or (:email/subject email) "(no subject)")
        date    (format-date (:email/date-sent email))
        from    (or (:email/from-address email) "?")
        pri     (report-priority report)
        replies (report-descendant-count report)
        deadline (:report/deadline-value report)
        expiry   (:report/expiry-value report)
        arch    (get-header (:email/headers-edn email) "Archived-At")]
    (str "  [" type "] " subject "\n"
         "    from: " from " — " date
         " — priority:" pri " replies:" replies
         (when deadline (str " deadline:" (format-date-iso deadline)))
         (when expiry (str " expiry:" (format-date-iso expiry)))
         (when arch (str "\n    " arch)))))

(defn- section
  "Build a text section with a header and report lines, or nil if empty."
  [title reports]
  (when (seq reports)
    (str title "\n"
         (str/join "\n\n" (map format-report-line reports))
         "\n")))

(defn build-email-body
  "Build the notification email body for a given subscriber.
  `failures` is a seq of cmd-failure entities to include."
  [db reports notify failures]
  (let [email      (:notify/email notify)
        source     (:notify/source notify)
        min-pri    (:notify/min-priority notify 0)
        min-sts    (:notify/min-status notify 0)
        subj-match (:notify/subject-match notify)
        topic      (:notify/topic notify)
        by-type    (filter #(contains? actionable-types (:report/type %)) reports)
        by-open    (filter open? by-type)
        by-source  (filter #(= source (get-in % [:report/email :email/source])) by-open)
        by-pri     (filter #(>= (report-priority %) min-pri) by-source)
        by-sts     (filter #(>= (report-status %) min-sts) by-pri)
        by-subj    (if subj-match
                     (let [lc (str/lower-case subj-match)]
                       (filter #(some-> (get-in % [:report/email :email/subject])
                                        str/lower-case
                                        (str/includes? lc))
                               by-sts))
                     by-sts)
        by-topic   (if topic
                     (let [lc (str/lower-case topic)]
                       (filter #(some-> (:report/topic-value %)
                                        str/lower-case
                                        (str/includes? lc))
                               by-subj))
                     by-subj)
        _          (do (log/debug "build-email-body for" email (str "(source: " source ")"))
                       (log/debug "  all reports:" (count reports))
                       (log/debug "  by type (bug/patch/request):" (count by-type))
                       (log/debug "  by open:" (count by-open))
                       (log/debug "  by source =" (pr-str source) ":" (count by-source))
                       (when (and (pos? (count by-open)) (zero? (count by-source)))
                         (log/debug "  report sources:"
                                    (pr-str (set (map #(get-in % [:report/email :email/source]) by-open)))))
                       (log/debug "  by min-priority>=" min-pri ":" (count by-pri))
                       (log/debug "  by min-status>=" min-sts ":" (count by-sts))
                       (when subj-match
                         (log/debug "  by subject-match" (pr-str subj-match) ":" (count by-subj)))
                       (when topic
                         (log/debug "  by topic" (pr-str topic) ":" (count by-topic))))
        relevant   (sort-by (juxt report-priority report-descendant-count)
                            #(compare %2 %1) by-topic)
        owned      (filter #(owned-by? % email) relevant)
        owned-dl   (->> owned
                        (filter :report/deadline-value)
                        (sort-by #(.getTime ^java.util.Date (:report/deadline-value %))))
        owned-rest (->> owned
                        (remove :report/deadline-value)
                        (sort-by #(- (report-priority %))))
        unacked    (->> relevant
                        (filter unacked?)
                        (filter unowned?))
        sec-dl     (section
                    (str "== Upcoming deadlines — owned by you (" source ") ==")
                    owned-dl)
        sec-owned  (section
                    (str "== Open bugs/patches/requests owned by you (" source ") ==")
                    owned-rest)
        sec-unack  (section
                    (str "== Unacked & unowned bugs/patches/requests (" source ") ==")
                    unacked)
        fail-subjects (when (seq failures)
                       (->> failures
                            (map :report-mid)
                            distinct
                            (reduce (fn [m mid]
                                      (if-let [s (report-subject-by-mid db mid)]
                                        (assoc m mid s) m))
                                    {})))
        sec-fail   (when (seq failures)
                     (str "== Failed commands (" source ") ==\n"
                          (str/join "\n\n"
                                   (map #(format-failure-line fail-subjects %) failures))
                          "\n"))]
    (if (or sec-dl sec-owned sec-unack sec-fail)
      (str (or sec-fail "")
           (when (and sec-fail (or sec-dl sec-owned sec-unack)) "\n")
           (or sec-dl "")
           (when (and sec-dl (or sec-owned sec-unack)) "\n")
           (or sec-owned "")
           (when (and sec-owned sec-unack) "\n")
           (or sec-unack "")
           "\n--\nSent by Bark. Reply with \"Notify: off\" to unsubscribe.")
      nil)))

;; ---------------------------------------------------------------------------
;; Per-source notification gate
;; ---------------------------------------------------------------------------

(defn- source-notify-enabled?
  "True unless the source explicitly sets :notifications {:enable false}."
  [source-map source-name]
  (let [src-cfg (get source-map source-name)]
    (not (false? (get-in src-cfg [:notifications :enable])))))

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
                     :subject  "[BARK] Reports"
                     :text     body})))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

;; Guard ensures this block only runs when the script is invoked directly,
;; not when loaded via load-file (e.g. from tests or other scripts).
(when (= (System/getProperty "babashka.file") *file*)
  (let [flags    (set *command-line-args*)
        dry-run? (flags "--dry-run")
        force?   (flags "--force")
        debug?   (flags "--debug")
        _        (when debug? (log/merge-config! {:min-level :debug}))
        db-path  (or (System/getenv "BARK_DB") "data/bark-db")
        config   (load-config)
        notif    (:notifications config)]
    (when-not (and notif (:enabled notif))
      (log/info "Notifications disabled in config.")
      (System/exit 0))
    (let [smtp (or (:smtp notif)
                   (do (log/error "No :smtp config under :notifications.")
                       (System/exit 1)))
          conn (d/get-conn db-path bark-schema {:wal? false})]
      (try
        (let [db       (d/db conn)
              now      (java.util.Date.)
              src-map  (build-source-map config)
              reports  (all-reports db)
              all-failures (load-failures)
              prefs    (all-notify-prefs db)
              last-sent-map (load-last-sent)
              _        (do (log/debug (count prefs) "notify pref(s) found")
                           (doseq [p prefs]
                             (log/debug " " (:notify/key p)
                                        "enabled=" (:notify/enabled p)
                                        "last-sent=" (get last-sent-map (:notify/key p))
                                        "interval=" (:notify/interval-days p))))
              enabled  (filter :notify/enabled prefs)
              _        (log/debug (count enabled) "enabled")
              src-ok   (filter #(source-notify-enabled? src-map (:notify/source %)) enabled)
              _        (do (when (< (count src-ok) (count enabled))
                             (doseq [p enabled
                                     :when (not (source-notify-enabled? src-map (:notify/source p)))]
                               (log/debug "SKIPPED" (:notify/email p)
                                          "— notifications disabled for source"
                                          (:notify/source p))))
                           (log/debug (count src-ok) "after per-source filter"))
              on-time  (if force? src-ok (filter #(due? % now last-sent-map) src-ok))
              _        (log/debug (count on-time) "due"
                                  (when force? "(--force, skipped interval check)"))
              due      (filter #(still-privileged? db %) on-time)
              _        (do (log/debug (count due) "still privileged")
                           (when (< (count due) (count on-time))
                             (doseq [p on-time
                                     :when (not (still-privileged? db p))]
                               (log/debug "DROPPED" (:notify/email p)
                                          "— not maintainer for"
                                          (:notify/source p)))))
              sent     (atom 0)
              updated-map (atom last-sent-map)]
          (if (empty? due)
            (log/info "No notifications due.")
            (doseq [notify due]
              (let [addr     (:notify/email notify)
                    since-ms (get last-sent-map (:notify/key notify))
                    since    (when since-ms (java.util.Date. (long since-ms)))
                    failures (failures-for-subscriber all-failures addr (:notify/source notify) since)
                    body     (build-email-body db reports notify failures)]
                (if body
                  (do (log/info (if dry-run? "[dry-run]" "")
                                "Notifying" addr (str "(source: " (:notify/source notify) ")"))
                      (when-not dry-run?
                        (send-notification! smtp addr body)
                        (swap! updated-map assoc (:notify/key notify) (.getTime now))
                        (swap! sent inc))
                      (when dry-run?
                        (println "---")
                        (println body)
                        (println "---")))
                  (log/info "No open items for" addr
                            (str "(source: " (:notify/source notify) "),") "skipping.")))))
          (when-not dry-run?
            (save-last-sent! @updated-map))
          (log/info "Done." (if dry-run? "Dry run, no emails sent." (str @sent " email(s) sent."))))
        (finally
          (d/close conn))))))
