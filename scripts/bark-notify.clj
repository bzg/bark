#!/usr/bin/env bb

;; bark-notify.clj -- Send notification emails to subscribers.
;;
;; BARK: Bug And Report Keeper
;;
;; Reads the subscriber list from config.edn (:notifications
;; :subscribers), builds plain-text summaries from the DB (read-only),
;; and sends emails via SMTP.  The cadence is the operator's
;; responsibility: schedule bb notify from cron or a systemd timer.
;;
;; Usage:
;;   bb notify           -- send notifications
;;   bb notify --dry-run -- show what would be sent without sending
;;   bb notify --debug   -- verbose diagnostics

(require '[babashka.pods :as pods]
         '[clojure.string :as str]
         '[taoensso.timbre :as log]
         '[bark.common :refer [get-header format-date format-date-iso
                               report-priority report-status report-descendant-count
                               load-config db-path build-source-map
                               bark-schema
                               failures-file-path read-failures-file
                               reason-labels]]
         '[bark.common-bb :refer [load-datalevin-pod! all-reports]])

(load-datalevin-pod!)
(pods/load-pod 'tzzh/mail "0.0.3")

(require '[pod.tzzh.mail :as mail])

;; ---------------------------------------------------------------------------
;; Failure queries
;; ---------------------------------------------------------------------------

(defn- load-failures
  "Read the failures file, returning a vector of failure maps."
  []
  (read-failures-file failures-file-path
                      (fn [e]
                        (log/warn "Could not parse" failures-file-path ":"
                                  (.getMessage e)))))

(defn- failures-for-subscriber
  "Return failures on `source` routed to `email-addr`.

  Routing is driven by the `:audience` field on each failure entry:
  - `:author`      -- shown only to the address that triggered the
                     failure (someone seeing their own typo); default
                     for legacy entries that predate the field.
  - `:maintainers` -- shown to every subscriber on the source."
  [all-failures email-addr source]
  (let [addr (str/lower-case email-addr)]
    (->> all-failures
         (filter (fn [{:keys [from audience] src :source}]
                   (and (= source src)
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
    (str "  [" date "] " command " -- " reason "\n"
         "    by: " from "\n"
         "    on: " subject)))

(defn- format-report-line
  "Format a single report as a text line."
  [report]
  (let [email   (:report/email report)
        type    (name (:report/type report))
        subject (or (:email/subject email) "(no subject)")
        date    (format-date (:email/date-sent email))
        from    (or (:email/author-address email) "?")
        pri     (report-priority report)
        replies (report-descendant-count report)
        deadline (:report/deadline-value report)
        expiry   (:report/expiry-value report)
        arch    (get-header (:email/headers-edn email) "Archived-At")]
    (str "  [" type "] " subject "\n"
         "    from: " from " -- " date
         " -- priority:" pri " replies:" replies
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

(defn- filter-relevant-reports
  "Filter the full report set against a subscription's filters: actionable
  type, open, on-source, meets min-priority and min-status, and (optionally)
  the :subject-match / :topic substring filters."
  [reports {:keys [source min-pri min-sts subj-match topic]}]
  (let [subj-lc (some-> subj-match str/lower-case)
        topic-lc (some-> topic str/lower-case)]
    (cond->> reports
      true       (filter #(contains? actionable-types (:report/type %)))
      true       (filter open?)
      true       (filter #(= source (get-in % [:report/email :email/source])))
      true       (filter #(>= (report-priority %) min-pri))
      true       (filter #(>= (report-status %) min-sts))
      subj-lc    (filter #(some-> (get-in % [:report/email :email/subject])
                                  str/lower-case
                                  (str/includes? subj-lc)))
      topic-lc   (filter #(some-> (:report/topic-value %)
                                  str/lower-case
                                  (str/includes? topic-lc))))))

(defn- build-sections
  "Group relevant reports into the three body sections."
  [relevant email]
  (let [owned (filter #(owned-by? % email) relevant)]
    {:dl      (->> owned
                   (filter :report/deadline-value)
                   (sort-by #(.getTime ^java.util.Date (:report/deadline-value %))))
     :owned   (->> owned
                   (remove :report/deadline-value)
                   (sort-by #(- (report-priority %))))
     :unacked (->> relevant
                   (filter unacked?)
                   (filter unowned?))}))

(defn- failure-subjects-map
  "Build {message-id -> subject} for the message-ids referenced by `failures`."
  [db failures]
  (when (seq failures)
    (->> failures
         (map :report-mid)
         distinct
         (reduce (fn [m mid]
                   (if-let [s (report-subject-by-mid db mid)]
                     (assoc m mid s) m))
                 {}))))

(defn- failures-section
  "Build the failures section text, or nil when there are no failures."
  [db source failures]
  (when (seq failures)
    (let [subjects (failure-subjects-map db failures)]
      (str "== Failed commands (" source ") ==\n"
           (str/join "\n\n" (map #(format-failure-line subjects %) failures))
           "\n"))))

(defn- join-sections
  "Concatenate the non-nil sections with blank-line separators and append
  a short footer.  Returns nil when every section is nil."
  [sections]
  (let [present (filter some? sections)]
    (when (seq present)
      (str (str/join "\n" present)
           "\n-- \nSent by BARK.  Contact the operator to change your subscription."))))

(defn build-email-body
  "Build the notification email body for one (email, subscription) pair.
  `failures` is a seq of cmd-failure entities to include."
  [db reports email subscription failures]
  (let [source     (:source subscription)
        prefs      {:source     source
                    :min-pri    (:min-priority subscription 0)
                    :min-sts    (:min-status subscription 0)
                    :subj-match (:subject-match subscription)
                    :topic      (:topic subscription)}
        relevant   (->> (filter-relevant-reports reports prefs)
                        (sort-by (juxt report-priority report-descendant-count)
                                 #(compare %2 %1)))
        {:keys [dl owned unacked]} (build-sections relevant email)
        sec-fail   (failures-section db source failures)
        sec-dl     (section
                    (str "== Upcoming deadlines -- owned by you (" source ") ==")
                    dl)
        sec-owned  (section
                    (str "== Open bugs/patches/requests owned by you (" source ") ==")
                    owned)
        sec-unack  (section
                    (str "== Unacked & unowned bugs/patches/requests (" source ") ==")
                    unacked)]
    (log/debug "build-email-body for" email (str "(source: " source ")"))
    (log/debug "  total reports:" (count reports) "-- relevant:" (count relevant))
    (join-sections [sec-fail sec-dl sec-owned sec-unack])))

;; ---------------------------------------------------------------------------
;; Per-source kill switch
;; ---------------------------------------------------------------------------

(defn- source-notify-enabled?
  "True unless the source explicitly sets :notifications {:enabled false}."
  [source-map source-name]
  (let [src-cfg (get source-map source-name)]
    (not (false? (get-in src-cfg [:notifications :enabled])))))

;; ---------------------------------------------------------------------------
;; SMTP
;; ---------------------------------------------------------------------------

(defn send-notification!
  "Send a plain-text notification email via SMTP."
  [smtp-config to-addr source body]
  (let [{:keys [host port tls user password from]} smtp-config]
    (mail/send-mail {:host     host
                     :port     port
                     :tls      (boolean tls)
                     :username user
                     :password password
                     :from     from
                     :to       [to-addr]
                     :subject  (str "[BARK " source "] Open reports")
                     :text     body})))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn- expand-subscribers
  "Expand the :subscribers map into a flat seq of [email subscription] pairs."
  [subscribers]
  (mapcat (fn [[email subs]]
            (map (fn [s] [email s]) subs))
          subscribers))

;; Guard ensures this block only runs when the script is invoked directly,
;; not when loaded via load-file (e.g. from tests or other scripts).
(when (= (System/getProperty "babashka.file") *file*)
  (let [flags    (set *command-line-args*)
        dry-run? (flags "--dry-run")
        debug?   (flags "--debug")
        _        (when debug? (log/merge-config! {:min-level :debug}))
        config   (load-config)
        dbp      (db-path config)
        notif    (:notifications config)]
    (when-not (and notif (:enabled notif))
      (log/info "Notifications disabled in config.")
      (System/exit 0))
    (let [smtp        (or (:smtp notif)
                          (do (log/error "No :smtp config under :notifications.")
                              (System/exit 1)))
          subscribers (:subscribers notif)
          conn        (when (seq subscribers)
                        (d/get-conn dbp bark-schema {:wal? false}))]
      (when (empty? subscribers)
        (log/info "No :subscribers configured.")
        (System/exit 0))
      (try
        (let [db           (d/db conn)
              src-map      (build-source-map config)
              reports      (all-reports db)
              all-failures (load-failures)
              pairs        (expand-subscribers subscribers)
              _            (log/debug (count pairs) "subscription(s) configured")
              live-pairs   (filter (fn [[_ s]] (source-notify-enabled? src-map (:source s))) pairs)
              _            (do (when (< (count live-pairs) (count pairs))
                                 (doseq [[email s] pairs
                                         :when (not (source-notify-enabled? src-map (:source s)))]
                                   (log/debug "SKIPPED" email
                                              "-- notifications disabled for source"
                                              (:source s))))
                               (log/debug (count live-pairs) "after per-source filter"))
              sent         (atom 0)]
          (if (empty? live-pairs)
            (log/info "No active subscriptions.")
            (doseq [[email subscription] live-pairs]
              (let [source   (:source subscription)
                    failures (failures-for-subscriber all-failures email source)
                    body     (build-email-body db reports email subscription failures)]
                (if body
                  (do (log/info (if dry-run? "[dry-run]" "")
                                "Notifying" email (str "(source: " source ")"))
                      (when-not dry-run?
                        (try
                          (send-notification! smtp email source body)
                          (swap! sent inc)
                          (catch Exception e
                            (log/error "Failed to send to" email
                                       (str "(source: " source "):")
                                       (.getMessage e)))))
                      (when dry-run?
                        (println "---")
                        (println body)
                        (println "---")))
                  (log/info "No open items for" email
                            (str "(source: " source "),") "skipping.")))))
          (log/info "Done." (if dry-run? "Dry run, no emails sent." (str @sent " email(s) sent."))))
        (finally
          (d/close conn))))))
