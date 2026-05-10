#!/usr/bin/env bb

;; bark-notify.clj -- Send notification emails to maintainers.
;;
;; BARK: Bug And Report Keeper
;;
;; Queries notification preferences from the database (read-only),
;; builds plain-text summaries, and sends emails via SMTP.
;; Last-sent timestamps are stored in public/.last-notify.edn (not the DB).
;;
;; Usage:
;;   bb notify           -- send due notifications
;;   bb notify --dry-run -- show what would be sent without sending
;;
;; Environment / defaults:
;;   BARK_DB -- path to db (default: ./data/bark-db)

(require '[babashka.pods :as pods]
         '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[taoensso.timbre :as log]
         '[bark.common :refer [get-header format-date format-date-iso
                               report-priority report-status report-descendant-count
                               load-config db-path build-source-map
                               bark-schema maintainer?
                               failures-file-path read-failures-file
                               reason-labels]]
         '[bark.common-bb :refer [load-datalevin-pod! dq all-reports get-tenures]])

(load-datalevin-pod!)
(pods/load-pod 'tzzh/mail "0.0.3")

(require '[pod.tzzh.mail :as mail])

;; ---------------------------------------------------------------------------
;; File-based last-sent timestamps (replaces DB :notify/last-sent)
;; ---------------------------------------------------------------------------

(def ^:private last-notify-file "public/.last-notify.edn")

(defn- load-last-sent
  "Read {notify-key -> epoch-millis} from .last-notify.edn, or {}.
  A corrupt file would reset every subscriber silently and re-spam
  them, so log at warn before returning the empty default."
  []
  (let [f (io/file last-notify-file)]
    (if (.exists f)
      (try (edn/read-string (slurp f))
           (catch Exception e
             (log/warn "Could not parse" last-notify-file
                       "-- starting from empty state.  Subscribers may be"
                       "re-notified at the next interval." (.getMessage e))
             {}))
      {})))

(defn- save-last-sent!
  "Write the last-sent map to .last-notify.edn."
  [m]
  (io/make-parents last-notify-file)
  (spit last-notify-file (pr-str m)))

;; ---------------------------------------------------------------------------
;; Notification queries
;; ---------------------------------------------------------------------------

(defn- load-failures
  "Read the failures file, returning a vector of failure maps.
  Log on parse failure instead of swallowing silently."
  []
  (read-failures-file failures-file-path
                      (fn [e]
                        (log/warn "Could not parse" failures-file-path ":"
                                  (.getMessage e)))))

(defn- failures-for-subscriber
  "Return failures relevant to `email-addr` on `source` since `since-date`.

  Routing is driven by the `:audience` field on each failure entry:
  - `:author`      -- shown only to the address that triggered the failure
                     (someone seeing their own typo); default for legacy
                     entries that predate the field.
  - `:maintainers` -- shown to every maintainer subscriber on the source
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
  "Filter the full report set down to those that match a subscriber's
  preferences: actionable type, open, on-source, meets min-priority and
  min-status, and (optionally) the :subject-match / :topic substring
  filters."
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
  "Group relevant reports into the three body sections. Returns a map
  with :dl (owned with deadline), :owned (owned, no deadline), and
  :unacked (not yet acked, not owned)."
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
  "Build {message-id -> subject} for the message-ids referenced by
  `failures`.  Skips mids whose report is missing from the DB."
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
  the unsubscribe footer.  Returns nil when every section is nil."
  [sections]
  (let [present (filter some? sections)]
    (when (seq present)
      (str (str/join "\n" present)
           "\n--\nSent by Bark. Reply with \"Notify: off\" to unsubscribe."))))

(defn build-email-body
  "Build the notification email body for a given subscriber.
  `failures` is a seq of cmd-failure entities to include."
  [db reports notify failures]
  (let [email      (:notify/email notify)
        source     (:notify/source notify)
        prefs      {:source     source
                    :min-pri    (:notify/min-priority notify 0)
                    :min-sts    (:notify/min-status notify 0)
                    :subj-match (:notify/subject-match notify)
                    :topic      (:notify/topic notify)}
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
;; Per-source notification gate
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
        config   (load-config)
        dbp      (db-path config)
        notif    (:notifications config)]
    (when-not (and notif (:enabled notif))
      (log/info "Notifications disabled in config.")
      (System/exit 0))
    (let [smtp (or (:smtp notif)
                   (do (log/error "No :smtp config under :notifications.")
                       (System/exit 1)))
          conn (d/get-conn dbp bark-schema {:wal? false})]
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
                                          "-- notifications disabled for source"
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
                                          "-- not maintainer for"
                                          (:notify/source p)))))
              sent     (atom 0)
              updated-map (atom last-sent-map)]
          (try
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
                          (try
                            (send-notification! smtp addr body)
                            (swap! updated-map assoc (:notify/key notify) (.getTime now))
                            (swap! sent inc)
                            (catch Exception e
                              ;; Don't advance the timestamp on failure so this
                              ;; subscriber is retried next run. Keep going so
                              ;; one SMTP blip doesn't starve every other
                              ;; recipient and so partial progress still lands
                              ;; on disk via the finally below.
                              (log/error "Failed to send to" addr
                                         (str "(source: " (:notify/source notify) "):")
                                         (.getMessage e)))))
                        (when dry-run?
                          (println "---")
                          (println body)
                          (println "---")))
                    (log/info "No open items for" addr
                              (str "(source: " (:notify/source notify) "),") "skipping.")))))
            (log/info "Done." (if dry-run? "Dry run, no emails sent." (str @sent " email(s) sent.")))
            (finally
              (when-not dry-run?
                (try (save-last-sent! @updated-map)
                     (catch Exception e
                       (log/error "Failed to persist last-sent timestamps:" (.getMessage e))))))))
        (finally
          (d/close conn))))))
