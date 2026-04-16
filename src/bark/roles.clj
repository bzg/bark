;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.roles
  "Maintainer tenure management, controls, and permission checks."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [taoensso.timbre :as log]
            [bark.common :as common]
            [bark.commands :as commands]
            [bark.tracking :as tracking]))

;; ---------------------------------------------------------------------------
;; Tenure queries
;; ---------------------------------------------------------------------------

(defn get-tenures
  "Return the seq of all tenure maps (active and closed) for `source-name`."
  [db source-name]
  (let [eids (d/q '[:find [?e ...]
                    :in $ ?src
                    :where [?e :maint-tenure/source ?src]]
                  db source-name)]
    (mapv #(common/tenure-map (d/pull db common/tenure-pull-pattern %)) eids)))

(defn- active-tenure-eid
  "Return the :db/id of the currently-active tenure for (source, email), or nil."
  [db source-name email]
  (let [a (str/lower-case email)]
    (->> (get-tenures db source-name)
         (filter #(and (= a (:email %)) (nil? (:to %))))
         first
         :eid)))

;; ---------------------------------------------------------------------------
;; Config seeding
;; ---------------------------------------------------------------------------

(defn ensure-source-roles!
  "Seed tenures from `config` for any source/email pair that does not yet
  have any tenure in the DB. Each maintainer gets a new open tenure with
  :from derived from :since (absent :since → :from = nil, meaning active
  since the beginning of time). The :order field is the index of the entry
  in the config :maintainers vector — used as a tie-break when computing
  the lead maintainer."
  [conn config]
  (doseq [{:keys [name maintainers]} (:sources config)]
    (when (seq maintainers)
      (let [db       (d/db conn)
            existing (set (map :email (get-tenures db name)))
            tx       (into []
                           (keep-indexed
                            (fn [idx {:keys [email since]}]
                              (when (and email
                                         (not (contains? existing (str/lower-case email))))
                                (let [addr (str/lower-case email)
                                      from (when since
                                             (cond
                                               (inst? since) since
                                               (string? since)
                                               (try (common/parse-iso-date since)
                                                    (catch Exception _ nil))))]
                                  (cond-> {:maint-tenure/source name
                                           :maint-tenure/email  addr
                                           :maint-tenure/order  idx}
                                    from (assoc :maint-tenure/from from))))))
                           maintainers)]
        (when (seq tx)
          (d/transact! conn tx)
          (doseq [{addr :maint-tenure/email from :maint-tenure/from} tx]
            (log/info (str "Config maintainer: " addr
                           (when from (str " (since " from ")"))
                           " (for " name ")"))))))))

;; ---------------------------------------------------------------------------
;; Role control parsing and application
;; ---------------------------------------------------------------------------

(def role-control-pattern
  #"(?m)^(Add maintainer|Remove maintainer):\s+(.+)$")

(defn- parse-addresses [s]
  (when s
    (->> (str/split (str/trim s) #"\s+")
         (remove str/blank?)
         (filter #(re-matches #"[^@\s<>]+@[^@\s<>]+\.[^@\s<>]+" %)))))

(defn parse-role-controls [body-text]
  (when body-text
    (->> (re-seq role-control-pattern body-text)
         (mapv (fn [[_ cmd addrs]]
                 {:command cmd :addresses (parse-addresses addrs)})))))

(defn- open-tenure!
  "Open a new tenure for each address that does not already have an active one.
  Returns the list of addresses for which a tenure was actually created."
  [conn source-name addresses email-date]
  (let [db    (d/db conn)
        opens (remove #(active-tenure-eid db source-name %) addresses)]
    (when (seq opens)
      (d/transact! conn
                   (mapv (fn [addr]
                           {:maint-tenure/source source-name
                            :maint-tenure/email  (str/lower-case addr)
                            :maint-tenure/from   email-date})
                         opens))
      (mapv str/lower-case opens))))

(defn- close-tenure!
  "Close the active tenure for each given address (by setting :to = email-date).
  The lead maintainer's tenure is never closed. Returns the list of addresses
  whose tenure was actually closed.  Attempts to close the lead maintainer
  are recorded as `:insufficient-scope` failures (audience `:maintainers`)
  when `failure-ctx` is provided."
  [conn tenures source-name addresses email-date failure-ctx]
  (let [lead (common/lead-maintainer tenures)
        db   (d/db conn)]
    (->> addresses
         (keep (fn [addr]
                 (let [a (str/lower-case addr)]
                   (cond
                     (= a lead)
                     (do (log/warn "Denied: cannot remove lead maintainer" a
                                   "(for" source-name ")")
                         (when failure-ctx
                           (commands/record-failure!
                            (assoc failure-ctx
                                   :reason   :insufficient-scope
                                   :audience :maintainers
                                   :command  (str "Remove maintainer: " a))))
                         nil)
                     :else
                     (when-let [eid (active-tenure-eid db source-name a)]
                       (d/transact! conn [[:db/add eid :maint-tenure/to email-date]])
                       a)))))
         vec)))

(defn apply-role-controls!
  "Apply `Add maintainer:` / `Remove maintainer:` directives found in
  `body-text`. `tenures` is the pre-directive snapshot used for permission
  checks; the DB is re-read between operations so effects chain correctly.

  Denied attempts are written to the failures file as
  `:insufficient-scope`/`:maintainers` so the lead maintainer (and any
  other notified maintainer) sees them in the next digest."
  [conn tenures source-name from-addr body-text email-date]
  (let [controls    (parse-role-controls body-text)
        is-maint    (common/maintainer? tenures from-addr)
        is-lead     (common/lead-maintainer? tenures from-addr)
        failure-ctx (when (and from-addr source-name)
                      {:source     source-name
                       :from-addr  from-addr
                       :email-date email-date
                       :report-mid ""})]
    (doseq [{:keys [command addresses]} controls]
      (case command
        "Add maintainer"
        (if is-maint
          (when-let [opened (seq (open-tenure! conn source-name addresses email-date))]
            (tracking/bump-global-modified! conn)
            (log/info "add maintainer:" (str/join " " opened)
                      (str "(for " source-name ")")))
          (do (log/warn "Denied:" from-addr "lacks permission for: Add maintainer")
              (when failure-ctx
                (commands/record-failure!
                 (assoc failure-ctx
                        :reason   :insufficient-scope
                        :audience :maintainers
                        :command  (str "Add maintainer: " (str/join " " addresses)))))))

        "Remove maintainer"
        (if is-lead
          ;; Always re-read tenures before closing so the lead check uses
          ;; the latest state (e.g. if this directive follows an Add).
          (let [current (get-tenures (d/db conn) source-name)]
            (when-let [closed (seq (close-tenure! conn current source-name
                                                  addresses email-date failure-ctx))]
              (tracking/bump-global-modified! conn)
              (log/info "remove maintainer:" (str/join " " closed)
                        (str "(for " source-name ")"))))
          (do (log/warn "Denied:" from-addr
                        "lacks permission for: Remove maintainer (lead only)")
              (when failure-ctx
                (commands/record-failure!
                 (assoc failure-ctx
                        :reason   :insufficient-scope
                        :audience :maintainers
                        :command  (str "Remove maintainer: " (str/join " " addresses)))))))

        nil))))

;; ---------------------------------------------------------------------------
;; Notify control parsing and application
;; ---------------------------------------------------------------------------

(def ^:private notify-pattern #"(?m)^Notify:\s+(.+)$")

(defn- parse-notify-params [s]
  (let [s (str/trim s) lc (str/lower-case s)]
    (cond
      (= lc "on")  {:enabled true}
      (= lc "off") {:enabled false}
      :else
      (let [has-on?  (str/starts-with? lc "on ")
            has-off? (str/starts-with? lc "off ")
            params   (re-seq #"([dpsmt]):(\S+)" s)
            base     (cond has-on? {:enabled true} has-off? {:enabled false} :else {})]
        (reduce (fn [m [_ k v]]
                  (case k
                    "d" (assoc m :interval-days (parse-long v))
                    "p" (assoc m :min-priority (parse-long v))
                    "s" (assoc m :min-status (parse-long v))
                    "m" (assoc m :subject-match v)
                    "t" (assoc m :topic v)
                    m))
                base params)))))

(defn- notify-key [source-name email]
  (str source-name ":" (str/lower-case email)))

(defn ensure-notify-defaults! [conn source-name tenures]
  (let [emails (distinct (keep :email (common/active-tenures tenures)))]
    (doseq [email emails]
      (let [k (notify-key source-name email)]
        (when-not (d/entid (d/db conn) [:notify/key k])
          (d/transact! conn [{:notify/key          k
                              :notify/source       source-name
                              :notify/email        (str/lower-case email)
                              :notify/enabled      true
                              :notify/interval-days 30
                              :notify/min-priority 1
                              :notify/min-status   1}]))))))

(defn apply-notify-controls!
  "Apply a `Notify:` control found in `body-text`.  Only maintainers
  are allowed to change their notification preferences.  A non-
  maintainer attempt is logged and recorded as an
  `:insufficient-scope` failure (audience `:maintainers`), so it
  becomes visible to the lead maintainer — otherwise the attempt
  would leave no trace at all."
  [conn roles source-name from-addr body-text email-date]
  (when-let [[_ params-str] (re-find notify-pattern (or body-text ""))]
    (if (common/maintainer? roles from-addr)
      (let [params (parse-notify-params params-str)
            k      (notify-key source-name from-addr)
            txn    (cond-> {:notify/key    k
                            :notify/source source-name
                            :notify/email  (str/lower-case from-addr)}
                     (contains? params :enabled)       (assoc :notify/enabled (:enabled params))
                     (contains? params :interval-days)  (assoc :notify/interval-days (:interval-days params))
                     (contains? params :min-priority)   (assoc :notify/min-priority (:min-priority params))
                     (contains? params :min-status)     (assoc :notify/min-status (:min-status params))
                     (contains? params :subject-match)  (assoc :notify/subject-match (:subject-match params))
                     (contains? params :topic)          (assoc :notify/topic (:topic params)))]
        (d/transact! conn [txn])
        (log/info "Notify:" params-str (str "(for " from-addr " on " source-name ")")))
      (do (log/warn "Denied:" from-addr
                    "lacks permission for: Notify (non-maintainer)")
          (when (and from-addr source-name)
            (commands/record-failure!
             {:source     source-name
              :from-addr  from-addr
              :email-date email-date
              :report-mid ""
              :reason     :insufficient-scope
              :audience   :maintainers
              :command    (str "Notify: " params-str)}))))))

;; ---------------------------------------------------------------------------
;; Permission check for report creation (pure)
;; ---------------------------------------------------------------------------

(def announcement-types #{:announcement :release :change})

(defn can-create-report?
  "Check whether from-addr is permitted to create a report on this source.
  Announcements/releases/changes require maintainer status.
  All other report types are allowed (source-match gate already filtered)."
  [roles from-addr report-info email _source-cfg]
  (if (announcement-types (:type report-info))
    (common/maintainer? roles from-addr (:email/date-sent email))
    true))
