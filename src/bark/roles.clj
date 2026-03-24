;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.roles
  "Role management, controls, and permission checks."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [taoensso.timbre :as log]
            [bark.common :as common]
            [bark.tracking :as tracking])
  (:import [java.util Date]))

;; ---------------------------------------------------------------------------
;; Role queries and checks (pure, given a roles map)
;; ---------------------------------------------------------------------------

;; Pure checks are defined in bark.common (no datalevin dependency).
;; These re-exports are the canonical entry points for all callers
;; outside bark.common — prefer roles/admin? over common/admin? etc.
(def admin?
  "True if addr is the admin for this source. Canonical entry point."
  common/admin?)
(def maintainer?
  "True if addr is a maintainer. 2-arity ignores temporal constraints,
  3-arity checks maintainer-since dates. Canonical entry point."
  common/maintainer?)
(def admin-or-maintainer?
  "True if addr is admin or maintainer. Canonical entry point."
  common/admin-or-maintainer?)
(def ignored?
  "True if addr is on the ignored list. Canonical entry point."
  common/ignored?)

;; ---------------------------------------------------------------------------
;; Role DB operations
;; ---------------------------------------------------------------------------

(defn get-roles [db source-name]
  (or (d/pull db '[:roles/admin :roles/maintainers :roles/maintainer-since :roles/ignored]
              [:roles/source source-name])
      {}))

(defn- roles-eid [conn source-name]
  (d/q '[:find ?e .
         :in $ ?src
         :where [?e :roles/source ?src]]
       (d/db conn) source-name))

(defn ensure-source-roles! [conn config]
  (let [default-admin (:admin config)]
    (doseq [{:keys [name admin maintainers]} (:sources config)]
      (let [admin    (or admin default-admin)
            existing (d/q '[:find ?e .
                            :in $ ?src
                            :where [?e :roles/source ?src]]
                          (d/db conn) name)]
        (d/transact! conn [{:roles/source name :roles/admin admin}])
        (when-not existing
          (log/info "Initialized roles for source" name (str "(admin: " admin ")")))
        (when (seq maintainers)
          (let [eid     (roles-eid conn name)
                current (common/ensure-set (:roles/maintainers (get-roles (d/db conn) name)))]
            (when eid
              (let [new-maints (remove (fn [{:keys [email]}]
                                         (or (nil? email)
                                             (contains? current (str/lower-case email))))
                                       maintainers)]
                (when (seq new-maints)
                  (let [tx (into []
                                 (mapcat (fn [{:keys [email since]}]
                                           (let [addr (str/lower-case email)]
                                             (cond-> [[:db/add eid :roles/maintainers addr]]
                                               since (conj [:db/add eid :roles/maintainer-since
                                                            (str addr ":" since)])))))
                                 new-maints)]
                    (d/transact! conn tx)
                    (doseq [{:keys [email since]} new-maints]
                      (log/info (str "Config maintainer: " (str/lower-case email)
                                     (when since (str " (since " since ")"))
                                     " (for " name ")")))))))))))))

;; Phase 0 fix: idempotent add-role! / remove-role!
(defn- add-role!
  "Add addresses to a role attr. Returns true if any change was made."
  [conn source-name attr addresses]
  (when-let [eid (roles-eid conn source-name)]
    (let [current   (common/ensure-set (get (get-roles (d/db conn) source-name) attr))
          new-addrs (remove #(contains? current (str/lower-case %)) addresses)]
      (when (seq new-addrs)
        (d/transact! conn (mapv (fn [addr] [:db/add eid attr (str/lower-case addr)]) new-addrs))
        true))))

(defn- remove-role!
  "Remove addresses from a role attr. Returns true if any change was made."
  [conn source-name attr addresses]
  (when-let [eid (roles-eid conn source-name)]
    (let [current   (common/ensure-set (get (get-roles (d/db conn) source-name) attr))
          to-remove (filter #(contains? current (str/lower-case %)) addresses)]
      (when (seq to-remove)
        (d/transact! conn (mapv (fn [addr] [:db/retract eid attr (str/lower-case addr)]) to-remove))
        true))))

;; ---------------------------------------------------------------------------
;; Role control parsing and application
;; ---------------------------------------------------------------------------

(def role-control-pattern
  #"(?m)^(Add maintainer|Remove maintainer|Ignore|Unignore):\s+(.+)$")

(defn- parse-addresses [s]
  (when s (remove str/blank? (str/split (str/trim s) #"\s+"))))

(defn parse-role-controls [body-text]
  (when body-text
    (->> (re-seq role-control-pattern body-text)
         (mapv (fn [[_ cmd addrs]]
                 {:command cmd :addresses (parse-addresses addrs)})))))

(defn- set-maintainer-since! [conn source-name addresses date]
  (when-let [eid (roles-eid conn source-name)]
    (let [roles    (get-roles (d/db conn) source-name)
          entries  (common/ensure-set (:roles/maintainer-since roles))
          date-str (when date
                     (if (string? date) date
                         (common/format-date-iso date)))]
      (doseq [addr addresses]
        (let [target   (when date-str (str (str/lower-case addr) ":" date-str))
              prefix   (str (str/lower-case addr) ":")
              existing (some #(when (str/starts-with? % prefix) %) entries)]
          (when (not= existing target)
            (when existing
              (d/transact! conn [[:db/retract eid :roles/maintainer-since existing]]))
            (when target
              (d/transact! conn [[:db/add eid :roles/maintainer-since target]]))))))))

(def ^:private role-dispatch
  {"Remove maintainer" {:requires :admin :attr :roles/maintainers :action :remove
                        :post-fn (fn [conn src addrs _date]
                                   (set-maintainer-since! conn src addrs nil))}
   "Unignore"          {:requires :admin :attr :roles/ignored     :action :remove}
   "Add maintainer"    {:requires :maint :attr :roles/maintainers :action :add
                        :post-fn (fn [conn src addrs date]
                                   (set-maintainer-since! conn src addrs date))}
   "Ignore"            {:requires :maint :attr :roles/ignored     :action :add}})

(defn apply-role-controls! [conn roles source-name from-addr body-text email-date]
  (let [controls (parse-role-controls body-text)
        perms    {:admin (admin? roles from-addr) :maint (admin-or-maintainer? roles from-addr)}]
    (doseq [{:keys [command addresses]} controls]
      (when-let [{:keys [requires action attr post-fn]} (role-dispatch command)]
        (if (perms requires)
          (when ((case action :add add-role! :remove remove-role!)
                 conn source-name attr addresses)
            (when post-fn (post-fn conn source-name addresses email-date))
            (tracking/bump-global-modified! conn)
            (log/info (str/lower-case command) ":"
                      (str/join " " addresses) (str "(for " source-name ")")))
          (log/warn "Denied:" from-addr "lacks permission for:" command))))))

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

(defn ensure-notify-defaults! [conn source-name roles]
  (let [admin  (:roles/admin roles)
        maints (common/ensure-set (:roles/maintainers roles))
        emails (distinct (remove nil? (cons admin maints)))]
    (doseq [email emails]
      (let [k (notify-key source-name email)]
        (when-not (d/q '[:find ?e . :in $ ?k :where [?e :notify/key ?k]]
                       (d/db conn) k)
          (d/transact! conn [{:notify/key          k
                              :notify/source       source-name
                              :notify/email        (str/lower-case email)
                              :notify/enabled      true
                              :notify/interval-days 30
                              :notify/min-priority 1
                              :notify/min-status   1}]))))))

(defn apply-notify-controls! [conn roles source-name from-addr body-text]
  (when-let [[_ params-str] (re-find notify-pattern (or body-text ""))]
    (when (admin-or-maintainer? roles from-addr)
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
        (log/info "Notify:" params-str (str "(for " from-addr " on " source-name ")"))))))

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
    (maintainer? roles from-addr (:email/date-sent email))
    true))
