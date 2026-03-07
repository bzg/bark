;; bark-roles.clj — Role management and permission checks.
;;
;; Pure functions: admin?, maintainer?, ignored?, can-create-report?,
;;                 parse-role-commands
;; Effectful:      apply-role-commands!, ensure-source-roles!,
;;                 get-roles, ensure-notify-defaults!, apply-notify-commands!
;;
;; Usage: (load-file "scripts/bark-roles.clj")

(require '[clojure.string :as str])

;; ---------------------------------------------------------------------------
;; Role queries and checks (pure, given a roles map)
;; ---------------------------------------------------------------------------

(defn- roles-set [roles attr]
  (let [v (get roles attr)]
    (if (nil? v) #{} (set (if (string? v) [v] v)))))

(defn admin? [roles addr]
  (= (:roles/admin roles) addr))

(defn maintainer? [roles addr]
  (contains? (roles-set roles :roles/maintainers) addr))

(defn admin-or-maintainer? [roles addr]
  (or (admin? roles addr) (maintainer? roles addr)))

(defn ignored? [roles addr]
  (contains? (roles-set roles :roles/ignored) addr))

;; ---------------------------------------------------------------------------
;; Role DB operations
;; ---------------------------------------------------------------------------

(defn get-roles [db source-name]
  (or (d/pull db '[:roles/admin :roles/maintainers :roles/ignored]
              [:roles/source source-name])
      {}))

(defn ensure-source-roles! [conn config]
  (let [default-admin (:admin config)]
    (doseq [{:keys [name admin]} (:sources config)]
      (let [admin    (or admin default-admin)
            existing (d/q '[:find ?e .
                            :in $ ?src
                            :where [?e :roles/source ?src]]
                          (d/db conn) name)]
        (d/transact! conn [{:roles/source name
                            :roles/admin  admin}])
        (when-not existing
          (println (str "  Initialized roles for source " name
                        " (admin: " admin ")")))))))

(defn- roles-eid [conn source-name]
  (d/q '[:find ?e .
         :in $ ?src
         :where [?e :roles/source ?src]]
       (d/db conn) source-name))

(defn- add-role! [conn source-name attr addresses]
  (when-let [eid (roles-eid conn source-name)]
    (doseq [addr addresses]
      (d/transact! conn [[:db/add eid attr addr]]))))

(defn- remove-role! [conn source-name attr addresses]
  (when-let [eid (roles-eid conn source-name)]
    (doseq [addr addresses]
      (d/transact! conn [[:db/retract eid attr addr]]))))

;; ---------------------------------------------------------------------------
;; Role command parsing (pure) and application (effectful)
;; ---------------------------------------------------------------------------

(def role-command-pattern
  #"(?m)^(Add admin|Remove admin|Add maintainer|Remove maintainer|Ignore|Unignore):\s+(.+)$")

(defn- parse-addresses [s]
  (when s (remove str/blank? (str/split (str/trim s) #"\s+"))))

(defn parse-role-commands
  "Parse role commands from body text. Returns a vector of
  {:command \"...\" :addresses [\"...\"]} maps."
  [body-text]
  (when body-text
    (->> (re-seq role-command-pattern body-text)
         (mapv (fn [[_ cmd addrs]]
                 {:command cmd :addresses (parse-addresses addrs)})))))

(def ^:private role-dispatch
  {"Add admin"         {:requires :admin  :action :set-admin}
   "Remove admin"      {:requires :admin  :action :noop :msg "cannot remove admin (use Add admin to replace)"}
   "Remove maintainer" {:requires :admin  :attr :roles/maintainers :action :remove}
   "Unignore"          {:requires :admin  :attr :roles/ignored     :action :remove}
   "Add maintainer"    {:requires :maint  :attr :roles/maintainers :action :add}
   "Ignore"            {:requires :maint  :attr :roles/ignored     :action :add}})

(defn apply-role-commands! [conn roles source-name from-addr body-text]
  (let [commands  (parse-role-commands body-text)
        is-admin  (admin? roles from-addr)
        is-maint  (admin-or-maintainer? roles from-addr)]
    (doseq [{:keys [command addresses]} commands]
      (when-let [{:keys [requires action attr msg]} (role-dispatch command)]
        (if (case requires :admin is-admin :maint is-maint)
          (case action
            :set-admin (when-let [new-admin (first addresses)]
                         (d/transact! conn [{:roles/source source-name
                                             :roles/admin  new-admin}])
                         (println (str "    → set admin: " new-admin " (for " source-name ")")))
            :add       (do (add-role! conn source-name attr addresses)
                           (println (str "    → " (str/lower-case command) ": "
                                         (str/join " " addresses) " (for " source-name ")")))
            :remove    (do (remove-role! conn source-name attr addresses)
                           (println (str "    → " (str/lower-case command) ": "
                                         (str/join " " addresses) " (for " source-name ")")))
            :noop      (println (str "    → " msg)))
          (println (str "    [denied] " from-addr " lacks permission for: " command)))))))

;; ---------------------------------------------------------------------------
;; Notify command parsing and application
;; ---------------------------------------------------------------------------

(def ^:private notify-pattern
  #"(?m)^Notify:\s+(.+)$")

(defn- parse-notify-params
  "Parse 'on', 'off', or param string like 'd:7 p:2 s:4'.
  Supports 'on'/'off' as prefix combined with params, e.g. 'on d:7 p:2'.
  Returns map with :enabled, :interval-days, :min-priority, :min-status."
  [s]
  (let [s (str/trim s)
        lc (str/lower-case s)]
    (cond
      (= lc "on")  {:enabled true}
      (= lc "off") {:enabled false}
      :else
      (let [has-on?  (str/starts-with? lc "on ")
            has-off? (str/starts-with? lc "off ")
            params   (re-seq #"([dps]):(\d+)" s)
            base     (cond has-on?  {:enabled true}
                           has-off? {:enabled false}
                           :else    {})]
        (reduce (fn [m [_ k v]]
                  (case k
                    "d" (assoc m :interval-days (parse-long v))
                    "p" (assoc m :min-priority (parse-long v))
                    "s" (assoc m :min-status (parse-long v))
                    m))
                base params)))))

(defn- notify-key [source-name email]
  (str source-name ":" (str/lower-case email)))

(defn ensure-notify-defaults!
  "Create default notify prefs for admin+maintainers who don't have one yet."
  [conn source-name roles]
  (let [admin  (:roles/admin roles)
        maints (let [v (:roles/maintainers roles)]
                 (cond (nil? v) [] (string? v) [v] :else v))
        emails (distinct (remove nil? (cons admin maints)))]
    (doseq [email emails]
      (let [k (notify-key source-name email)]
        (when-not (d/q '[:find ?e .
                         :in $ ?k
                         :where [?e :notify/key ?k]]
                       (d/db conn) k)
          (d/transact! conn [{:notify/key          k
                              :notify/source       source-name
                              :notify/email        (str/lower-case email)
                              :notify/enabled      true
                              :notify/interval-days 30
                              :notify/min-priority 1
                              :notify/min-status   1}]))))))

(defn apply-notify-commands!
  "Parse and apply Notify: commands from email body.
  Only admin/maintainers can set their own notification prefs."
  [conn roles source-name from-addr body-text]
  (when-let [[_ params-str] (re-find notify-pattern (or body-text ""))]
    (when (admin-or-maintainer? roles from-addr)
      (let [params (parse-notify-params params-str)
            k      (notify-key source-name from-addr)
            base   {:notify/key    k
                    :notify/source source-name
                    :notify/email  (str/lower-case from-addr)}
            txn    (cond-> base
                     (contains? params :enabled)       (assoc :notify/enabled (:enabled params))
                     (contains? params :interval-days)  (assoc :notify/interval-days (:interval-days params))
                     (contains? params :min-priority)   (assoc :notify/min-priority (:min-priority params))
                     (contains? params :min-status)     (assoc :notify/min-status (:min-status params)))]
        (d/transact! conn [txn])
        (println (str "    → notify: " params-str " (for " from-addr " on " source-name ")"))))))

;; ---------------------------------------------------------------------------
;; Permission check for report creation (pure)
;; ---------------------------------------------------------------------------

(def announcement-types #{:announcement :release :change})

(defn- from-mailing-list?
  "True if the email was delivered through a mailing list (has List-Id header)."
  [email]
  (some? (get-header (:email/headers-edn email) "List-Id")))

(defn- list-post-address
  "Extract the email address from the List-Post header, e.g.
  \"<mailto:list@example.org>\" → \"list@example.org\".  Returns nil if absent."
  [email]
  (when-let [lp (get-header (:email/headers-edn email) "List-Post")]
    (second (re-find #"<mailto:([^>]+)>" lp))))

(defn can-create-report?
  "Check if from-addr is allowed to create this report.
  Announcements require admin/maintainer.
  On list-backed sources, non-privileged users must send through
  the list (List-Post header matches :list-post)."
  [roles from-addr report-info email source-cfg]
  (cond
    (announcement-types (:type report-info))
    (admin-or-maintainer? roles from-addr)

    (admin-or-maintainer? roles from-addr)
    true

    :else
    (let [ml-email (:list-post source-cfg)]
      (if (nil? ml-email)
        true
        (let [lp (list-post-address email)]
          (when (and lp (not= lp ml-email))
            (println (str "    [list-post mismatch] expected " ml-email " got " lp)))
          (= lp ml-email))))))

(defn from-mailing-list-email?
  "True if the email was delivered through a mailing list."
  [email]
  (from-mailing-list? email))
