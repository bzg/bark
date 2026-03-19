;; bark-roles.clj — Role management and permission checks.
;;
;; Pure functions: admin?, maintainer?, ignored?, can-create-report?,
;;                 parse-role-commands
;; Effectful:      apply-role-commands!, ensure-source-roles!,
;;                 get-roles, ensure-notify-defaults!, apply-notify-commands!
;;
;; Usage: (load-file "scripts/bark-roles.clj")

(require '[clojure.string :as str])

;; Defined in bark-common.clj; forward-declared for clj-kondo.
(declare get-header parse-maintainer-since-entries)

;; ---------------------------------------------------------------------------
;; Role queries and checks (pure, given a roles map)
;; ---------------------------------------------------------------------------

(defn- roles-set [roles attr]
  (let [v (get roles attr)]
    (if (nil? v) #{} (set (if (string? v) [v] v)))))

(defn- has-role?
  "True if addr (case-insensitive) appears in the multi-valued role attr."
  [roles attr addr]
  (let [addrs (roles-set roles attr)]
    (boolean (some #(= (str/lower-case %) (str/lower-case addr)) addrs))))

(defn admin? [roles addr]
  (and addr (:roles/admin roles)
       (= (str/lower-case (:roles/admin roles))
          (str/lower-case addr))))

(defn- parse-maintainer-since
  "Parse :roles/maintainer-since entries into a map of lower-cased email -> java.util.Date.
  Uses the shared parse-maintainer-since-entries for string splitting."
  [roles]
  (let [fmt     (doto (java.text.SimpleDateFormat. "yyyy-MM-dd")
                  (.setTimeZone (java.util.TimeZone/getTimeZone "UTC")))
        entries (parse-maintainer-since-entries roles)]
    (into {}
          (keep (fn [[email date-str]]
                  (try [email (.parse fmt date-str)]
                       (catch Exception _ nil))))
          entries)))

(defn maintainer?
  "True if addr is a maintainer.  When `as-of` (a java.util.Date) is
  provided, config-seeded maintainers are only active on or after their
  :since date.  Directive-added maintainers (no :since entry) are active
  at any date."
  ([roles addr]
   (and addr (has-role? roles :roles/maintainers addr)))
  ([roles addr as-of]
   (and addr
        (has-role? roles :roles/maintainers addr)
        (if as-of
          (let [since-map (parse-maintainer-since roles)
                since     (get since-map (str/lower-case addr))]
            ;; No since entry = directive-added or config without :since → always active
            (or (nil? since)
                (not (.before ^java.util.Date as-of since))))
          true))))

(defn admin-or-maintainer? [roles addr]
  (or (admin? roles addr) (maintainer? roles addr)))

(defn ignored? [roles addr]
  (and addr (has-role? roles :roles/ignored addr)))

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
        (d/transact! conn [{:roles/source name
                            :roles/admin  admin}])
        (when-not existing
          (log/info "Initialized roles for source" name "(admin:" admin ")"))
        ;; Seed config-declared maintainers
        (when (seq maintainers)
          (let [eid (roles-eid conn name)]
            (when eid
              (doseq [{:keys [email since]} maintainers]
                (when email
                  (let [addr (str/lower-case email)]
                    ;; Add to :roles/maintainers if not already present.
                    ;; Since-date is only set on first seed — directive
                    ;; "Add/Remove maintainer:" overrides config and clears
                    ;; the since-constraint, so we must not restore it here.
                    (when-not (has-role? (get-roles (d/db conn) name) :roles/maintainers addr)
                      (d/transact! conn [[:db/add eid :roles/maintainers addr]])
                      (when since
                        (d/transact! conn [[:db/add eid :roles/maintainer-since
                                            (str addr ":" since)]]))
                      (log/info "Config maintainer:" addr
                                (if since (str "(since " since ")") "")
                                "(for" name ")"))))))))))))

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
  #"(?m)^(Add maintainer|Remove maintainer|Ignore|Unignore):\s+(.+)$")

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
  {"Remove maintainer" {:requires :admin  :attr :roles/maintainers :action :remove}
   "Unignore"          {:requires :admin  :attr :roles/ignored     :action :remove}
   "Add maintainer"    {:requires :maint  :attr :roles/maintainers :action :add}
   "Ignore"            {:requires :maint  :attr :roles/ignored     :action :add}})

(defn- set-maintainer-since!
  "Set :roles/maintainer-since for the given addresses to the specified date.
  Removes any existing entry first, then adds the new one.
  Re-reads roles from DB to get current entries — acceptable since this
  runs at most once per role-command email, not in a hot loop."
  [conn source-name addresses date]
  (when-let [eid (roles-eid conn source-name)]
    (let [roles   (get-roles (d/db conn) source-name)
          entries (roles-set roles :roles/maintainer-since)]
      (doseq [addr addresses]
        (let [prefix (str (str/lower-case addr) ":")]
          ;; Remove old entry
          (doseq [entry entries]
            (when (str/starts-with? entry prefix)
              (d/transact! conn [[:db/retract eid :roles/maintainer-since entry]])))
          ;; Add new entry with date
          (when date
            (let [date-str (if (string? date)
                             date
                             (let [fmt (java.text.SimpleDateFormat. "yyyy-MM-dd")]
                               (.setTimeZone fmt (java.util.TimeZone/getTimeZone "UTC"))
                               (.format fmt date)))]
              (d/transact! conn [[:db/add eid :roles/maintainer-since
                                  (str (str/lower-case addr) ":" date-str)]]))))))))

(defn apply-role-commands! [conn roles source-name from-addr body-text email-date]
  ;; Permission check uses the non-temporal admin-or-maintainer? deliberately:
  ;; role management is an administrative operation, so config-seeded
  ;; maintainers can issue role commands regardless of their :since date.
  ;; Note: `roles` is read once before the doseq — if one command changes
  ;; who is a maintainer, subsequent commands still use the original snapshot.
  ;; This is safe because the sender's own permission doesn't change mid-email.
  (let [commands  (parse-role-commands body-text)
        is-admin  (admin? roles from-addr)
        is-maint  (admin-or-maintainer? roles from-addr)]
    (doseq [{:keys [command addresses]} commands]
      (when-let [{:keys [requires action attr]} (role-dispatch command)]
        (if (case requires :admin is-admin :maint is-maint)
          (do ((case action :add add-role! :remove remove-role!)
               conn source-name attr addresses)
              ;; Update since-date for maintainer changes
              (when (= attr :roles/maintainers)
                (set-maintainer-since! conn source-name addresses
                                       (when (= action :add) email-date)))
              ;; Signal change so bb export sees it
              (bump-global-modified! conn)
              (log/info (str/lower-case command) ":"
                        (str/join " " addresses) "(for" source-name ")"))
          (log/warn "Denied:" from-addr "lacks permission for:" command))))))

;; ---------------------------------------------------------------------------
;; Notify command parsing and application
;; ---------------------------------------------------------------------------

(def ^:private notify-pattern
  #"(?m)^Notify:\s+(.+)$")

(defn- parse-notify-params
  "Parse 'on', 'off', or param string like 'd:7 p:2 s:4 m:foo t:bar'.
  Supports 'on'/'off' as prefix combined with params, e.g. 'on d:7 p:2'.
  Returns map with :enabled, :interval-days, :min-priority, :min-status,
  :subject-match, :topic."
  [s]
  (let [s (str/trim s)
        lc (str/lower-case s)]
    (cond
      (= lc "on")  {:enabled true}
      (= lc "off") {:enabled false}
      :else
      (let [has-on?  (str/starts-with? lc "on ")
            has-off? (str/starts-with? lc "off ")
            params   (re-seq #"([dpsmt]):(\S+)" s)
            base     (cond has-on?  {:enabled true}
                           has-off? {:enabled false}
                           :else    {})]
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
                     (contains? params :min-status)     (assoc :notify/min-status (:min-status params))
                     (contains? params :subject-match)  (assoc :notify/subject-match (:subject-match params))
                     (contains? params :topic)          (assoc :notify/topic (:topic params)))]
        (d/transact! conn [txn])
        (log/info "Notify:" params-str "(for" from-addr "on" source-name ")")))))

;; ---------------------------------------------------------------------------
;; Permission check for report creation (pure)
;; ---------------------------------------------------------------------------

(def announcement-types #{:announcement :release :change})

(defn from-mailing-list?
  "True if the email was delivered through a mailing list.
  Requires both List-Id and List-Post headers — a manually added List-Id
  (for source classification) is not enough."
  [email]
  (let [hdrs (:email/headers-edn email)]
    (and (some? (get-header hdrs "List-Id"))
         (some? (get-header hdrs "List-Post")))))

(defn- list-post-address
  "Extract the email address from the List-Post header, e.g.
  \"<mailto:list@example.org>\" -> \"list@example.org\".  Returns nil if absent."
  [email]
  (when-let [lp (get-header (:email/headers-edn email) "List-Post")]
    (second (re-find #"<mailto:([^>]+)>" lp))))

(defn can-create-report?
  "Check if from-addr is allowed to create this report.
  Announcements require maintainer status (time-aware).
  On list-backed sources, non-privileged users must send through
  the list (List-Post header matches :list-post).
  Note: admin does not imply maintainer — the admin must be explicitly
  added as a maintainer to gain maintainer privileges."
  [roles from-addr report-info email source-cfg]
  (let [as-of (:email/date-sent email)]
    (cond
      (announcement-types (:type report-info))
      (maintainer? roles from-addr as-of)

      (maintainer? roles from-addr as-of)
      true

      :else
      (let [ml-email (:list-post source-cfg)]
        (if (nil? ml-email)
          true
          (let [lp (list-post-address email)]
            (when (and lp (not= lp ml-email))
              (log/warn "List-post mismatch: expected" ml-email "got" lp))
            (boolean (= lp ml-email))))))))

