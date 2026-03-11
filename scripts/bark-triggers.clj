;; bark-triggers.clj — Trigger detection, voting, and application.
;;
;; Pure functions: detect-triggers, detect-vote, build-source-triggers
;; Effectful:      apply-triggers!, apply-vote!
;;
;; Usage: (load-file "scripts/bark-triggers.clj")

(require '[clojure.string :as str])

;; ---------------------------------------------------------------------------
;; Trigger pattern compilation
;; ---------------------------------------------------------------------------

(defn- trigger-pattern [& words]
  (re-pattern (str "(?m)^(" (str/join "|" (map #(java.util.regex.Pattern/quote %) words)) ")[.,;:]")))

(defn- match-triggers [triggers body-text]
  (into {} (keep (fn [[k p]] (when (re-find p body-text) [(keyword "report" (name k)) true]))) triggers))

;; ---------------------------------------------------------------------------
;; Trigger defaults and compilation
;; ---------------------------------------------------------------------------

(def default-trigger-words
  "Default trigger words per report type and action."
  {:bug          {:acked ["Approved" "Confirmed"] :owned ["Handled"] :closed ["Canceled" "Fixed"]}
   :patch        {:acked ["Approved" "Reviewed"]  :owned ["Handled"] :closed ["Canceled" "Applied"]}
   :request      {:acked ["Approved"]             :owned ["Handled"] :closed ["Canceled" "Done" "Closed"]}
   :announcement {:closed ["Canceled"]}
   :release      {:closed ["Canceled"]}
   :change       {:closed ["Canceled"]}})

(defn- compile-trigger-words
  "Compile a map of action->word-lists into action->regex-patterns."
  [action-map]
  (into {} (map (fn [[action words]] [action (apply trigger-pattern words)])) action-map))

(defn- compile-triggers-by-type
  "Compile a full type->action->words map into type->action->pattern."
  [tw]
  (into {} (map (fn [[rtype actions]] [rtype (compile-trigger-words actions)])) tw))

(def default-triggers-by-type (compile-triggers-by-type default-trigger-words))

(defn- deep-merge-triggers
  "Merge overrides into base trigger-words, merging per-type action maps."
  [base overrides]
  (reduce-kv (fn [acc rtype actions]
               (assoc acc rtype (merge (get acc rtype) actions)))
             base overrides))

(defn build-source-triggers
  "Merge triggers for a source: defaults -> global -> per-source.
  Returns compiled type->action->pattern map."
  [source-cfg]
  (let [global  (:global-triggers source-cfg)
        per-src (:triggers source-cfg)
        merged  (cond-> default-trigger-words
                  global  (deep-merge-triggers global)
                  per-src (deep-merge-triggers per-src))]
    (if (or global per-src)
      (compile-triggers-by-type merged)
      default-triggers-by-type)))

;; ---------------------------------------------------------------------------
;; Priority triggers
;; ---------------------------------------------------------------------------

(def report-priority-triggers
  {:urgent (trigger-pattern "Urgent") :important (trigger-pattern "Important")})

(def report-types-with-priority #{:bug :patch :request})

;; ---------------------------------------------------------------------------
;; Trigger detection (pure)
;; ---------------------------------------------------------------------------

(defn detect-triggers
  "Detect trigger words in body text for a given report type.
  Returns a map of {attr true ...} or nil."
  [report-type body-text triggers-by-type]
  (when body-text
    (let [sets     (when-let [t (triggers-by-type report-type)] (match-triggers t body-text))
          priority (when (report-types-with-priority report-type) (match-triggers report-priority-triggers body-text))
          all-sets (merge sets priority)]
      (when (seq all-sets) all-sets))))

;; ---------------------------------------------------------------------------
;; Vote detection (pure)
;; ---------------------------------------------------------------------------

(def vote-up-pattern   #"(?m)(?:^|\s)(?:\+1|1\+)(?![a-zA-Z0-9])")
(def vote-down-pattern #"(?m)(?:^|\s)(?:-1|1-)(?![a-zA-Z0-9])")
(def vote-null-pattern #"(?m)(?:^|\s)(?:\+0|0\+|-0|0-)(?![a-zA-Z0-9])")

(defn detect-vote
  "Detect a vote in body text. Returns :up, :down, :null, or nil."
  [body-text]
  (when body-text
    (cond
      (re-find vote-up-pattern body-text)   :up
      (re-find vote-down-pattern body-text) :down
      (re-find vote-null-pattern body-text) :null)))

;; ---------------------------------------------------------------------------
;; Trigger and vote application (effectful)
;; ---------------------------------------------------------------------------

(defn- ref-eid [v] (if (map? v) (:db/id v) v))

(def state-attrs [:report/acked :report/owned :report/closed :report/urgent :report/important])

(def proxy-attrs
  {:report/acked     :report/acked-proxy
   :report/owned     :report/owned-proxy
   :report/closed    :report/closed-proxy
   :report/urgent    :report/urgent-proxy
   :report/important :report/important-proxy})

;; ---------------------------------------------------------------------------
;; Maintainer directives: Acked-by, Owned-by, Closed-by, Urgent-by,
;; Important-by, Unacked, Unowned, Unclosed, Unurgent, Unimportant
;; ---------------------------------------------------------------------------

(def directive-by-pattern
  #"(?m)^(Acked|Owned|Closed|Urgent|Important)-by:\s+(\S+@\S+)[.,;:]?\s*$")

(def directive-un-pattern
  #"(?m)^(Unacked|Unowned|Unclosed|Unurgent|Unimportant)[.,;:]?\s*$")

(def ^:private directive-attr
  {"Acked"     :report/acked
   "Owned"     :report/owned
   "Closed"    :report/closed
   "Urgent"    :report/urgent
   "Important" :report/important})

(def ^:private un-directive-attr
  {"Unacked"      :report/acked
   "Unowned"      :report/owned
   "Unclosed"     :report/closed
   "Unurgent"     :report/urgent
   "Unimportant"  :report/important})

;; ---------------------------------------------------------------------------
;; Deadline directive — Org timestamp parsing
;; ---------------------------------------------------------------------------

(def ^:private deadline-pattern
  "Matches 'Deadline: 2026-03-10.' (with trailing punctuation)."
  #"(?m)^Deadline:\s+(\d{4}-\d{2}-\d{2})[.,;:]?\s*$")

(def ^:private undeadline-pattern
  #"(?m)^Undeadline[.,;:]?\s*$")

(defn- parse-org-date
  "Parse an ISO date string (yyyy-MM-dd) into a java.util.Date at midnight UTC."
  [s]
  (try
    (let [fmt (java.text.SimpleDateFormat. "yyyy-MM-dd")]
      (.setTimeZone fmt (java.util.TimeZone/getTimeZone "UTC"))
      (.parse fmt s))
    (catch Exception _ nil)))

(defn detect-directives
  "Parse maintainer directives from body text.
  Returns a seq of actions in order, each {:action :set/:unset :attr ... :addr/:date ...}.
  Last-one-wins is handled by the caller."
  [body-text]
  (when body-text
    (let [lines (str/split-lines body-text)]
      (->> lines
           (keep (fn [line]
                   (or (when-let [[_ verb addr] (re-matches directive-by-pattern line)]
                         {:action :set :attr (directive-attr verb) :addr addr})
                       (when-let [[_ verb] (re-matches directive-un-pattern line)]
                         {:action :unset :attr (un-directive-attr verb)})
                       (when-let [[_ date-str] (re-matches deadline-pattern line)]
                         (when-let [d (parse-org-date date-str)]
                           {:action :set-deadline :date d}))
                       (when (re-matches undeadline-pattern line)
                         {:action :unset-deadline}))))
           vec))))

(defn resolve-directives
  "Given a seq of directive actions, apply last-one-wins per attribute.
  Returns {:set {attr addr ...} :unset #{attr ...} :deadline date-or-nil :undeadline? bool}."
  [directives]
  (reduce (fn [acc {:keys [action attr addr date]}]
            (case action
              :set   (-> acc
                         (assoc-in [:set attr] addr)
                         (update :unset disj attr))
              :unset (-> acc
                         (update :set dissoc attr)
                         (update :unset conj attr))
              :set-deadline   (-> acc
                                  (assoc :deadline date)
                                  (dissoc :undeadline?))
              :unset-deadline (-> acc
                                  (dissoc :deadline)
                                  (assoc :undeadline? true))))
          {:set {} :unset #{}}
          directives))

(defn find-or-create-synthetic-email!
  "Return the eid of a synthetic email entity for `addr`.
  Uses a deterministic Message-ID keyed on (attr, addr, report-message-id)
  so repeated processing is idempotent."
  [conn addr report-message-id attr-name]
  (let [synthetic-mid (str "<bark-synthetic-" (name attr-name) "-"
                           addr "-" report-message-id ">")
        existing      (d/q '[:find ?e .
                             :in $ ?mid
                             :where [?e :email/message-id ?mid]]
                           (d/db conn) synthetic-mid)]
    (if existing
      existing
      (do (d/transact! conn [{:email/message-id   synthetic-mid
                              :email/from-address addr
                              :email/date-sent    (java.util.Date.)
                              :email/subject      (str "Synthetic: " (name attr-name)
                                                       " for " report-message-id)}])
          (d/q '[:find ?e .
                 :in $ ?mid
                 :where [?e :email/message-id ?mid]]
               (d/db conn) synthetic-mid)))))

(defn apply-directives!
  "Apply maintainer proxy directives to a report.
  `email` is the maintainer's email entity (used as the proxy ref)."
  [conn report-eid email roles]
  (let [body-text  (or (:email/body-text email) (:email/body-text-from-html email))
        from-addr  (:email/from-address email)
        directives (detect-directives body-text)]
    (when (and (seq directives) (admin-or-maintainer? roles from-addr))
      (let [{:keys [set unset deadline undeadline?]} (resolve-directives directives)
            report-mid (d/q '[:find ?mid . :in $ ?r
                              :where [?r :report/message-id ?mid]]
                            (d/db conn) report-eid)
            proxy-eid  (:db/id email)
            current    (d/pull (d/db conn) (conj state-attrs :report/deadline) report-eid)
            ;; Build set transactions
            set-tx (mapcat (fn [[attr addr]]
                             (let [target-eid (find-or-create-synthetic-email!
                                               conn addr report-mid attr)]
                               [[:db/add report-eid attr target-eid]
                                [:db/add report-eid (proxy-attrs attr) proxy-eid]]))
                           set)
            ;; Build unset transactions
            unset-tx (mapcat (fn [attr]
                               (when-let [cur (get current attr)]
                                 (let [retract [[:db/retract report-eid attr (ref-eid cur)]]
                                       proxy-attr (proxy-attrs attr)
                                       proxy-cur  (get (d/pull (d/db conn) [proxy-attr] report-eid)
                                                       proxy-attr)]
                                   (if proxy-cur
                                     (conj retract [:db/retract report-eid proxy-attr (ref-eid proxy-cur)])
                                     retract))))
                             unset)
            ;; Deadline transactions
            deadline-tx (cond
                          deadline    [[:db/add report-eid :report/deadline deadline]]
                          undeadline? (when (:report/deadline current)
                                        [[:db/retract report-eid :report/deadline
                                          (:report/deadline current)]])
                          :else       nil)
            all-tx (vec (concat set-tx unset-tx deadline-tx))
            ;; Log description
            desc (concat (map (fn [[attr addr]] (str (name attr) " -> " addr)) set)
                         (map #(str "un-" (name %)) unset)
                         (when deadline [(str "deadline " deadline)])
                         (when undeadline? ["undeadline"]))]
        (when (seq all-tx)
          (d/transact! conn all-tx)
          (log/info "Directives:" (str/join ", " desc)
                        "(proxy by" from-addr ")"))))))


(defn apply-vote! [conn report-eid from-addr body-text]
  ;; NB: read-then-write is safe because cmd-digest! processes emails
  ;; sequentially. If parallelized, this needs a transaction function.
  (when-let [vote (detect-vote body-text)]
    (let [db      (d/db conn)
          current (d/pull db [:report/voters :report/votes-up :report/votes-down
                              :report/votes-null] report-eid)
          voters  (set (:report/voters current))]
      (when-not (contains? voters from-addr)
        (let [attr (case vote :up :report/votes-up :down :report/votes-down :report/votes-null)
              n    (or (get current attr) 0)]
          (d/transact! conn [[:db/add report-eid attr (inc n)]
                             [:db/add report-eid :report/voters from-addr]])
          (log/info "Vote" (case vote :up "+1" :down "-1" "0") "by" from-addr))))))

(defn apply-triggers! [conn report-eid report-type email source-map]
  (let [body-text  (or (:email/body-text email) (:email/body-text-from-html email))
        from-addr  (:email/from-address email)
        src-name   (d/q '[:find ?src . :in $ ?rid :where
                          [?rid :report/email ?e] [?e :email/source ?src]]
                        (d/db conn) report-eid)
        triggers   (build-source-triggers (get source-map src-name))
        result     (detect-triggers report-type body-text triggers)]
    (when (and (= :request report-type) from-addr body-text)
      (apply-vote! conn report-eid from-addr body-text))
    (when result
      (let [eid      (:db/id email)
            current  (d/pull (d/db conn) state-attrs report-eid)
            new-sets (into {} (remove (fn [[k _]] (get current k))) result)
            set-tx   (when (seq new-sets)
                       [(into {:db/id report-eid} (map (fn [[k _]] [k eid])) new-sets)])]
        (when (seq set-tx)
          (d/transact! conn set-tx)
          (log/info (str/join ", " (map (comp name key) new-sets))
                        "(by" (:email/message-id email) ")"))))))
