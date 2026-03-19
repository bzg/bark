;; bark-commands.clj — Unified command detection, resolution, and application.
;;
;; Commands are either:
;;   - triggers   (:kind :trigger)   — single-word, any user can fire
;;   - directives (:kind :directive) — parameterized, maintainer-only
;;
;; Pure functions: detect-commands, detect-vote, resolve-commands,
;;                 build-source-commands
;; Effectful:      apply-commands!
;;
;; Usage: (load-file "scripts/bark-commands.clj")

(require '[clojure.string :as str])

;; Defined in bark-common.clj / bark-roles.clj; forward-declared for clj-kondo.
(declare default-commands close-reasons
         resolve-commands-map
         email-body-text report-priority maintainer?
         bump-report-updated!)

;; ---------------------------------------------------------------------------
;; Trailing punctuation (shared across all command patterns)
;; ---------------------------------------------------------------------------

(def trailing-punct
  "Character class for optional trailing punctuation after command words."
  "[.,;:?!]")

;; ---------------------------------------------------------------------------
;; Command registry
;; ---------------------------------------------------------------------------

(def commands
  "Declarative registry of all BARK commands.
  :kind      — :trigger (single-word, any user) or :directive (parameterized, maintainer)
  :attr      — the report attribute affected
  :scope     — :user (anyone) or :maintainer
  :words     — trigger words (for :trigger commands, configurable per source)
  :syntax    — command prefix (for :directive commands)
  :param     — parameter type: :email-address, :date, :text (for directives)
  :action    — :set, :unset, :set-deadline, :unset-deadline, :set-topic
  :report-types — set of report types this command applies to (nil = all)"
  [;; --- Triggers (single-word, any user) ---
   {:id        :acked
    :kind      :trigger
    :action    :set
    :attr      :report/acked
    :scope     :user
    :words     :acked
    :report-types #{:bug :patch :request}}
   {:id        :owned
    :kind      :trigger
    :action    :set
    :attr      :report/owned
    :scope     :user
    :words     :owned
    :report-types #{:bug :patch :request}}
   {:id        :closed
    :kind      :trigger
    :action    :set
    :attr      :report/closed
    :scope     :user
    :words     :closed}
   {:id        :urgent
    :kind      :trigger
    :action    :set
    :attr      :report/urgent
    :scope     :user
    :words     :urgent
    :report-types #{:bug :patch :request}}
   {:id        :important
    :kind      :trigger
    :action    :set
    :attr      :report/important
    :scope     :user
    :words     :important
    :report-types #{:bug :patch :request}}
   ;; --- Directives: proxy set (maintainer-only, -by: email-address) ---
   {:id        :acked-by
    :kind      :directive
    :action    :set
    :attr      :report/acked
    :scope     :maintainer
    :syntax    "Acked-by"
    :param     :email-address}
   {:id        :owned-by
    :kind      :directive
    :action    :set
    :attr      :report/owned
    :scope     :maintainer
    :syntax    "Owned-by"
    :param     :email-address}
   {:id        :closed-by
    :kind      :directive
    :action    :set
    :attr      :report/closed
    :scope     :maintainer
    :syntax    "Closed-by"
    :param     :email-address}
   {:id        :urgent-by
    :kind      :directive
    :action    :set
    :attr      :report/urgent
    :scope     :maintainer
    :syntax    "Urgent-by"
    :param     :email-address}
   {:id        :important-by
    :kind      :directive
    :action    :set
    :attr      :report/important
    :scope     :maintainer
    :syntax    "Important-by"
    :param     :email-address}
   ;; --- Directives: unset (maintainer-only, single-word) ---
   {:id        :unacked
    :kind      :directive
    :action    :unset
    :attr      :report/acked
    :scope     :maintainer
    :syntax    "Unacked"}
   {:id        :unowned
    :kind      :directive
    :action    :unset
    :attr      :report/owned
    :scope     :maintainer
    :syntax    "Unowned"}
   {:id        :unclosed
    :kind      :directive
    :action    :unset
    :attr      :report/closed
    :scope     :maintainer
    :syntax    "Unclosed"}
   {:id        :unurgent
    :kind      :directive
    :action    :unset
    :attr      :report/urgent
    :scope     :maintainer
    :syntax    "Unurgent"}
   {:id        :unimportant
    :kind      :directive
    :action    :unset
    :attr      :report/important
    :scope     :maintainer
    :syntax    "Unimportant"}
   ;; --- Directives: deadline ---
   {:id        :deadline
    :kind      :directive
    :action    :set-deadline
    :attr      :report/deadline
    :scope     :maintainer
    :syntax    "Deadline"
    :param     :date}
   {:id        :undeadline
    :kind      :directive
    :action    :unset-deadline
    :attr      :report/deadline
    :scope     :maintainer
    :syntax    "Undeadline"}
   ;; --- Directives: topic ---
   {:id        :topic
    :kind      :directive
    :action    :set-topic
    :attr      :report/topic
    :scope     :maintainer
    :syntax    "Topic"
    :param     :text}])

;; ---------------------------------------------------------------------------
;; Derived indexes from the registry
;; ---------------------------------------------------------------------------

(def trigger-commands  (filterv #(= :trigger  (:kind %)) commands))
(def directive-commands (filterv #(= :directive (:kind %)) commands))

(def state-attrs
  "Report attributes managed by commands."
  [:report/acked :report/owned :report/closed :report/urgent :report/important])

(def proxy-attrs
  {:report/acked     :report/acked-proxy
   :report/owned     :report/owned-proxy
   :report/closed    :report/closed-proxy
   :report/urgent    :report/urgent-proxy
   :report/important :report/important-proxy})

;; ---------------------------------------------------------------------------
;; Pattern compilation
;; ---------------------------------------------------------------------------

(defn- trigger-pattern
  "Build a regex matching any of `words` at start of line, followed by
  trailing punctuation or end-of-line."
  [& words]
  (re-pattern
   (str "(?m)^(" (str/join "|" (map #(java.util.regex.Pattern/quote %) words))
        ")(?:" trailing-punct "|$)")))

(defn- directive-pattern
  "Build a regex for a directive command.
  - No param: matches 'Syntax' + optional trailing punct at EOL.
  - :email-address: matches 'Syntax: addr@host' + optional trailing punct.
  - :date: matches 'Syntax: yyyy-MM-dd' + optional trailing punct.
  - :text: matches 'Syntax: <text>' + optional trailing punct."
  [{:keys [syntax param]}]
  (let [qs (java.util.regex.Pattern/quote syntax)]
    (re-pattern
     (case param
       :email-address (str "^" qs ":\\s+(\\S+@\\S+)" trailing-punct "?\\s*$")
       :date          (str "^" qs ":\\s+(\\d{4}-\\d{2}-\\d{2})" trailing-punct "?\\s*$")
       :text          (str "^" qs ":\\s+(.+?)\\s*" trailing-punct "?\\s*$")
       ;; No param (Un- directives)
       (str "^" qs trailing-punct "?\\s*$")))))

;; ---------------------------------------------------------------------------
;; Trigger compilation (configurable per source via :commands config key)
;; ---------------------------------------------------------------------------

(defn- compile-trigger-words
  "Compile a map of action->word-lists into action->regex-patterns."
  [action-map]
  (update-vals action-map #(apply trigger-pattern %)))

(def default-compiled-commands (compile-trigger-words default-commands))

(defn build-source-commands
  "Merge trigger words for a source: defaults -> global -> per-source.
  Returns {:compiled action->pattern, :words action->word-list}."
  [source-cfg]
  (let [merged (resolve-commands-map source-cfg)]
    {:compiled (if (= merged default-commands)
                 default-compiled-commands
                 (compile-trigger-words merged))
     :words    merged}))

;; ---------------------------------------------------------------------------
;; Compiled directive patterns (built once from the registry)
;; ---------------------------------------------------------------------------

(def ^:private compiled-directives
  "Vec of [cmd-spec compiled-pattern] for all directive commands."
  (mapv (fn [cmd] [cmd (directive-pattern cmd)]) directive-commands))

;; ---------------------------------------------------------------------------
;; Close-reason detection
;; ---------------------------------------------------------------------------

(defn- detect-close-reason
  "Find which :closed trigger word matched in body-text and return
  the corresponding close-reason keyword from `close-reasons`.
  Returns :resolved for words not in the map, or nil if none matched."
  [closed-words body-text]
  (when (seq closed-words)
    (let [pattern (re-pattern
                   (str "(?m)^(" (str/join "|" (map #(java.util.regex.Pattern/quote %) closed-words))
                        ")(?:" trailing-punct "|$)"))]
      (when-let [[_ matched] (re-find pattern body-text)]
        (get close-reasons matched :resolved)))))

;; ---------------------------------------------------------------------------
;; Date parsing
;; ---------------------------------------------------------------------------

(defn- parse-date-iso
  "Parse an ISO date string (yyyy-MM-dd) into a java.util.Date at midnight UTC."
  [s]
  (try
    (let [fmt (java.text.SimpleDateFormat. "yyyy-MM-dd")]
      (.setTimeZone fmt (java.util.TimeZone/getTimeZone "UTC"))
      (.parse fmt s))
    (catch Exception _ nil)))

;; ---------------------------------------------------------------------------
;; Command detection (pure)
;; ---------------------------------------------------------------------------

(defn- match-triggers
  "Match trigger patterns against body-text. Returns {attr true ...}."
  [triggers body-text]
  (into {} (keep (fn [[k p]] (when (re-find p body-text) [(keyword "report" (name k)) true]))) triggers))

(defn detect-triggers
  "Detect trigger commands in body text.
  `source-commands` is {:compiled action->pattern, :words action->word-list}.
  Returns a map of {attr true ...} or nil.
  Filters by report-type restrictions from the command registry."
  [report-type body-text source-commands]
  (when body-text
    (let [compiled (:compiled source-commands)
          all-sets (match-triggers compiled body-text)
          ;; Filter out triggers not applicable to this report type
          filtered (into {}
                         (keep (fn [[attr :as entry]]
                                 (let [cmd (first (filter #(= attr (:attr %)) trigger-commands))
                                       rt  (:report-types cmd)]
                                   (when (or (nil? rt) (contains? rt report-type))
                                     entry))))
                         all-sets)
          reason   (when (:report/closed filtered)
                     (detect-close-reason (get-in source-commands [:words :closed]) body-text))
          result   (cond-> filtered
                     reason (assoc :report/close-reason reason))]
      (when (seq result) result))))

(defn detect-directives
  "Parse directive commands from body text.
  Returns a seq of actions in order, each {:action ... :attr ... :email-address/:date/:topic ...}.
  Last-one-wins is handled by resolve-commands."
  [body-text]
  (when body-text
    (let [lines (str/split-lines body-text)]
      (->> lines
           (keep (fn [line]
                   (some (fn [[{:keys [action attr param]} pattern]]
                           (when-let [m (re-matches pattern line)]
                             (case action
                               :set            {:action :set   :attr attr :email-address (nth m 1)}
                               :unset          {:action :unset :attr attr}
                               :set-deadline   (when-let [d (parse-date-iso (nth m 1))]
                                                 {:action :set-deadline :date d})
                               :unset-deadline {:action :unset-deadline}
                               :set-topic      {:action :set-topic :topic (str/trim (nth m 1))})))
                         compiled-directives)))
           vec))))

(defn resolve-commands
  "Given a seq of directive actions, apply last-one-wins per attribute.
  Returns {:set {attr email-address ...} :unset #{attr ...} :deadline date-or-nil
           :undeadline? bool :topic str-or-nil}."
  [directives]
  (reduce (fn [acc {:keys [action attr email-address date topic]}]
            (case action
              :set   (-> acc
                         (assoc-in [:set attr] email-address)
                         (update :unset disj attr))
              :unset (-> acc
                         (update :set dissoc attr)
                         (update :unset conj attr))
              :set-deadline   (-> acc
                                  (assoc :deadline date)
                                  (dissoc :undeadline?))
              :unset-deadline (-> acc
                                  (dissoc :deadline)
                                  (assoc :undeadline? true))
              :set-topic      (assoc acc :topic topic)))
          {:set {} :unset #{}}
          directives))

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
;; Command application (effectful)
;; ---------------------------------------------------------------------------

(defn- ref-eid [v] (if (map? v) (:db/id v) v))

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

(defn- apply-vote! [conn report-eid from-addr body-text]
  ;; SAFETY INVARIANT: cmd-digest! processes emails sequentially.
  ;; This read-then-write is safe only under single-threaded digest.
  ;; If parallelized, replace with a Datalevin transaction function.
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
          (bump-report-updated! conn report-eid)
          (log/info "Vote" (case vote :up "+1" :down "-1" "0") "by" from-addr))))))

(defn apply-commands!
  "Detect all commands and votes from an email's body text,
  then apply them: triggers first, then directives.
  `roles` is the roles map for the report's source."
  [conn report-eid report-type email source-map roles]
  (let [body-text (email-body-text email)
        from-addr (:email/from-address email)
        eid       (:db/id email)]
    (when body-text
      ;; --- 1. Detect everything (pure) ---
      (let [src-name    (d/q '[:find ?src . :in $ ?rid :where
                               [?rid :report/email ?e] [?e :email/source ?src]]
                             (d/db conn) report-eid)
            src-cmds    (build-source-commands (get source-map src-name))
            trig-result (detect-triggers report-type body-text src-cmds)
            directives  (detect-directives body-text)
            is-maintainer? (maintainer? roles from-addr (:email/date-sent email))]

        ;; --- 2. Apply votes (requests only) ---
        (when (and (= :request report-type) from-addr)
          (apply-vote! conn report-eid from-addr body-text))

        ;; --- 3. Apply triggers ---
        (when trig-result
          (let [close-reason (:report/close-reason trig-result)
                ref-result   (dissoc trig-result :report/close-reason)
                current      (d/pull (d/db conn) state-attrs report-eid)
                new-sets     (into {} (remove (fn [[k _]] (get current k))) ref-result)
                set-tx       (when (seq new-sets)
                               [(into {:db/id report-eid} (map (fn [[k _]] [k eid])) new-sets)])
                reason-tx    (when (and close-reason (:report/closed new-sets))
                               [[:db/add report-eid :report/close-reason close-reason]])
                all-tx       (vec (concat set-tx reason-tx))]
            (when (seq all-tx)
              (d/transact! conn all-tx)
              (bump-report-updated! conn report-eid)
              (log/info (str/join ", " (cond-> (mapv (comp name key) new-sets)
                                         close-reason (conj (str "close-reason:" (name close-reason)))))
                        "(by" (:email/message-id email) ")"))))

        ;; --- 4. Apply directives (maintainer-only) ---
        (when (and (seq directives) is-maintainer?)
          (let [{:keys [set unset deadline undeadline? topic]} (resolve-commands directives)
                report-mid (d/q '[:find ?mid . :in $ ?r
                                  :where [?r :report/message-id ?mid]]
                                (d/db conn) report-eid)
                proxy-eid  eid
                current    (d/pull (d/db conn) (conj state-attrs :report/deadline :report/close-reason) report-eid)
                ;; Build set transactions
                set-tx (mapcat (fn [[attr addr]]
                                 (let [target-eid (find-or-create-synthetic-email!
                                                   conn addr report-mid attr)]
                                   [[:db/add report-eid attr target-eid]
                                    [:db/add report-eid (proxy-attrs attr) proxy-eid]]))
                               set)
                ;; Close-reason for Closed-by directive (always :resolved)
                close-reason-tx (when (contains? set :report/closed)
                                  [[:db/add report-eid :report/close-reason :resolved]])
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
                ;; Clear close-reason when Unclosed
                unclose-reason-tx (when (and (contains? unset :report/closed)
                                             (:report/close-reason current))
                                    [[:db/retract report-eid :report/close-reason
                                      (:report/close-reason current)]])
                ;; Deadline transactions
                deadline-tx (cond
                              deadline    [[:db/add report-eid :report/deadline deadline]]
                              undeadline? (when (:report/deadline current)
                                            [[:db/retract report-eid :report/deadline
                                              (:report/deadline current)]])
                              :else       nil)
                ;; Topic transaction (overwrites previous topic)
                topic-tx (when topic [[:db/add report-eid :report/topic topic]])
                all-tx (vec (concat set-tx close-reason-tx unset-tx unclose-reason-tx
                                    deadline-tx topic-tx))
                ;; Log description
                desc (concat (map (fn [[attr addr]] (str (name attr) " -> " addr)) set)
                             (map #(str "un-" (name %)) unset)
                             (when deadline [(str "deadline " deadline)])
                             (when undeadline? ["undeadline"])
                             (when topic [(str "topic: " topic)]))]
            (when (seq all-tx)
              (d/transact! conn all-tx)
              (bump-report-updated! conn report-eid)
              (log/info "Commands:" (str/join ", " desc)
                        "(proxy by" from-addr ")"))))))))
