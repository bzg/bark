;; bark-triggers.clj — Trigger detection, voting, and application.
;;
;; Pure functions: detect-triggers, detect-vote, detect-directives,
;;                 resolve-directives, build-source-triggers
;; Effectful:      apply-triggers-and-directives!
;;
;; Usage: (load-file "scripts/bark-triggers.clj")

(require '[clojure.string :as str])

;; Defined in bark-common.clj / bark-roles.clj; forward-declared for clj-kondo.
(declare default-triggers close-reasons
         resolve-triggers-map
         email-body-text report-priority maintainer?)

;; ---------------------------------------------------------------------------
;; Trigger pattern compilation
;; ---------------------------------------------------------------------------

(defn- trigger-pattern [& words]
  (re-pattern (str "(?m)^(" (str/join "|" (map #(java.util.regex.Pattern/quote %) words)) ")(?:[.,;:]|$)")))

(defn- match-triggers [triggers body-text]
  (into {} (keep (fn [[k p]] (when (re-find p body-text) [(keyword "report" (name k)) true]))) triggers))

(defn- detect-close-reason
  "Find which :closed trigger word matched in body-text and return
  the corresponding close-reason keyword from `close-reasons`.
  Returns :resolved for words not in the map, or nil if none matched."
  [closed-words body-text]
  (when (seq closed-words)
    (let [pattern (re-pattern
                   (str "(?m)^(" (str/join "|" (map #(java.util.regex.Pattern/quote %) closed-words))
                        ")(?:[.,;:]|$)"))]
      (when-let [[_ matched] (re-find pattern body-text)]
        (get close-reasons matched :resolved)))))

;; ---------------------------------------------------------------------------
;; Trigger defaults and compilation
;; ---------------------------------------------------------------------------

;; default-triggers is defined in bark-common.clj

(defn- compile-trigger-words
  "Compile a map of action->word-lists into action->regex-patterns."
  [action-map]
  (update-vals action-map #(apply trigger-pattern %)))

(def default-compiled-triggers (compile-trigger-words default-triggers))

(defn build-source-triggers
  "Merge triggers for a source: defaults -> global -> per-source.
  Returns {:compiled action->pattern, :words action->word-list}."
  [source-cfg]
  (let [merged (resolve-triggers-map source-cfg)]
    {:compiled (if (= merged default-triggers)
                 default-compiled-triggers
                 (compile-trigger-words merged))
     :words    merged}))

;; ---------------------------------------------------------------------------
;; Priority triggers
;; ---------------------------------------------------------------------------

(def report-priority-triggers
  {:urgent (trigger-pattern "Urgent") :important (trigger-pattern "Important")})

(def report-types-with-priority #{:bug :patch :request})

(def report-types-actionable
  "Report types that support :acked and :owned states.
  Announcements, releases, and changes can only be closed."
  #{:bug :patch :request})

;; ---------------------------------------------------------------------------
;; Trigger detection (pure)
;; ---------------------------------------------------------------------------

(defn detect-triggers
  "Detect trigger words in body text.
  `source-triggers` is {:compiled action->pattern, :words action->word-list}.
  Returns a map of {attr true/value ...} or nil.
  When :report/closed is detected, also includes :report/close-reason.
  Acked/owned are only detected for actionable report types (bug, patch, request)."
  [report-type body-text source-triggers]
  (when body-text
    (let [compiled   (:compiled source-triggers)
          actionable? (report-types-actionable report-type)
          sets       (cond-> (match-triggers compiled body-text)
                       (not actionable?) (dissoc :report/acked :report/owned))
          priority   (when (report-types-with-priority report-type) (match-triggers report-priority-triggers body-text))
          reason     (when (:report/closed sets)
                       (detect-close-reason (get-in source-triggers [:words :closed]) body-text))
          all-sets   (cond-> (merge sets priority)
                       reason (assoc :report/close-reason reason))]
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
  #"^(Acked|Owned|Closed|Urgent|Important)-by:\s+(\S+@\S+)[.,;:]?\s*$")

(def directive-un-pattern
  #"^(Unacked|Unowned|Unclosed|Unurgent|Unimportant)[.,;:]?\s*$")

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
  "Matches 'Deadline: 2026-03-10' (with optional trailing punctuation)."
  #"^Deadline:\s+(\d{4}-\d{2}-\d{2})[.,;:]?\s*$")

(def ^:private undeadline-pattern
  #"^Undeadline[.,;:]?\s*$")

;; ---------------------------------------------------------------------------
;; Topic directive — maintainer-set topic (last-one-wins, overwrites)
;; ---------------------------------------------------------------------------

(def ^:private topic-directive-pattern
  "Matches 'Topic: <topic>' at the beginning of a line (trailing punctuation optional)."
  #"^Topic:\s+(.+?)\s*[.,;:]?\s*$")

(defn- parse-date-iso
  "Parse an ISO date string (yyyy-MM-dd) into a java.util.Date at midnight UTC."
  [s]
  (try
    (let [fmt (java.text.SimpleDateFormat. "yyyy-MM-dd")]
      (.setTimeZone fmt (java.util.TimeZone/getTimeZone "UTC"))
      (.parse fmt s))
    (catch Exception _ nil)))

(defn detect-directives
  "Parse maintainer directives from body text.
  Returns a seq of actions in order, each {:action :set/:unset :attr ... :addr/:date/:topic ...}.
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
                         (when-let [d (parse-date-iso date-str)]
                           {:action :set-deadline :date d}))
                       (when (re-matches undeadline-pattern line)
                         {:action :unset-deadline})
                       (when-let [[_ topic] (re-matches topic-directive-pattern line)]
                         {:action :set-topic :topic (str/trim topic)}))))
           vec))))

(defn resolve-directives
  "Given a seq of directive actions, apply last-one-wins per attribute.
  Returns {:set {attr addr ...} :unset #{attr ...} :deadline date-or-nil
           :undeadline? bool :topic str-or-nil}."
  [directives]
  (reduce (fn [acc {:keys [action attr addr date topic]}]
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
                                  (assoc :undeadline? true))
              :set-topic      (assoc acc :topic topic)))
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
          (log/info "Vote" (case vote :up "+1" :down "-1" "0") "by" from-addr))))))

(defn apply-triggers-and-directives!
  "Detect all triggers, directives, and votes from an email's body text,
  then apply them in a single pass: triggers first, then directives.
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
            triggers    (build-source-triggers (get source-map src-name))
            trig-result (detect-triggers report-type body-text triggers)
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
              (log/info (str/join ", " (cond-> (mapv (comp name key) new-sets)
                                         close-reason (conj (str "close-reason:" (name close-reason)))))
                        "(by" (:email/message-id email) ")"))))

        ;; --- 4. Apply directives (maintainer-only) ---
        (when (and (seq directives) is-maintainer?)
          (let [{:keys [set unset deadline undeadline? topic]} (resolve-directives directives)
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
              (log/info "Directives:" (str/join ", " desc)
                        "(proxy by" from-addr ")"))))))))

