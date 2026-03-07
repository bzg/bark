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

(defn- match-unset-triggers [triggers body-text]
  (into #{} (keep (fn [[k p]] (when (re-find p body-text) (keyword "report" (name k))))) triggers))

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
  "Compile a map of action→word-lists into action→regex-patterns."
  [action-map]
  (into {} (map (fn [[action words]] [action (apply trigger-pattern words)])) action-map))

(defn- compile-triggers-by-type
  "Compile a full type→action→words map into type→action→pattern."
  [tw]
  (into {} (map (fn [[rtype actions]] [rtype (compile-trigger-words actions)])) tw))

(def default-triggers-by-type (compile-triggers-by-type default-trigger-words))

(defn build-source-triggers
  "Merge triggers for a source: defaults → global → per-source.
  Returns compiled type→action→pattern map."
  [source-cfg]
  (let [global  (:global-triggers source-cfg)
        per-src (:triggers source-cfg)
        merged  (cond
                  (and global per-src)
                  (reduce-kv (fn [acc rtype overrides]
                               (assoc acc rtype (merge (get acc rtype) overrides)))
                             (reduce-kv (fn [acc rtype overrides]
                                          (assoc acc rtype (merge (get acc rtype) overrides)))
                                        default-trigger-words global)
                             per-src)
                  global
                  (reduce-kv (fn [acc rtype overrides]
                               (assoc acc rtype (merge (get acc rtype) overrides)))
                             default-trigger-words global)
                  per-src
                  (reduce-kv (fn [acc rtype overrides]
                               (assoc acc rtype (merge (get acc rtype) overrides)))
                             default-trigger-words per-src)
                  :else nil)]
    (if merged
      (compile-triggers-by-type merged)
      default-triggers-by-type)))

;; ---------------------------------------------------------------------------
;; Priority and unset triggers
;; ---------------------------------------------------------------------------

(def report-unset-triggers
  {:urgent (trigger-pattern "Not urgent") :important (trigger-pattern "Not important")})

(def report-priority-triggers
  {:urgent (trigger-pattern "Urgent") :important (trigger-pattern "Important")})

(def report-types-with-priority #{:bug :patch :request})

;; ---------------------------------------------------------------------------
;; Trigger detection (pure)
;; ---------------------------------------------------------------------------

(defn detect-triggers
  "Detect trigger words in body text for a given report type.
  Returns {:set {attr true ...} :unset #{attr ...}} or nil."
  [report-type body-text triggers-by-type]
  (when body-text
    (let [sets     (when-let [t (triggers-by-type report-type)] (match-triggers t body-text))
          priority (when (report-types-with-priority report-type) (match-triggers report-priority-triggers body-text))
          unsets   (when (report-types-with-priority report-type) (match-unset-triggers report-unset-triggers body-text))
          all-sets (into {} (remove (fn [[k _]] (contains? unsets k))) (merge sets priority))]
      (when (or (seq all-sets) (seq unsets))
        {:set all-sets :unset unsets}))))

;; ---------------------------------------------------------------------------
;; Vote detection (pure)
;; ---------------------------------------------------------------------------

(def vote-up-pattern   #"(?m)^\s*(?:\+1|1\+)\s*$")
(def vote-down-pattern #"(?m)^\s*(?:-1|1-)\s*$")

(defn detect-vote
  "Detect a +1/-1 vote in body text. Returns :up, :down, or nil."
  [body-text]
  (when body-text
    (cond
      (re-find vote-up-pattern body-text)   :up
      (re-find vote-down-pattern body-text) :down)))

;; ---------------------------------------------------------------------------
;; Trigger and vote application (effectful)
;; ---------------------------------------------------------------------------

(defn- ref-eid [v] (if (map? v) (:db/id v) v))

(def state-attrs [:report/acked :report/owned :report/closed :report/urgent :report/important])

(defn apply-vote! [conn report-eid from-addr body-text]
  ;; NB: read-then-write is safe because cmd-digest! processes emails
  ;; sequentially. If parallelized, this needs a transaction function.
  (when-let [vote (detect-vote body-text)]
    (let [db      (d/db conn)
          current (d/pull db [:report/voters :report/votes-up :report/votes-down] report-eid)
          voters  (set (:report/voters current))]
      (when-not (contains? voters from-addr)
        (let [attr (if (= vote :up) :report/votes-up :report/votes-down)
              n    (or (get current attr) 0)]
          (d/transact! conn [[:db/add report-eid attr (inc n)]
                             [:db/add report-eid :report/voters from-addr]])
          (println (str "    → vote " (if (= vote :up) "+1" "-1")
                        " by " from-addr)))))))

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
            new-sets (into {} (remove (fn [[k _]] (get current k))) (:set result))
            new-unsets (into #{} (filter current) (:unset result))
            set-tx   (when (seq new-sets)
                       [(into {:db/id report-eid} (map (fn [[k _]] [k eid])) new-sets)])
            unset-tx (when (seq new-unsets)
                       (mapv (fn [attr] [:db/retract report-eid attr (ref-eid (get current attr))]) new-unsets))
            all-tx   (into (vec set-tx) unset-tx)]
        (when (seq all-tx)
          (d/transact! conn all-tx)
          (println (str "    → "
                        (str/join ", " (concat (map (comp name key) new-sets)
                                               (map #(str "un-" (name %)) new-unsets)))
                        " (by " (:email/message-id email) ")")))))))
