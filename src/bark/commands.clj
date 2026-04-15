;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.commands
  "Unified command detection, resolution, and application."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [datalevin.core :as d]
            [taoensso.timbre :as log]
            [bark.common :as common]
            [bark.tracking :as tracking])
  (:import [java.time LocalDate ZoneOffset]
           [java.util Date]))

;; ---------------------------------------------------------------------------
;; Trailing punctuation
;; ---------------------------------------------------------------------------

(def trailing-punct "[.,;:?!]")

;; ---------------------------------------------------------------------------
;; Command registry
;;
;; :scope values:
;;   :user                 — anyone
;;   :maintainer           — any maintainer
;;   :setter-or-maintainer — the address that previously set the attribute,
;;                           or any maintainer (maintainers keep their
;;                           administrative override)
;;
;; :setter-or-maintainer is only meaningful for unset-style directives
;; whose target attribute is tracked by a ref to the pose-email (see
;; `setter-ref-attrs` below).  These are the five original state
;; attrs (acked/owned/closed/urgent/important) plus topic, deadline,
;; expiry and superseded-by — a total of nine `:un*` commands.
;; `validate-config.clj` rejects that scope on any other command.
;; ---------------------------------------------------------------------------

(def commands
  [;; Triggers
   {:id :acked    :kind :trigger  :action :set   :attr :report/acked    :scope :user
    :words :acked  :report-types #{:bug :patch :request}}
   {:id :owned    :kind :trigger  :action :set   :attr :report/owned    :scope :user
    :words :owned  :report-types #{:bug :patch :request}}
   {:id :closed   :kind :trigger  :action :set   :attr :report/closed   :scope :user  :words :closed}
   {:id :urgent   :kind :trigger  :action :set   :attr :report/urgent   :scope :user  :words :urgent}
   {:id :important :kind :trigger :action :set   :attr :report/important :scope :user :words :important}
   ;; -by directives (maintainer sets attribute on behalf of someone else)
   {:id :acked-by     :kind :directive :action :set   :attr :report/acked    :scope :maintainer
    :syntax "Acked-by" :param :email-address :report-types #{:bug :patch :request}}
   {:id :owned-by     :kind :directive :action :set   :attr :report/owned    :scope :maintainer
    :syntax "Owned-by" :param :email-address :report-types #{:bug :patch :request}}
   {:id :closed-by    :kind :directive :action :set   :attr :report/closed   :scope :maintainer
    :syntax "Closed-by" :param :email-address}
   {:id :urgent-by    :kind :directive :action :set   :attr :report/urgent   :scope :maintainer
    :syntax "Urgent-by" :param :email-address}
   {:id :important-by :kind :directive :action :set   :attr :report/important :scope :maintainer
    :syntax "Important-by" :param :email-address}
   ;; Unset directives — :setter-or-maintainer lets the user who previously
   ;; set the attribute retract it (and maintainers retain full override).
   {:id :unacked     :kind :directive :action :unset :attr :report/acked    :scope :setter-or-maintainer
    :syntax "Not acked" :report-types #{:bug :patch :request}}
   {:id :unowned     :kind :directive :action :unset :attr :report/owned    :scope :setter-or-maintainer
    :syntax "Not owned" :report-types #{:bug :patch :request}}
   {:id :unclosed    :kind :directive :action :unset :attr :report/closed   :scope :setter-or-maintainer :syntax "Not closed"}
   {:id :unurgent    :kind :directive :action :unset :attr :report/urgent   :scope :setter-or-maintainer :syntax "Not urgent"}
   {:id :unimportant :kind :directive :action :unset :attr :report/important :scope :setter-or-maintainer :syntax "Not important"}
   ;; Deadline / topic
   {:id :deadline    :kind :directive :action :set-deadline   :attr :report/deadline :scope :maintainer
    :syntax "Deadline" :param :date-or-duration :report-types #{:bug :patch :request}}
   {:id :undeadline  :kind :directive :action :unset-deadline :attr :report/deadline :scope :maintainer
    :syntax "No deadline" :report-types #{:bug :patch :request}}
   {:id :expiry      :kind :directive :action :set-expiry   :attr :report/expiry :scope :maintainer
    :syntax "Expiry" :param :date-or-duration :report-types #{:bug :patch :request}}
   {:id :unexpiry    :kind :directive :action :unset-expiry :attr :report/expiry :scope :maintainer
    :syntax "No expiry" :report-types #{:bug :patch :request}}
   {:id :topic       :kind :directive :action :set-topic :attr :report/topic :scope :user
    :syntax "Topic" :param :word}
   {:id :untopic     :kind :directive :action :unset-topic :attr :report/topic :scope :user
    :syntax "No topic"}
   ;; Supersede
   {:id :superseded-by  :kind :directive :action :set-superseded :attr :report/superseded-by :scope :user
    :syntax "Superseded-by" :param :message-id}
   {:id :unsuperseded   :kind :directive :action :unset-superseded :attr :report/superseded-by :scope :user
    :syntax "Not superseded"}])

;; Derived indexes
(def trigger-commands  (filterv #(= :trigger  (:kind %)) commands))
(def directive-commands (filterv #(= :directive (:kind %)) commands))

(def commands-by-id (into {} (map (juxt :id identity)) commands))

(def attr->trigger-cmd
  (into {} (map (juxt :attr identity)) trigger-commands))

;; The five state attrs that support the `-by` proxy form (e.g.
;; `Acked-by: bob@example.com`, where the sender credits Bob instead
;; of themselves).  Paired with `-address` caches below so the
;; designated address survives the proxy indirection.
(def proxy-state-attrs
  [:report/acked :report/owned :report/closed :report/urgent :report/important])

;; Proxy-state attr → paired `-address` cache.  The cache holds the
;; credited address, which may differ from the pose-email's
;; from-address when the `-by` form is used.
(def address-attrs
  {:report/acked     :report/acked-address
   :report/owned     :report/owned-address
   :report/closed    :report/closed-address
   :report/urgent    :report/urgent-address
   :report/important :report/important-address})

;; All report attributes that Bark tracks as refs to the pose-email.
;; Shape: `{ref-attr paired-value-attr-or-nil}`.
;; The paired attr holds the business datum posed alongside the
;; setter identity — a scalar for topic/deadline/expiry (`-value`),
;; a structural ref for superseded-by (`-target`).  The five
;; proxy-state attrs (acked/owned/closed/urgent/important) carry no
;; paired value — their "value" is just the fact that the state was
;; set, and the proxy-designated address lives in `address-attrs`.
;; Consumed by `set-ref-value-tx`/`retract-ref-value-tx` helpers, by
;; the drift test in `bark.common-test`, and by the config validator
;; via `bark.common/setter-scoped-command-ids`.
(def setter-ref-attrs
  {:report/acked         nil
   :report/owned         nil
   :report/closed        nil
   :report/urgent        nil
   :report/important     nil
   :report/topic         :report/topic-value
   :report/deadline      :report/deadline-value
   :report/expiry        :report/expiry-value
   :report/superseded-by :report/superseded-by-target})

;; ---------------------------------------------------------------------------
;; Pattern compilation
;; ---------------------------------------------------------------------------

(def ^:private strict-punct-actions
  "Trigger actions that must NOT accept bare whitespace as a separator
  (too many false positives with ordinary prose, e.g. \"Important note:\")."
  #{:urgent :important})

(defn- trigger-pattern [strict? & words]
  (re-pattern
   (str "(?m)^(" (str/join "|" (map #(java.util.regex.Pattern/quote %) words))
        ")(?:" trailing-punct (when-not strict? "|\\s") "|$)")))

(defn- directive-pattern [{:keys [syntax param]}]
  (let [qs (java.util.regex.Pattern/quote syntax)]
    (re-pattern
     (case param
       :email-address (str "^" qs ":\\s+(?:.+<(\\S+@\\S+)>|(\\S+@\\S+))" trailing-punct "?\\s*$")
       :date          (str "^" qs ":\\s+(\\d{4}-\\d{2}-\\d{2})" trailing-punct "?\\s*$")
       :date-or-duration (str "^" qs ":\\s+(\\d{4}-\\d{2}-\\d{2}|\\d+[dwmy](?:\\s+\\d+[dwmy])*)" trailing-punct "?\\s*$")
       :word          (str "^" qs ":\\s+([a-zA-Z0-9_-]+)" trailing-punct "?\\s*$")
       :message-id    (str "^" qs ":\\s+<?([^<>\\s]+@[^<>\\s]+)>?" trailing-punct "?\\s*$")
       (str "^" qs trailing-punct "?\\s*$")))))

(defn- compile-trigger-words [action-map]
  (into {}
        (map (fn [[k words]]
               [k (apply trigger-pattern (contains? strict-punct-actions k) words)]))
        action-map))

(def default-compiled-commands (compile-trigger-words common/default-commands))

(defn build-source-commands [source-cfg]
  (let [merged (common/resolve-commands-map source-cfg)]
    {:compiled  (if (= merged common/default-commands)
                  default-compiled-commands
                  (compile-trigger-words merged))
     :words     merged
     :overrides (common/resolve-command-overrides source-cfg)}))

(def ^:private compiled-directives
  (mapv (fn [cmd] [cmd (directive-pattern cmd)]) directive-commands))

(defn compile-directive-aliases
  "Compile a map of {\"OldSyntax\" \"New syntax\"} into additional [cmd pattern]
  pairs that route alias patterns to the same commands as the canonical syntax."
  [aliases-map]
  (when (seq aliases-map)
    (let [syntax->cmd (into {} (map (fn [[cmd _]] [(:syntax cmd) cmd])) compiled-directives)]
      (vec (keep (fn [[old-syntax new-syntax]]
                   (if-let [cmd (syntax->cmd new-syntax)]
                     [cmd (directive-pattern (assoc cmd :syntax old-syntax))]
                     (log/warn "Command alias target not found:" (pr-str new-syntax)
                               "for alias" (pr-str old-syntax))))
                 aliases-map)))))

;; ---------------------------------------------------------------------------
;; Detection (pure)
;; ---------------------------------------------------------------------------

(defn- detect-close-reason [closed-words body-text]
  (when (seq closed-words)
    (let [pattern (re-pattern
                   (str "(?m)^(" (str/join "|" (map #(java.util.regex.Pattern/quote %) closed-words))
                        ")(?:" trailing-punct "|\\s|$)"))]
      (when-let [[_ matched] (re-find pattern body-text)]
        (get common/close-reasons matched :resolved)))))

(defn- parse-date-iso [s]
  (try
    (-> (LocalDate/parse s) (.atStartOfDay ZoneOffset/UTC) .toInstant Date/from)
    (catch Exception _
      (log/warn "Invalid ISO date in directive:" s)
      nil)))

(defn- parse-date-or-duration
  "Parse a YYYY-MM-DD date string or a duration like '2d', '3w', '1m 2w'.
  Durations are resolved to an absolute date relative to `as-of` (a
  java.util.Date, typically the email's date-sent).  Falls back to today
  when `as-of` is nil."
  [s as-of]
  (if (re-matches #"\d{4}-\d{2}-\d{2}" s)
    (parse-date-iso s)
    (if-let [days (common/parse-delay s)]
      (let [base (if as-of
                   (-> as-of ^Date .toInstant (LocalDate/ofInstant ZoneOffset/UTC))
                   (LocalDate/now ZoneOffset/UTC))]
        (-> base (.plusDays days) (.atStartOfDay ZoneOffset/UTC) .toInstant Date/from))
      (do (log/warn "Unparseable date/duration in directive:" s)
          nil))))

(defn- match-triggers [triggers body-text]
  (into {} (keep (fn [[k p]] (when (re-find p body-text) [(keyword "report" (name k)) true]))) triggers))

(defn detect-triggers [report-type body-text source-commands]
  (when body-text
    (let [compiled  (:compiled source-commands)
          overrides (:overrides source-commands)
          all-sets  (match-triggers compiled body-text)
          ;; Pre-compute close-reason from unfiltered triggers so it survives
          ;; any future refactoring of the filter step.
          reason   (when (:report/closed all-sets)
                     (detect-close-reason (get-in source-commands [:words :closed]) body-text))
          filtered (into {}
                         (keep (fn [[attr :as entry]]
                                 (let [cmd (attr->trigger-cmd attr)
                                       rt  (or (:report-types (get overrides (:id cmd)))
                                               (:report-types cmd))]
                                   (when (or (nil? rt) (contains? rt report-type)) entry))))
                         all-sets)
          ;; Only attach close-reason when :report/closed survived filtering.
          result   (cond-> filtered
                     (and reason (:report/closed filtered))
                     (assoc :report/close-reason reason))]
      (when (seq result) result))))

(defn detect-directives
  ([report-type body-text] (detect-directives report-type body-text nil nil nil))
  ([report-type body-text overrides] (detect-directives report-type body-text overrides nil nil))
  ([report-type body-text overrides email-date] (detect-directives report-type body-text overrides email-date nil))
  ([report-type body-text overrides email-date aliases]
   (when body-text
     (let [lines (str/split-lines body-text)
           all-directives (if (seq aliases)
                            (into compiled-directives aliases)
                            compiled-directives)]
       (->> lines
            (keep (fn [line]
                    (some (fn [[{:keys [id action attr _param scope report-types]} pattern]]
                            (let [rt (or (:report-types (get overrides id)) report-types)
                                  sc (or (:scope (get overrides id)) scope)]
                              (when (or (nil? rt) (contains? rt report-type))
                                (when-let [m (re-matches pattern line)]
                                  (let [base (case action
                                               :set            (when-let [addr (or (nth m 1 nil) (nth m 2 nil))]
                                                                 {:action :set :attr attr :email-address addr})
                                               :unset          {:action :unset :attr attr}
                                               :set-deadline   (when-let [d (parse-date-or-duration (nth m 1) email-date)]
                                                                 {:action :set-deadline :date d})
                                               :unset-deadline {:action :unset-deadline}
                                               :set-expiry     (when-let [d (parse-date-or-duration (nth m 1) email-date)]
                                                                 {:action :set-expiry :date d})
                                               :unset-expiry   {:action :unset-expiry}
                                               :unset-topic    {:action :unset-topic}
                                               :set-topic      (when-let [t (nth m 1 nil)]
                                                                 {:action :set-topic :topic t})
                                               :set-superseded   (when-let [mid (nth m 1 nil)]
                                                                   {:action :set-superseded
                                                                    :target-message-id (str "<" mid ">")})
                                               :unset-superseded {:action :unset-superseded})]
                                    (when base (assoc base :scope sc :id id)))))))
                          all-directives)))
            vec)))))

;; ---------------------------------------------------------------------------
;; Command failure recording (file-based)
;; ---------------------------------------------------------------------------

(def ^:dynamic *failures-file*
  "Path to the failures EDN file. Bound to `public/.failures.edn` in
  production; tests rebind it to a temp path so they don't pollute the
  real file."
  "public/.failures.edn")

(def ^:private max-failure-age-ms (* 365 24 60 60 1000))

(defn- load-failures []
  (let [f (io/file *failures-file*)]
    (if (.exists f)
      (try (edn/read-string (slurp f)) (catch Exception _ []))
      [])))

(defn- save-failures! [failures]
  (io/make-parents (io/file *failures-file*))
  (spit *failures-file* (pr-str failures)))

(defn record-failure!
  "Append a command failure to the failures file for later notification.
  Prunes entries older than 1 year.

  `:audience` controls who the notifier will route the entry to:
  - `:author`      — the address that sent the command (the default,
                     used for typo-class failures like `Superseded-by:`
                     with an unknown target).
  - `:maintainers` — all maintainer subscribers on the source, so a
                     permission denial is visible to the people who can
                     act on it."
  [{:keys [source from-addr email-date reason command report-mid audience]}]
  (let [now-ms   (System/currentTimeMillis)
        cutoff   (Date. (- now-ms max-failure-age-ms))
        existing (load-failures)
        pruned   (filterv (fn [{:keys [date]}]
                            (and date (.after ^Date date cutoff)))
                          existing)
        entry    {:source     source
                  :from       (str/lower-case from-addr)
                  :date       (or email-date (Date.))
                  :reason     reason
                  :command    command
                  :report-mid (or report-mid "")
                  :audience   (or audience :author)}]
    (save-failures! (conj pruned entry))
    (log/info "Command failure:" reason command "from" from-addr
              (str "(audience: " (name (:audience entry)) ")"))))

(defn resolve-commands
  "Fold a seq of parsed directives into a summary map.
  NOT for trigger results (which map attrs to `true`, not addresses)."
  [directives]
  (reduce (fn [acc {:keys [action attr email-address date topic target-message-id]}]
            (case action
              :set   (-> acc (assoc-in [:set attr] email-address) (update :unset disj attr))
              :unset (-> acc (update :set dissoc attr) (update :unset conj attr))
              :set-deadline   (-> acc (assoc :deadline date) (dissoc :undeadline?))
              :unset-deadline (-> acc (dissoc :deadline) (assoc :undeadline? true))
              :set-expiry     (-> acc (assoc :expiry date) (dissoc :unexpiry?))
              :unset-expiry   (-> acc (dissoc :expiry) (assoc :unexpiry? true))
              :set-topic      (assoc acc :topic topic)
              :unset-topic    (-> acc (dissoc :topic) (assoc :untopic? true))
              :set-superseded   (-> acc (assoc :superseded-by target-message-id) (dissoc :unsuperseded?))
              :unset-superseded (-> acc (dissoc :superseded-by) (assoc :unsuperseded? true))))
          {:set {} :unset #{}}
          directives))

;; ---------------------------------------------------------------------------
;; Vote detection (pure)
;; ---------------------------------------------------------------------------

(def vote-up-pattern   #"(?m)(?:^|\s)(?:\+1|1\+)(?![a-zA-Z0-9])")
(def vote-down-pattern #"(?m)(?:^|\s)(?:-1|1-)(?![a-zA-Z0-9])")
(def vote-null-pattern #"(?m)(?:^|\s)(?:\+0|0\+|-0|0-)(?![a-zA-Z0-9])")

(defn detect-vote [body-text]
  (when body-text
    (cond
      (re-find vote-up-pattern body-text)   :up
      (re-find vote-down-pattern body-text) :down
      (re-find vote-null-pattern body-text) :null)))

;; ---------------------------------------------------------------------------
;; Command application (effectful)
;; ---------------------------------------------------------------------------

(defn- ref-eid [v] (if (map? v) (:db/id v) v))

(defn- apply-vote! [conn report-eid from-addr body-text email delivery source-cfg]
  (when-let [vote (detect-vote body-text)]
    (if-not (common/sent-via-source-channel? delivery source-cfg)
      (log/info "Vote ignored (private email on public source)" from-addr)
      (let [report-mid (:report/message-id (d/entity (d/db conn) report-eid))
            vote-key   (str report-mid ":" from-addr)]
        ;; :vote/key has :db.unique/identity — if this voter already voted,
        ;; the upsert overwrites silently (first vote wins in practice,
        ;; since we only transact when key is new).
        (when-not (d/entid (d/db conn) [:vote/key vote-key])
          (d/transact! conn [{:vote/key    vote-key
                              :vote/report report-eid
                              :vote/email  (:db/id email)
                              :vote/value  vote
                              :vote/voter  from-addr}])
          (tracking/bump-report-updated! conn report-eid)
          (log/info "Vote" (case vote :up "+1" :down "-1" "0") "by" from-addr))))))

(defn- build-unset-tx
  "Build retraction datoms for unsetting attributes and their address attrs."
  [report-eid current attrs]
  (into []
        (mapcat (fn [attr]
                  (let [cur      (get current attr)
                        addr-cur (get current (address-attrs attr))]
                    (cond-> []
                      cur      (conj [:db/retract report-eid attr (ref-eid cur)])
                      addr-cur (conj [:db/retract report-eid (address-attrs attr) addr-cur])))))
        attrs))

(defn- build-directive-set-tx
  "Build assertion datoms for setting attributes via -by directives.
  Points the attr to the real email and stores the designated address."
  [report-eid email-eid set-map]
  (into []
        (mapcat (fn [[attr addr]]
                  [[:db/add report-eid attr email-eid]
                   [:db/add report-eid (address-attrs attr) addr]]))
        set-map))

(defn build-trigger-tx
  "Build transaction data for trigger results.
  `current` is the report's current state (pulled with
  `proxy-state-attrs`).
  Returns [tx-data new-sets] or nil if nothing to do."
  [report-eid trig-result email-eid from-addr current]
  (let [close-reason (:report/close-reason trig-result)
        ref-result   (dissoc trig-result :report/close-reason)
        new-sets     (into {} (remove (fn [[k _]] (get current k))) ref-result)
        all-tx       (cond-> (when (seq new-sets)
                               (into [(into {:db/id report-eid} (map (fn [[k _]] [k email-eid])) new-sets)]
                                     (map (fn [[k _]] [:db/add report-eid (address-attrs k) from-addr]))
                                     new-sets))
                       (and close-reason (:report/closed new-sets))
                       (conj [:db/add report-eid :report/close-reason close-reason]))]
    (when (seq all-tx) [(vec all-tx) new-sets close-reason])))

(defn apply-triggers! [conn report-eid trig-result email-eid email-mid from-addr]
  (when trig-result
    (let [current (d/pull (d/db conn) proxy-state-attrs report-eid)]
      (when-let [[all-tx new-sets close-reason]
                 (build-trigger-tx report-eid trig-result email-eid from-addr current)]
        (d/transact! conn all-tx)
        (tracking/bump-report-updated! conn report-eid)
        (log/info (str/join ", " (cond-> (mapv (comp name key) new-sets)
                                   close-reason (conj (str "close-reason:" (name close-reason)))))
                  (str "(by " email-mid ")"))))))

(def ^:private directive-pull-pattern
  ;; Proxy-state attrs are pulled as bare refs (we only need :db/id
  ;; for retractions; the setter address comes from their paired
  ;; `-address` cache, pulled separately below).  The other ref
  ;; attrs additionally pull :email/from-address so scope-permits?
  ;; can derive the setter without a second query.
  (into proxy-state-attrs
        [:report/close-reason
         {:report/topic         [:db/id :email/from-address]}
         :report/topic-value
         {:report/deadline      [:db/id :email/from-address]}
         :report/deadline-value
         {:report/expiry        [:db/id :email/from-address]}
         :report/expiry-value
         {:report/superseded-by [:db/id :email/from-address]}
         :report/superseded-by-target
         :report/closed-address :report/acked-address
         :report/owned-address :report/urgent-address
         :report/important-address]))

(defn- set-ref-value-tx
  "Datoms to set the pose-email ref and the paired value in one shot.
  `attr` must be a key of `setter-ref-attrs` with a non-nil paired
  value attr."
  [report-eid email-eid attr value]
  [[:db/add report-eid attr email-eid]
   [:db/add report-eid (setter-ref-attrs attr) value]])

(defn- retract-ref-value-tx
  "Datoms to retract the pose-email ref and the paired value from
  `current`.  Skips the pair when the current value is absent."
  [report-eid current attr]
  (let [value-attr (setter-ref-attrs attr)
        ref-cur    (get current attr)
        val-cur    (get current value-attr)]
    (cond-> []
      ref-cur (conj [:db/retract report-eid attr (ref-eid ref-cur)])
      val-cur (conj [:db/retract report-eid value-attr val-cur]))))

(defn build-directives-tx
  "Build transaction data from resolved commands and current report state.
  Returns the tx vector (may be empty)."
  [report-eid email-eid from-addr resolved current target-eid]
  (let [{:keys [set unset deadline undeadline? expiry unexpiry?
                topic untopic? unsuperseded?]} resolved]
    (-> []
        (into (build-directive-set-tx report-eid email-eid set))
        (cond-> (and (contains? set :report/closed)
                     (not (:report/close-reason current)))
          (conj [:db/add report-eid :report/close-reason :resolved]))
        (into (build-unset-tx report-eid current unset))
        (cond-> (and (contains? unset :report/closed) (:report/close-reason current))
          (conj [:db/retract report-eid :report/close-reason (:report/close-reason current)]))
        ;; Deadline / expiry / topic all share the same set/retract
        ;; shape: pose-email ref + paired `-value` scalar.
        (cond-> deadline
          (into (set-ref-value-tx report-eid email-eid :report/deadline deadline)))
        (cond-> undeadline?
          (into (retract-ref-value-tx report-eid current :report/deadline)))
        (cond-> expiry
          (into (set-ref-value-tx report-eid email-eid :report/expiry expiry)))
        (cond-> unexpiry?
          (into (retract-ref-value-tx report-eid current :report/expiry)))
        (cond-> topic
          (into (set-ref-value-tx report-eid email-eid :report/topic topic)))
        (cond-> untopic?
          (into (retract-ref-value-tx report-eid current :report/topic)))
        ;; Supersede uses the same ref-and-target shape plus the
        ;; side-effects of closing the report and linking it to
        ;; the target bidirectionally.
        (cond-> target-eid
          (into (into (set-ref-value-tx report-eid email-eid
                                        :report/superseded-by target-eid)
                      [[:db/add report-eid :report/closed email-eid]
                       [:db/add report-eid :report/closed-address from-addr]
                       [:db/add report-eid :report/close-reason :superseded]
                       [:db/add report-eid :report/related target-eid]
                       [:db/add target-eid :report/related report-eid]])))
        ;; Unsupersede: retract both ref and target, reopen, clear reason
        (cond-> (and unsuperseded? (:report/superseded-by current))
          (into (into (retract-ref-value-tx report-eid current :report/superseded-by)
                      (cond-> []
                        (:report/closed current)
                        (conj [:db/retract report-eid :report/closed
                               (ref-eid (:report/closed current))])
                        (:report/closed-address current)
                        (conj [:db/retract report-eid :report/closed-address
                               (:report/closed-address current)])
                        (:report/close-reason current)
                        (conj [:db/retract report-eid :report/close-reason
                               (:report/close-reason current)]))))))))

(defn describe-directives
  "Build a human-readable summary of applied directives."
  [resolved target-eid current]
  (let [{:keys [set unset deadline undeadline? expiry unexpiry?
                topic untopic? superseded-by unsuperseded?]} resolved]
    (str/join ", " (concat (map (fn [[attr addr]] (str (name attr) " -> " addr)) set)
                           (map #(str "un-" (name %)) unset)
                           (when deadline [(str "deadline " deadline)])
                           (when undeadline? ["no deadline"])
                           (when expiry [(str "expiry " expiry)])
                           (when unexpiry? ["no expiry"])
                           (when topic [(str "topic:" topic)])
                           (when untopic? ["no topic"])
                           (when target-eid [(str "superseded-by:" superseded-by)])
                           (when (and unsuperseded? (:report/superseded-by-target current))
                             ["not superseded"])))))

(defn- setter-address
  "Return the address credited as the setter of `attr` on `current`.
  For proxy-capable state attrs, reads the `-address` cache (which
  captures the designated setter in the `-by` case).  For all other
  setter-tracked attrs, follows the email ref and reads the pose
  email's `:email/from-address`."
  [current attr]
  (or (get current (address-attrs attr))
      (:email/from-address (get current attr))))

(defn- scope-permits?
  "Check whether a directive scope permits `from-addr` to act on `attr`.
  `current-d` is a `delay` that pulls the report's current state — it
  is only forced in the `:setter-or-maintainer` branch, so emails that
  contain only `:user`/`:maintainer`-scoped directives pay no pull cost.

  - :user                 — anyone
  - :setter-or-maintainer — the address that previously set `attr`, or any
                            maintainer (maintainers retain full override)
  - :maintainer           — any maintainer
  Unknown scopes are rejected with a warning (defensive fallthrough for
  configs that bypassed the validator)."
  [scope attr from-addr is-maintainer? current-d]
  (case scope
    :user                 true
    :setter-or-maintainer (or (boolean is-maintainer?)
                              (= (setter-address @current-d attr) from-addr))
    :maintainer           (boolean is-maintainer?)
    (do (log/warn "Unknown command scope on directive:" scope)
        false)))

(defn- describe-denied-directive
  "Rebuild a short, human-readable form of a parsed directive for use
  in failure records and logs.  Dates are formatted as ISO (yyyy-MM-dd)
  so the failure record stays readable when rendered verbatim in a
  notification email.  Falls back to the `:syntax` of the looked-up
  command when the directive carries no parameter."
  [{:keys [id action email-address date topic target-message-id]}]
  (let [cmd    (get commands-by-id id)
        syntax (or (:syntax cmd) (some-> id name))]
    (case action
      :set              (str syntax ": " email-address)
      :set-deadline     (str syntax ": " (common/format-date-iso date))
      :set-expiry       (str syntax ": " (common/format-date-iso date))
      :set-topic        (str syntax ": " topic)
      :set-superseded   (str syntax ": " target-message-id)
      syntax)))

(defn- filter-permitted-directives
  "Return the subset of `directives` whose scope permits `from-addr` to
  act, optionally further filtered by `action-pred`. `current-d` is
  the delay passed through to `scope-permits?` — only forced when at
  least one directive has scope `:setter-or-maintainer`.

  When `failure-ctx` is non-nil, directives that are rejected by the
  scope check (but pass `action-pred`) are written to the failures
  file as `:insufficient-scope`, audience `:maintainers`, so they
  surface in the next notification round."
  ([directives current-d from-addr is-maintainer?]
   (filter-permitted-directives directives current-d from-addr is-maintainer?
                                 (constantly true) nil))
  ([directives current-d from-addr is-maintainer? action-pred]
   (filter-permitted-directives directives current-d from-addr is-maintainer?
                                 action-pred nil))
  ([directives current-d from-addr is-maintainer? action-pred failure-ctx]
   ;; Eager realization via `filterv` so the recording side effect in
   ;; the denial branch fires deterministically, regardless of whether
   ;; callers seq or reduce over the result.
   (filterv (fn [{:keys [scope attr action] :as directive}]
              (and (action-pred action)
                   (or (scope-permits? scope attr from-addr is-maintainer? current-d)
                       (do (when failure-ctx
                             (record-failure!
                              (assoc failure-ctx
                                     :reason    :insufficient-scope
                                     :audience  :maintainers
                                     :command   (describe-denied-directive directive))))
                           false))))
            directives)))

(defn apply-directives! [conn report-eid directives email-eid from-addr is-maintainer?
                         failure-ctx]
  (let [db         (d/db conn)
        current-d  (delay (d/pull db directive-pull-pattern report-eid))
        permitted  (filter-permitted-directives directives current-d from-addr
                                                 is-maintainer? (constantly true)
                                                 failure-ctx)]
    (when (seq permitted)
      ;; `current` is definitely needed from here on (for close-reason
      ;; lookups, directive tx building, and logging).
      (let [current    @current-d
            resolved   (resolve-commands permitted)
            superseded-by (:superseded-by resolved)
            target-eid (when superseded-by
                         (d/entid db [:report/message-id superseded-by]))
            all-tx     (build-directives-tx report-eid email-eid from-addr
                                            resolved current target-eid)]
        (when (seq all-tx)
          (d/transact! conn all-tx)
          (tracking/bump-report-updated! conn report-eid)
          (when target-eid (tracking/bump-report-updated! conn target-eid))
          (log/info "Commands:" (describe-directives resolved target-eid current)
                    (str "(by " from-addr ")")))
        (when (and superseded-by (nil? target-eid))
          (log/warn "Superseded-by: unknown message-id" superseded-by)
          (when failure-ctx
            (record-failure! (assoc failure-ctx
                                    :reason :unknown-target
                                    :audience :author
                                    :command (str "Superseded-by: " superseded-by)))))))))

(def ^:private unclosed-pull-pattern
  ;; `:report/superseded-by` is pulled as {:db/id :email/from-address}
  ;; so scope-permits? can derive the setter; the target report is
  ;; under `:report/superseded-by-target`.
  [:report/closed :report/closed-address :report/close-reason
   {:report/superseded-by [:db/id :email/from-address]}
   :report/superseded-by-target
   :report/related])

(defn- try-unclosed!
  "If a closed report has a Not closed or Not superseded directive, retract the closure."
  [conn report-eid directives is-maintainer? from-addr failure-ctx]
  (let [current-d (delay (d/pull (d/db conn) unclosed-pull-pattern report-eid))
        permitted (filter-permitted-directives
                   directives current-d from-addr is-maintainer?
                   #{:unset :unset-superseded} failure-ctx)
        {:keys [unset unsuperseded?]} (resolve-commands permitted)]
    (when (or (contains? unset :report/closed) unsuperseded?)
      ;; From here on `current` is always needed.
      (let [current          @current-d
            supersede-pose   (:report/superseded-by current)
            target-ref       (:report/superseded-by-target current)
            clear-supersede? (or unsuperseded?
                                 (and (contains? unset :report/closed) target-ref))
            all-tx   (-> []
                         (into (build-unset-tx report-eid current #{:report/closed}))
                         (cond-> (:report/close-reason current)
                           (conj [:db/retract report-eid :report/close-reason (:report/close-reason current)]))
                         (cond-> (and clear-supersede? target-ref)
                           (into (let [target-eid (ref-eid target-ref)]
                                   (cond-> [[:db/retract report-eid
                                             :report/superseded-by-target target-eid]
                                            [:db/retract report-eid :report/related target-eid]
                                            [:db/retract target-eid :report/related report-eid]]
                                     supersede-pose
                                     (conj [:db/retract report-eid
                                            :report/superseded-by
                                            (ref-eid supersede-pose)]))))))]
        (when (seq all-tx)
          (d/transact! conn all-tx)
          (when (and clear-supersede? target-ref)
            (tracking/bump-report-updated! conn (ref-eid target-ref)))
          (tracking/bump-report-updated! conn report-eid)
          (log/info (str "Commands: "
                         (if unsuperseded? "not superseded" "not closed")
                         " (by " from-addr ")")))))))

(defn- trigger-scope-permits?
  "Scope check for triggers.

  Triggers always *set* an attribute, so the scope values behave as
  follows:
  - :user                 — anyone
  - :maintainer           — maintainer only
  - :setter-or-maintainer — equivalent to :user here; the sender of
                            the trigger IS the setter.  The validator
                            rejects this value on triggers, so this
                            branch only fires for configs that bypass
                            validation.  We fall back to :user silently
                            rather than logging on every incoming
                            trigger.

  Truly unknown scopes (e.g. typos that also bypass validation) are
  rejected with a warning so they surface in the logs."
  [scope is-maintainer?]
  (case scope
    :user                 true
    :setter-or-maintainer true
    :maintainer           (boolean is-maintainer?)
    (do (log/warn "Unknown command scope on trigger:" scope)
        false)))

(defn- describe-denied-trigger
  "Short, human-readable form of a trigger refused by scope — always
  the canonical English command word (\"Closed.\", \"Acked.\", …)
  derived from the command id.  Source-level word overrides aren't
  reflected: the goal is a stable label in failure logs, not a
  round-trip of the user's exact input."
  [attr]
  (some-> (attr->trigger-cmd attr) :id name str/capitalize (str ".")))

(defn- filter-triggers-by-scope
  "Filter `trig-result` to the subset allowed by the effective scope
  for each trigger.  When `failure-ctx` is non-nil, triggers that fail
  the scope check are recorded as `:insufficient-scope` failures with
  `:audience :maintainers`, so denied attempts surface to maintainer
  subscribers via the notification loop."
  [trig-result overrides is-maintainer? failure-ctx]
  (when trig-result
    (let [filtered (into {}
                         (keep (fn [[attr :as entry]]
                                 (let [cmd   (attr->trigger-cmd attr)
                                       scope (or (:scope (get overrides (:id cmd))) (:scope cmd))]
                                   (if (trigger-scope-permits? scope is-maintainer?)
                                     entry
                                     (do (when failure-ctx
                                           (record-failure!
                                            (assoc failure-ctx
                                                   :reason   :insufficient-scope
                                                   :audience :maintainers
                                                   :command  (describe-denied-trigger attr))))
                                         nil)))))
                         (dissoc trig-result :report/close-reason))]
      (when (seq filtered)
        (cond-> filtered
          (:report/close-reason trig-result) (assoc :report/close-reason
                                                    (:report/close-reason trig-result)))))))

(defn apply-commands!
  "Detect and apply all commands from an email's body text.
  Returns true when at least one command (trigger, directive, or vote)
  was detected, false otherwise."
  [conn report-eid report-type email source-map roles delivery]
  (if-let [body-text (common/email-body-text email)]
    (let [db          (d/db conn)
          from-addr   (:email/from-address email)
          eid         (:db/id email)
          report-mid  (:report/message-id (d/entity db report-eid))
          src-name    (d/q '[:find ?src . :in $ ?rid
                             :where [?rid :report/email ?e] [?e :email/source ?src]] db report-eid)
          source-cfg  (get source-map src-name)
          src-cmds    (build-source-commands source-cfg)
          overrides   (:overrides src-cmds)
          is-maint?   (common/maintainer? roles from-addr (:email/date-sent email))
          fail-ctx    (when (and from-addr src-name)
                        {:source     src-name
                         :from-addr  from-addr
                         :email-date (:email/date-sent email)
                         :report-mid report-mid})
          trig-result (-> (detect-triggers report-type body-text src-cmds)
                          (filter-triggers-by-scope overrides is-maint? fail-ctx))
          aliases     (compile-directive-aliases (:command-aliases source-cfg))
          directives  (detect-directives report-type body-text overrides (:email/date-sent email) aliases)
          closed?     (some? (:report/closed (d/pull db [:report/closed] report-eid)))]

      (if closed?
        (do (try-unclosed! conn report-eid directives is-maint? from-addr fail-ctx)
            (boolean (seq directives)))
        (let [voted? (when (and (= :request report-type) from-addr)
                       (apply-vote! conn report-eid from-addr body-text email delivery source-cfg)
                       (some? (detect-vote body-text)))]
          (apply-triggers! conn report-eid trig-result eid (:email/message-id email) from-addr)
          (apply-directives! conn report-eid directives eid from-addr is-maint? fail-ctx)
          (boolean (or (seq trig-result) (seq directives) voted?)))))
    false))
