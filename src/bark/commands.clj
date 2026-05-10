;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.commands
  "Unified command detection, resolution, and application."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [datalevin.core :as d]
            [taoensso.timbre :as log]
            [bark.common :as common]
            [bark.commands.registry :refer [commands-by-id directive-commands
                                            attr->trigger-cmd proxy-state-attrs
                                            address-attrs setter-ref-attrs]]
            [bark.periods :as periods]
            [bark.relations :as rel]
            [bark.tracking :as tracking])
  (:import [java.time LocalDate ZoneOffset]
           [java.util Date]))

;; ---------------------------------------------------------------------------
;; Trailing punctuation
;; ---------------------------------------------------------------------------

(def trailing-punct "[.,;:?!]")

;; ---------------------------------------------------------------------------
;; Pattern compilation
;; ---------------------------------------------------------------------------

(def ^:private strict-punct-actions
  "Trigger actions that must NOT accept bare whitespace as a separator
  (too many false positives with ordinary prose, e.g. \"Important note:\")."
  #{:urgent :important})

(defn- trigger-pattern [strict-punct? strict-syntax? & words]
  (re-pattern
   (str "(?m)^" (common/bang-prefix strict-syntax?)
        "(" (str/join "|" (map #(java.util.regex.Pattern/quote %) words))
        ")(?:" trailing-punct (when-not strict-punct? "|\\s") "|$)")))

(defn- directive-pattern [strict-syntax? {:keys [syntax param]}]
  (let [qs       (java.util.regex.Pattern/quote syntax)
        prefix   (common/bang-prefix strict-syntax?)
        addr     "[^@<>\\s]+@[^@<>\\s]+\\.[^@<>\\s]+"   ; email address (dot required)
        mid      "[^<>\\s]+@[^<>\\s]+"                  ; bracketed message-id
        mid-path "[^/<>\\s]+@[^/<>\\s]+"]               ; mid in path or bare
    (re-pattern
     (case param
       :email-address    (str "^" prefix qs ":\\s+(?:.*<(" addr ")>|(" addr "))" trailing-punct "?\\s*$")
       :date             (str "^" prefix qs ":\\s+(\\d{4}-\\d{2}-\\d{2})" trailing-punct "?\\s*$")
       :date-or-duration (str "^" prefix qs ":\\s+(\\d{4}-\\d{2}-\\d{2}|\\d+[dwmy](?:\\s+\\d+[dwmy])*)" trailing-punct "?\\s*$")
       :word             (str "^" prefix qs ":\\s+([a-zA-Z0-9_-]+)" trailing-punct "?\\s*$")
       :message-id       (str "^" prefix qs ":\\s+(?:.*<(" mid ")>|.*/(" mid-path ")/?|(" mid-path "))" trailing-punct "?\\s*$")
       (str "^" prefix qs trailing-punct "?\\s*$")))))

(defn- compile-trigger-words [strict-syntax? action-map]
  (into {}
        (map (fn [[k words]]
               [k (apply trigger-pattern
                         (contains? strict-punct-actions k)
                         strict-syntax?
                         words)]))
        action-map))

(def default-compiled-commands
  "Precompiled default trigger regex for :loose mode. Strict mode
  recompiles on demand in build-source-commands."
  (compile-trigger-words false common/default-commands))

(def ^:private compile-directives
  "Memoized directive compilation keyed on strict-syntax? -- only two
  shapes exist (loose/strict), so memoizing collapses to at most two
  calls per process."
  (memoize
   (fn [strict-syntax?]
     (mapv (fn [cmd] [cmd (directive-pattern strict-syntax? cmd)]) directive-commands))))

(defn build-source-commands
  "Return a source-commands descriptor with keys:
    :commands       -- {cmd-id [strings]} active vocabulary
    :compiled       -- {cmd-id regex} compiled trigger patterns
    :directives     -- [[cmd pattern] …] compiled directive patterns
    :strict-syntax? -- boolean (true when :command-syntax is :strict)
    :overrides      -- per-command :scope/:report-types overrides"
  [source-cfg]
  (let [commands       (common/resolve-commands-map source-cfg)
        strict-syntax? (= :strict (common/resolve-command-syntax source-cfg))]
    {:commands       commands
     :compiled       (if (and (not strict-syntax?)
                              (= commands common/default-commands))
                       default-compiled-commands
                       (compile-trigger-words strict-syntax? commands))
     :directives     (compile-directives strict-syntax?)
     :strict-syntax? strict-syntax?
     :overrides      (common/resolve-command-overrides source-cfg)}))

;; ---------------------------------------------------------------------------
;; Detection (pure)
;; ---------------------------------------------------------------------------

(defn- detect-close-reason [closed-words body-text strict-syntax?]
  (when (seq closed-words)
    (let [pattern (apply trigger-pattern false strict-syntax? closed-words)]
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

(defn detect-triggers
  "Detect trigger matches in `body-text` for a given `report-type`,
  using the precompiled vocabulary in `source-commands`."
  [report-type body-text {:keys [compiled commands strict-syntax? overrides]}]
  (when body-text
    (let [all-sets (match-triggers compiled body-text)
          ;; Pre-compute close-reason from unfiltered triggers so it survives
          ;; any future refactoring of the filter step.
          reason   (when (:report/closed all-sets)
                     (detect-close-reason (:closed commands) body-text strict-syntax?))
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

(def ^:private compiled-directives-loose (compile-directives false))

(defn detect-directives
  "Detect directives in `body-text`. The optional `compiled-dirs`
  arg provides the precompiled directive patterns (from
  `build-source-commands`); if omitted, the loose-mode defaults are
  used."
  ([report-type body-text] (detect-directives report-type body-text nil nil compiled-directives-loose))
  ([report-type body-text overrides] (detect-directives report-type body-text overrides nil compiled-directives-loose))
  ([report-type body-text overrides email-date] (detect-directives report-type body-text overrides email-date compiled-directives-loose))
  ([report-type body-text overrides email-date compiled-dirs]
   (when body-text
     (let [lines (str/split-lines body-text)
           all-directives compiled-dirs]
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
                                               :set-superseded   (when-let [mid (or (nth m 1 nil) (nth m 2 nil) (nth m 3 nil))]
                                                                   {:action :set-superseded
                                                                    :attr attr
                                                                    :target-message-id (str "<" mid ">")})
                                               :unset-superseded {:action :unset-superseded :attr attr}
                                               :set-duplicate    (when-let [mid (or (nth m 1 nil) (nth m 2 nil) (nth m 3 nil))]
                                                                   {:action :set-duplicate
                                                                    :attr attr
                                                                    :target-message-id (str "<" mid ">")})
                                               :unset-duplicate  {:action :unset-duplicate :attr attr}
                                               :set-related      (when-let [mid (or (nth m 1 nil) (nth m 2 nil) (nth m 3 nil))]
                                                                   {:action :set-related
                                                                    :attr attr
                                                                    :target-message-id (str "<" mid ">")})
                                               :unset-related    (when-let [mid (or (nth m 1 nil) (nth m 2 nil) (nth m 3 nil))]
                                                                   {:action :unset-related
                                                                    :attr attr
                                                                    :target-message-id (str "<" mid ">")}))]
                                    (when base (assoc base :scope sc :id id)))))))
                          all-directives)))
            vec)))))

;; ---------------------------------------------------------------------------
;; Command failure recording (file-based)
;; ---------------------------------------------------------------------------

(def ^:dynamic *failures-file*
  "Path to the failures EDN file. Bound to `bark.common/failures-file-path`
  in production; tests rebind it to a temp path so they don't pollute
  the real file."
  common/failures-file-path)

(def ^:private max-failure-age-ms (* 365 24 60 60 1000))

(defn- load-failures []
  (common/read-failures-file *failures-file*))

(defn- save-failures! [failures]
  (io/make-parents (io/file *failures-file*))
  (spit *failures-file* (pr-str failures)))

(defn record-failure!
  "Append a command failure to the failures file for later notification.
  Prunes entries older than 1 year.

  `:audience` controls who the notifier will route the entry to:
  - `:author`      -- the address that sent the command (the default,
                     used for typo-class failures like `Superseded-by:`
                     with an unknown target).
  - `:maintainers` -- all maintainer subscribers on the source, so a
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
              :unset-superseded (-> acc (dissoc :superseded-by) (assoc :unsuperseded? true))
              :set-duplicate    (-> acc (assoc :duplicate-of target-message-id) (dissoc :unduplicate?))
              :unset-duplicate  (-> acc (dissoc :duplicate-of) (assoc :unduplicate? true))
              :set-related      (-> acc
                                    (update :related-to-set   (fnil conj #{}) target-message-id)
                                    (update :related-to-unset (fnil disj #{}) target-message-id))
              :unset-related    (-> acc
                                    (update :related-to-unset (fnil conj #{}) target-message-id)
                                    (update :related-to-set   (fnil disj #{}) target-message-id))))
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
        ;; :vote/key has :db.unique/identity -- if this voter already voted,
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
  Points the attr to the real email and stores the designated address
  in its lowercased form so downstream comparisons against
  `:email/author-address` are case-insensitive."
  [report-eid email-eid set-map]
  (into []
        (mapcat (fn [[attr addr]]
                  [[:db/add report-eid attr email-eid]
                   [:db/add report-eid (address-attrs attr) (str/lower-case addr)]]))
        set-map))

(defn build-trigger-tx
  "Build transaction data for trigger results.
  `current` is the report's current state (pulled with
  `proxy-state-attrs`).  The `-address` cache is stored lowercased
  so downstream comparisons against `:email/author-address` are
  case-insensitive.
  Returns [tx-data new-sets] or nil if nothing to do."
  [report-eid trig-result email-eid from-addr current]
  (let [close-reason (:report/close-reason trig-result)
        ref-result   (dissoc trig-result :report/close-reason)
        new-sets     (into {} (remove (fn [[k _]] (get current k))) ref-result)
        addr-lc      (some-> from-addr str/lower-case)
        all-tx       (cond-> (when (seq new-sets)
                               (into [(into {:db/id report-eid} (map (fn [[k _]] [k email-eid])) new-sets)]
                                     (map (fn [[k _]] [:db/add report-eid (address-attrs k) addr-lc]))
                                     new-sets))
                       (and close-reason (:report/closed new-sets))
                       (conj [:db/add report-eid :report/close-reason close-reason]))]
    (when (seq all-tx) [(vec all-tx) new-sets close-reason])))

(defn apply-triggers! [conn report-eid trig-result email-eid email-mid from-addr source-cfg]
  (when trig-result
    (let [db      (d/db conn)
          current (d/pull db proxy-state-attrs report-eid)
          rtype   (:report/type (d/pull db [:report/type] report-eid))]
      (when-let [[all-tx new-sets close-reason]
                 (build-trigger-tx report-eid trig-result email-eid from-addr current)]
        (d/transact! conn all-tx)
        (tracking/bump-report-updated! conn report-eid)
        (log/info (str/join ", " (cond-> (mapv (comp name key) new-sets)
                                   close-reason (conj (str "close-reason:" (name close-reason)))))
                  (str "(by " email-mid ")"))
        ;; Propagate trigger-driven closure of a patch to the
        ;; bugs/requests it resolves (no successor in the trigger path).
        ;; Sources with `:patch-triggers? false` opt out of the :resolved
        ;; propagation; :canceled retraction still runs (no-op when no
        ;; auto-credit was posed in the first place).
        (when (and (= :patch rtype) close-reason (:report/closed new-sets)
                   (or (not= :resolved close-reason)
                       (common/patch-triggers? source-cfg)))
          (rel/propagate-patch-closure! conn report-eid rtype email-eid
                                        close-reason nil))))))

(def ^:private directive-pull-pattern
  ;; Proxy-state attrs are pulled as bare refs (we only need :db/id
  ;; for retractions; the setter address comes from their paired
  ;; `-address` cache, pulled separately below).  The other ref
  ;; attrs additionally pull :email/author-address so scope-permits?
  ;; can derive the setter without a second query.
  ;; Note: superseded-by lives in :rel/* now, not in attributes.
  (into proxy-state-attrs
        [:report/close-reason
         :report/type
         {:report/topic         [:db/id :email/author-address]}
         :report/topic-value
         {:report/deadline      [:db/id :email/author-address]}
         :report/deadline-value
         {:report/expiry        [:db/id :email/author-address]}
         :report/expiry-value
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

(def ^:private paired-directive-attrs
  "Directives that share the same {pose-email ref + paired `-value` scalar}
  tx shape.  Each row maps [resolved-set-key resolved-unset-key attr]:
    - set-key   -- value in the resolved map that triggers an assertion
    - unset-key -- flag in the resolved map that triggers a retraction
    - attr      -- report attribute the pair targets"
  [[:deadline :undeadline? :report/deadline]
   [:expiry   :unexpiry?   :report/expiry]
   [:topic    :untopic?    :report/topic]])

(defn- build-paired-directive-tx
  "Apply every set/unset from `paired-directive-attrs` to `tx`, reading
  the value/flag pair from `resolved`."
  [tx report-eid email-eid current resolved]
  (reduce (fn [tx [set-k unset-k attr]]
            (cond-> tx
              (get resolved set-k)
              (into (set-ref-value-tx report-eid email-eid attr (get resolved set-k)))
              (get resolved unset-k)
              (into (retract-ref-value-tx report-eid current attr))))
          tx paired-directive-attrs))

(defn- close-with-reason-tx
  "Tx datoms to close `report-eid` with `close-reason`.  The relation
  pose (:supersedes / :duplicates plus :related-to) is done separately
  by `apply-directives!` via bark.relations."
  [report-eid email-eid from-addr close-reason]
  [[:db/add report-eid :report/closed email-eid]
   [:db/add report-eid :report/closed-address from-addr]
   [:db/add report-eid :report/close-reason close-reason]])

(defn- reopen-tx
  "Tx datoms to undo a close: reopen the report and clear the close
  reason.  Used by both Not superseded. and Not duplicate.  The matching
  relation retract is done separately by `try-unclosed!`."
  [report-eid current]
  (cond-> []
    (:report/closed current)
    (conj [:db/retract report-eid :report/closed
           (ref-eid (:report/closed current))])
    (:report/closed-address current)
    (conj [:db/retract report-eid :report/closed-address
           (:report/closed-address current)])
    (:report/close-reason current)
    (conj [:db/retract report-eid :report/close-reason
           (:report/close-reason current)])))

(defn build-directives-tx
  "Build transaction data from resolved commands and current report state.
  Returns the tx vector (may be empty).
  Note: Supersede and Duplicate-of are handled separately by
  `apply-directives!` (they post a relation, not a paired attr)."
  [report-eid email-eid from-addr resolved current
   supersede-target-eid duplicate-target-eid
   unsupersede-now? unduplicate-now?]
  (let [{:keys [set unset]} resolved]
    (-> []
        (into (build-directive-set-tx report-eid email-eid set))
        (cond-> (and (contains? set :report/closed)
                     (not (:report/close-reason current)))
          (conj [:db/add report-eid :report/close-reason :resolved]))
        (into (build-unset-tx report-eid current unset))
        (cond-> (and (contains? unset :report/closed) (:report/close-reason current))
          (conj [:db/retract report-eid :report/close-reason (:report/close-reason current)]))
        (build-paired-directive-tx report-eid email-eid current resolved)
        (cond-> supersede-target-eid
          (into (close-with-reason-tx report-eid email-eid from-addr :superseded)))
        (cond-> duplicate-target-eid
          (into (close-with-reason-tx report-eid email-eid from-addr :canceled)))
        (cond-> (or unsupersede-now? unduplicate-now?)
          (into (reopen-tx report-eid current))))))

(defn describe-directives
  "Build a human-readable summary of applied directives."
  [resolved]
  (let [{:keys [set unset deadline undeadline? expiry unexpiry?
                topic untopic? superseded-by unsuperseded?
                duplicate-of unduplicate?
                related-to-set related-to-unset]} resolved]
    (str/join ", " (concat (map (fn [[attr addr]] (str (name attr) " -> " addr)) set)
                           (map #(str "un-" (name %)) unset)
                           (when deadline [(str "deadline " deadline)])
                           (when undeadline? ["no deadline"])
                           (when expiry [(str "expiry " expiry)])
                           (when unexpiry? ["no expiry"])
                           (when topic [(str "topic:" topic)])
                           (when untopic? ["no topic"])
                           (when superseded-by [(str "superseded-by:" superseded-by)])
                           (when unsuperseded? ["not superseded"])
                           (when duplicate-of [(str "duplicate-of:" duplicate-of)])
                           (when unduplicate? ["not duplicate"])
                           (map #(str "related-to:" %) related-to-set)
                           (map #(str "not-related-to:" %) related-to-unset)))))

(defn- setter-address
  "Return the address credited as the setter of `attr` on `current`.
  For proxy-capable state attrs, reads the `-address` cache (which
  captures the designated setter in the `-by` case).  For all other
  setter-tracked attrs, follows the email ref and reads the pose
  email's `:email/author-address`."
  [current attr]
  (or (get current (address-attrs attr))
      (:email/author-address (get current attr))))

(defn- scope-permits?
  "Check whether a directive scope permits `from-addr` to act on `attr`.
  `current-d` is a `delay` that pulls the report's current state -- it
  is only forced in the `:setter-or-maintainer` branch, so emails that
  contain only `:user`/`:maintainer`-scoped directives pay no pull cost.

  - :user                 -- anyone
  - :setter-or-maintainer -- the address that previously set `attr`, or any
                            maintainer (maintainers retain full override).
                            The setter comparison is case-insensitive so
                            that historical records stored with mixed
                            case still match.
  - :maintainer           -- any maintainer
  Unknown scopes are rejected with a warning (defensive fallthrough for
  configs that bypassed the validator)."
  [scope attr from-addr is-maintainer? current-d]
  (case scope
    :user                 true
    :setter-or-maintainer (or (boolean is-maintainer?)
                              (when-let [setter (setter-address @current-d attr)]
                                (and from-addr
                                     (= (str/lower-case setter)
                                        (str/lower-case from-addr)))))
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
      :set-duplicate    (str syntax ": " target-message-id)
      :set-related      (str syntax ": " target-message-id)
      :unset-related    (str syntax ": " target-message-id)
      syntax)))

(defn- filter-permitted-directives
  "Return the subset of `directives` whose scope permits `from-addr` to
  act, optionally further filtered by `action-pred`. `current-d` is
  the delay passed through to `scope-permits?` -- only forced when at
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

(defn- relation-active-from?
  "True if `report-eid` has at least one active outgoing relation of `kind`."
  [db report-eid kind]
  (boolean
   (seq (d/q '[:find [?e ...] :in $ ?from ?kind
               :where
               [?e :rel/from ?from]
               [?e :rel/kind ?kind]
               [?e :rel/active? true]]
             db report-eid kind))))

(defn- resolve-target
  "Look up the target report-eid for a relation directive (Superseded-by:
  or Duplicate-of:).  Returns {:target-eid :target-type :valid?} where
  `:valid?` is true iff the target exists AND the type constraint passes
  for `kind`.  Mids exceeding the LMDB index limit are treated as
  unknown (the target cannot have been stored)."
  [db kind report-eid source-type target-mid]
  (let [target-eid  (when (common/indexable-mid? target-mid)
                      (d/entid db [:report/message-id target-mid]))
        target-type (when target-eid
                      (:report/type (d/pull db [:report/type] target-eid)))]
    {:target-eid  target-eid
     :target-type target-type
     :valid?      (and target-eid
                       (rel/valid-pose? kind report-eid target-eid
                                        source-type target-type))}))

(defn- record-target-failures!
  [failure-ctx syntax target-mid target-eid valid?]
  (when failure-ctx
    (cond
      (and target-mid (nil? target-eid))
      (record-failure! (assoc failure-ctx
                              :reason :unknown-target
                              :audience :author
                              :command (str syntax ": " target-mid)))
      (and target-eid (not valid?))
      (record-failure! (assoc failure-ctx
                              :reason :type-mismatch
                              :audience :author
                              :command (str syntax ": " target-mid))))))

(defn- apply-related-to!
  "Pose / retract :related-to relations from a resolved directives map.
  Independent of report closure state; called from both `apply-directives!`
  (open path) and `try-unclosed!` (closed path).  Emits failures for
  unknown targets, ignores self-loops silently."
  [conn report-eid resolved email-eid from-addr failure-ctx]
  (let [db        (d/db conn)
        ;; Filter mids exceeding the LMDB index limit; the lookup would
        ;; otherwise raise MDB_BAD_VALSIZE.  Such targets cannot exist
        ;; in the DB (we reject them at ingestion), so reporting them
        ;; as :unknown-target is also accurate.
        indexable (filter common/indexable-mid?)
        to-pose   (into #{} indexable (:related-to-set resolved))
        to-clear  (into #{} indexable (:related-to-unset resolved))
        posed-at  (Date.)]
    (doseq [mid to-pose]
      (let [target-eid (d/entid db [:report/message-id mid])]
        (cond
          (nil? target-eid)
          (when failure-ctx
            (record-failure! (assoc failure-ctx
                                    :reason   :unknown-target
                                    :audience :author
                                    :command  (str "Related-to: " mid))))
          (= report-eid target-eid)
          (log/warn "Related-to: self-loop ignored" mid)
          :else
          (do (rel/pose-if-absent! conn {:from-eid  report-eid
                                         :to-eid    target-eid
                                         :kind      :related-to
                                         :setter    from-addr :email-eid email-eid
                                         :posed-at  posed-at  :value nil})
              (tracking/bump-report-updated! conn target-eid)
              (log/info "Related-to:" mid "(by" from-addr ")")))))
    (doseq [mid to-clear]
      (let [target-eid (d/entid db [:report/message-id mid])]
        (when target-eid
          (when (rel/retract-pair! conn report-eid :related-to
                                    target-eid email-eid)
            (tracking/bump-report-updated! conn target-eid)
            (log/info "Not related-to:" mid "(by" from-addr ")")))))))

(defn- relation-target-eid
  "Eid of the target report of the active outgoing relation of `kind`
  on `report-eid`, or nil."
  [db report-eid kind]
  (d/q '[:find ?to . :in $ ?from ?kind
         :where
         [?e :rel/from ?from]
         [?e :rel/kind ?kind]
         [?e :rel/active? true]
         [?e :rel/to ?to]]
       db report-eid kind))

(defn- relation-setter
  "`:rel/setter` address of the active outgoing relation of `kind` on
  `report-eid`, or nil."
  [db report-eid kind]
  (d/q '[:find ?setter . :in $ ?from ?kind
         :where
         [?e :rel/from ?from]
         [?e :rel/kind ?kind]
         [?e :rel/active? true]
         [?e :rel/setter ?setter]]
       db report-eid kind))

(defn- relation-setters-as-pull
  "Build a partial pull map exposing relation setters under `:setter-attr`
  so `scope-permits?` can resolve `:setter-or-maintainer` on relation
  unset directives.  `rows` is a seq carrying `:kind` and `:setter-attr`."
  [db report-eid rows]
  (into {} (keep (fn [{:keys [kind setter-attr]}]
                   (when-let [s (relation-setter db report-eid kind)]
                     [setter-attr {:email/author-address s}])))
        rows))

(def ^:private closure-relation-rows
  "Specs for directive-driven closure relations (Superseded-by /
  Duplicate-of).  Each row drives an iteration in `apply-directives!`:
    :kind          -- relation kind to pose
    :propagate     -- close-reason passed to propagate-patch-closure!
    :propagate-tgt -- when true, pass the target eid as `successor-eid`
    :syntax        -- human-readable directive name (for failure logs)
    :mid-key       -- key in resolved holding the target message-id
    :unset-key     -- key in resolved holding the unset flag
    :setter-attr   -- key under which the relation's :rel/setter is
                      surfaced in the pull map so `scope-permits?` can
                      resolve `:setter-or-maintainer` on the unset
                      directive in the open-report path."
  [{:kind :supersedes :propagate :superseded :propagate-tgt true
    :syntax "Superseded-by" :mid-key :superseded-by :unset-key :unsuperseded?
    :setter-attr :rel/supersedes}
   {:kind :duplicates :propagate :canceled :propagate-tgt false
    :syntax "Duplicate-of" :mid-key :duplicate-of :unset-key :unduplicate?
    :setter-attr :rel/duplicates}])

(defn- compute-closure-rows
  "Enrich `closure-relation-rows` with per-kind decisions derived from
  `resolved` and current DB state.  Each row gains:
    :target-mid -- string from `resolved`'s mid-key (or nil)
    :resolved   -- {:target-eid :target-type :valid?} from `resolve-target`
    :clear-now? -- true when an active relation should be retracted
                   on this email (explicit unset directive, AND a
                   matching active outgoing relation exists)."
  [db report-eid source-type resolved]
  (mapv (fn [{:keys [kind unset-key mid-key] :as row}]
          (let [target-mid (get resolved mid-key)
                r          (resolve-target db kind report-eid
                                            source-type target-mid)]
            (assoc row
                   :target-mid target-mid
                   :resolved   r
                   :clear-now? (and (boolean (get resolved unset-key))
                                    (relation-active-from? db report-eid kind)))))
        closure-relation-rows))

(defn apply-directives! [conn report-eid directives email-eid from-addr is-maintainer?
                         failure-ctx]
  (let [db         (d/db conn)
        ;; Surface relation setters under their :setter-attr so the
        ;; scope check on `:unsuperseded` / `:unduplicate` works on the
        ;; open-report path (mirrors the same trick in `try-unclosed!`).
        current-d  (delay (merge (d/pull db directive-pull-pattern report-eid)
                                  (relation-setters-as-pull db report-eid
                                                             closure-relation-rows)))
        permitted  (filter-permitted-directives directives current-d from-addr
                                                 is-maintainer? (constantly true)
                                                 failure-ctx)]
    (when (seq permitted)
      (let [current     @current-d
            resolved    (resolve-commands permitted)
            source-type (:report/type current)
            rows        (compute-closure-rows db report-eid source-type resolved)
            by-kind     (into {} (map (juxt :kind identity)) rows)
            valid-tgt   (fn [k] (when (get-in by-kind [k :resolved :valid?])
                                  (get-in by-kind [k :resolved :target-eid])))
            sup-target  (valid-tgt :supersedes)
            dup-target  (valid-tgt :duplicates)
            unsupersede-now? (get-in by-kind [:supersedes :clear-now?])
            unduplicate-now? (get-in by-kind [:duplicates :clear-now?])
            all-tx      (build-directives-tx
                         report-eid email-eid from-addr resolved current
                         sup-target dup-target
                         unsupersede-now? unduplicate-now?)]
        (when (seq all-tx)
          (d/transact! conn all-tx)
          (tracking/bump-report-updated! conn report-eid)
          (doseq [{:keys [resolved]} rows
                  :let [t (:target-eid resolved)]
                  :when t]
            (tracking/bump-report-updated! conn t))
          (log/info "Commands:"
                    (describe-directives resolved)
                    (str "(by " from-addr ")"))
          ;; A patch just got closed with :canceled or :superseded via
          ;; the directive: propagate to the bugs it resolves.
          (when (= :patch source-type)
            (doseq [{:keys [resolved propagate propagate-tgt]} rows
                    :when (:valid? resolved)]
              (rel/propagate-patch-closure!
               conn report-eid :patch email-eid
               propagate (when propagate-tgt (:target-eid resolved))))))
        ;; Pose :supersedes / :duplicates (+ :related-to) once attrs are written.
        (let [posed-at (Date.)]
          (doseq [{:keys [kind resolved target-mid]} rows
                  :when (:valid? resolved)
                  :let [tgt (:target-eid resolved)
                        opts {:from-eid report-eid :to-eid tgt
                              :setter from-addr :email-eid email-eid
                              :posed-at posed-at}]]
            (rel/pose-if-absent! conn (assoc opts :kind kind :value target-mid))
            (rel/pose-if-absent! conn (assoc opts :kind :related-to :value nil))))
        ;; Retract relation on Not (super|dupli)cated
        (doseq [{:keys [kind clear-now?]} rows :when clear-now?]
          (rel/retract-by-from! conn report-eid kind email-eid))
        ;; Errors / warnings
        (doseq [{:keys [resolved syntax target-mid]} rows
                :let [tgt-eid (:target-eid resolved)
                      valid?  (:valid? resolved)]]
          (record-target-failures! failure-ctx syntax target-mid tgt-eid valid?)
          (when (and tgt-eid (not valid?))
            (log/warn (str syntax ": type mismatch -- source")
                      source-type "vs target" (:target-type resolved))))
        (apply-related-to! conn report-eid resolved email-eid from-addr failure-ctx)))))

(def ^:private unclosed-pull-pattern
  [:report/closed :report/closed-address :report/close-reason])

(def ^:private unclose-relation-rows
  "Per-row data for retracting a closure relation in `try-unclosed!`.
    :kind        -- relation kind to retract
    :unset-key   -- flag in `resolved` that explicitly clears this kind
    :setter-attr -- key under which the setter is surfaced in the pull
                    map so `scope-permits?` can resolve
                    `:setter-or-maintainer` on the matching directive."
  [{:kind :supersedes :unset-key :unsuperseded? :setter-attr :rel/supersedes}
   {:kind :duplicates :unset-key :unduplicate?  :setter-attr :rel/duplicates}])

(defn- try-unclosed!
  "If a closed report has a Not closed / Not superseded / Not duplicate
  directive, retract the closure (and the relation if any)."
  [conn report-eid directives email-eid is-maintainer? from-addr failure-ctx]
  (let [db          (d/db conn)
        current-d   (delay (merge (d/pull db unclosed-pull-pattern report-eid)
                                  (relation-setters-as-pull
                                   db report-eid unclose-relation-rows)))
        permitted   (filter-permitted-directives
                     directives current-d from-addr is-maintainer?
                     #{:unset :unset-superseded :unset-duplicate
                       :set-related :unset-related} failure-ctx)
        resolved    (resolve-commands permitted)
        unset       (:unset resolved)
        unset-closed? (contains? unset :report/closed)]
    (apply-related-to! conn report-eid resolved email-eid from-addr failure-ctx)
    (when (or unset-closed? (:unsuperseded? resolved) (:unduplicate? resolved))
      (let [current  @current-d
            ;; For each row: target-eid + whether to clear this relation
            ;; (explicit Not X, or implicit when Not closed and the closure
            ;; came from this relation).
            rows     (mapv (fn [{:keys [kind unset-key] :as row}]
                             (let [target (relation-target-eid db report-eid kind)]
                               (assoc row
                                      :target target
                                      :clear? (or (boolean (get resolved unset-key))
                                                  (and unset-closed? target)))))
                           unclose-relation-rows)
            attr-tx  (-> []
                         (into (build-unset-tx report-eid current #{:report/closed}))
                         (cond-> (:report/close-reason current)
                           (conj [:db/retract report-eid :report/close-reason
                                  (:report/close-reason current)])))
            any-clear? (some :clear? rows)]
        (when (seq attr-tx)
          (d/transact! conn attr-tx))
        (doseq [{:keys [kind target clear?]} rows :when clear?]
          (rel/retract-by-from! conn report-eid kind email-eid)
          (when target
            ;; The supersede/duplicate pose also added a :related-to
            ;; (audit link); clear it so undo restores the prior state.
            (rel/retract-pair! conn report-eid :related-to target email-eid)
            (tracking/bump-report-updated! conn target)))
        (when (or (seq attr-tx) any-clear?)
          (tracking/bump-report-updated! conn report-eid)
          (log/info (str "Commands: "
                         (cond (:unsuperseded? resolved) "not superseded"
                               (:unduplicate? resolved)  "not duplicate"
                               :else                     "not closed")
                         " (by " from-addr ")")))))))

(defn- trigger-scope-permits?
  "Scope check for triggers.

  Triggers always *set* an attribute, so the scope values behave as
  follows:
  - :user                 -- anyone
  - :maintainer           -- maintainer only
  - :setter-or-maintainer -- equivalent to :user here; the sender of
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
  "Short, human-readable form of a trigger refused by scope -- always
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
          from-addr   (:email/author-address email)
          eid         (:db/id email)
          report-mid  (:report/message-id (d/entity db report-eid))
          src-name    (d/q '[:find ?src . :in $ ?rid
                             :where [?rid :report/email ?e] [?e :email/source ?src]] db report-eid)
          source-cfg  (when-let [cfg (get source-map src-name)]
                          (periods/source-cfg-at-date cfg (:email/date-sent email)))
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
          directives  (detect-directives report-type body-text overrides
                                         (:email/date-sent email)
                                         (:directives src-cmds))
          closed?     (some? (:report/closed (d/pull db [:report/closed] report-eid)))]

      (if closed?
        (do (try-unclosed! conn report-eid directives eid is-maint? from-addr fail-ctx)
            (boolean (seq directives)))
        (let [voted? (when (and (= :request report-type) from-addr)
                       (apply-vote! conn report-eid from-addr body-text email delivery source-cfg)
                       (some? (detect-vote body-text)))]
          (apply-triggers! conn report-eid trig-result eid (:email/message-id email) from-addr
                           source-cfg)
          (apply-directives! conn report-eid directives eid from-addr is-maint? fail-ctx)
          (boolean (or (seq trig-result) (seq directives) voted?)))))
    false))
