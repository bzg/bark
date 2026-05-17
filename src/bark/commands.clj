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
            [bark.commands.registry :refer [commands-by-id line-commands
                                            attr->word-cmd proxy-state-attrs
                                            address-attrs setter-ref-attrs]]
            [bark.detect :as detect]
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

(defn- word-pattern [strict-punct? strict-syntax? & words]
  (re-pattern
   (str "(?m)^" (common/bang-prefix strict-syntax?)
        "(" (str/join "|" (map #(java.util.regex.Pattern/quote %) words))
        ")(?:" trailing-punct (when-not strict-punct? "|\\s") "|$)")))

(defn- line-pattern [strict-syntax? {:keys [syntax param]}]
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

(def ^:private compile-word-patterns
  "Compile an action=>words map into action=>regex."
  (memoize
   (fn [strict-syntax? action-map]
     (into {}
           (map (fn [[k words]]
                  [k (apply word-pattern
                            (contains? strict-punct-actions k)
                            strict-syntax?
                            words)]))
           action-map))))

(def ^:private compile-line-patterns
  "Compile colon-line command patterns for a syntax mode."
  (memoize
   (fn [strict-syntax?]
     (mapv (fn [cmd] [cmd (line-pattern strict-syntax? cmd)]) line-commands))))

(defn build-source-commands
  "Return a source-commands descriptor with keys:
    :commands       -- {cmd-id [strings]} active vocabulary
    :word-patterns  -- {cmd-id regex} compiled bareword patterns
    :line-patterns  -- [[cmd pattern] ...] compiled colon-line patterns
    :strict-syntax? -- boolean (true when :command-syntax is :strict)
    :overrides      -- per-command :scope/:report-types overrides"
  [source-cfg]
  (let [commands       (common/resolve-commands-map source-cfg)
        strict-syntax? (= :strict (common/resolve-command-syntax source-cfg))]
    {:commands       commands
     :word-patterns  (compile-word-patterns strict-syntax? commands)
     :line-patterns  (compile-line-patterns strict-syntax?)
     :strict-syntax? strict-syntax?
     :overrides      (common/resolve-command-overrides source-cfg)}))

;; ---------------------------------------------------------------------------
;; Detection (pure)
;; ---------------------------------------------------------------------------

(defn- detect-close-reason [closed-words body-text strict-syntax?]
  (when (seq closed-words)
    (let [pattern (apply word-pattern false strict-syntax? closed-words)]
      (when-let [[_ matched] (re-find pattern body-text)]
        (get common/close-reasons matched :resolved)))))

(defn- parse-date-iso [s]
  (try
    (-> (LocalDate/parse s) (.atStartOfDay ZoneOffset/UTC) .toInstant Date/from)
    (catch Exception _
      (log/warn "Invalid ISO date in command:" s)
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
      (do (log/warn "Unparseable date/duration in command:" s)
          nil))))

(defn- match-words [patterns body-text]
  (into {} (keep (fn [[k p]] (when (re-find p body-text) [(keyword "report" (name k)) true]))) patterns))

(defn detect-words
  "Detect bareword command matches in `body-text` for a given
  `report-type`, using the precompiled vocabulary in `source-commands`."
  [report-type body-text {:keys [word-patterns commands strict-syntax? overrides]}]
  (when body-text
    (let [all-sets (match-words word-patterns body-text)
          ;; Pre-compute close-reason from unfiltered matches so it survives
          ;; any future refactoring of the filter step.
          reason   (when (:report/closed all-sets)
                     (detect-close-reason (:closed commands) body-text strict-syntax?))
          filtered (into {}
                        (keep (fn [[attr :as entry]]
                                (let [cmd (attr->word-cmd attr)
                                      rt  (or (:report-types (get overrides (:id cmd)))
                                              (:report-types cmd))]
                                  (when (or (nil? rt) (contains? rt report-type)) entry))))
                         all-sets)
          ;; Only attach close-reason when :report/closed survived filtering.
          result   (cond-> filtered
                     (and reason (:report/closed filtered))
                     (assoc :report/close-reason reason))]
      (when (seq result) result))))

(def ^:private compiled-lines-loose (compile-line-patterns false))

(defn detect-lines
  "Detect colon-line commands in `body-text`. The optional `compiled`
  arg provides the precompiled line patterns (from
  `build-source-commands`); if omitted, the loose-mode defaults are
  used."
  ([report-type body-text] (detect-lines report-type body-text nil nil compiled-lines-loose))
  ([report-type body-text overrides] (detect-lines report-type body-text overrides nil compiled-lines-loose))
  ([report-type body-text overrides email-date] (detect-lines report-type body-text overrides email-date compiled-lines-loose))
  ([report-type body-text overrides email-date compiled]
   (when body-text
     (let [lines (str/split-lines body-text)
           all-lines compiled]
       (->> lines
            (keep (fn [line]
                    (some (fn [[{:keys [id action attr _param scope report-types]} pattern]]
                            (let [rt (or (:report-types (get overrides id)) report-types)
                                  sc (or (:scope (get overrides id)) scope)]
                              (when (or (nil? rt) (contains? rt report-type))
                                (when-let [m (re-matches pattern line)]
                                  (let [first-capture (fn [] (some #(nth m % nil) (range 1 (count m))))
                                        mid-result    (fn [act] (when-let [mid (first-capture)]
                                                                  {:action act :attr attr
                                                                   :target-message-id (str "<" mid ">")}))
                                        date-result   (fn [act] (when-let [d (parse-date-or-duration (nth m 1) email-date)]
                                                                  {:action act :date d}))
                                        base (case action
                                               :set              (when-let [addr (first-capture)]
                                                                   {:action :set :attr attr :email-address addr})
                                               :unset            {:action :unset :attr attr}
                                               :set-deadline     (date-result :set-deadline)
                                               :unset-deadline   {:action :unset-deadline}
                                               :set-expiry       (date-result :set-expiry)
                                               :unset-expiry     {:action :unset-expiry}
                                               :set-topic        (when-let [t (nth m 1 nil)]
                                                                   {:action :set-topic :topic t})
                                               :unset-topic      {:action :unset-topic}
                                               :set-superseded   (mid-result :set-superseded)
                                               :unset-superseded (mid-result :unset-superseded)
                                               :set-supersedes   (mid-result :set-supersedes)
                                               :unset-supersedes (mid-result :unset-supersedes)
                                               :set-duplicate    (mid-result :set-duplicate)
                                               :unset-duplicate  (mid-result :unset-duplicate)
                                               :set-related      (mid-result :set-related)
                                               :unset-related    (mid-result :unset-related))]
                                    (when base (assoc base :scope sc :id id)))))))
                          all-lines)))
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

  :audience controls who the notifier will route the entry to:
  - :author      -- the address that sent the command (the default,
                     used for typo-class failures like `Superseded-by:`
                     with an unknown target).
  - :maintainers -- all maintainer subscribers on the source, so a
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
  "Fold a seq of parsed colon-line commands into a summary map.
  NOT for bareword results (which map attrs to `true`, not addresses)."
  [lines]
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
              ;; All relation actions carry a mid (the `:param :message-id`
              ;; in the registry guarantees the parser captured one).
              :set-superseded   (-> acc (assoc  :superseded-by target-message-id)
                                       (dissoc :unsuperseded-by? :unsuperseded-by-mid))
              :unset-superseded (-> acc (dissoc :superseded-by)
                                       (assoc  :unsuperseded-by? true
                                               :unsuperseded-by-mid target-message-id))
              :set-supersedes   (-> acc (assoc  :supersedes target-message-id)
                                       (dissoc :unsupersedes? :unsupersedes-mid))
              :unset-supersedes (-> acc (dissoc :supersedes)
                                       (assoc  :unsupersedes? true
                                               :unsupersedes-mid target-message-id))
              :set-duplicate    (-> acc (assoc  :duplicate-of target-message-id)
                                       (dissoc :unduplicate-of? :unduplicate-of-mid))
              :unset-duplicate  (-> acc (dissoc :duplicate-of)
                                       (assoc  :unduplicate-of? true
                                               :unduplicate-of-mid target-message-id))
              :set-related      (-> acc
                                    (update :related-to-set   (fnil conj #{}) target-message-id)
                                    (update :related-to-unset (fnil disj #{}) target-message-id))
              :unset-related    (-> acc
                                    (update :related-to-unset (fnil conj #{}) target-message-id)
                                    (update :related-to-set   (fnil disj #{}) target-message-id))))
          {:set {} :unset #{}}
          lines))

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

(defn- apply-vote!
  "Record a pre-detected `vote` (caller runs `detect-vote`)."
  [conn report-eid from-addr vote email delivery source-cfg]
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
        (log/info "Vote" (case vote :up "+1" :down "-1" "0") "by" from-addr)))))

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

(defn- build-line-set-tx
  "Build assertion datoms for setting attributes via -by lines.
  Points the attr to the real email and stores the designated address
  in its lowercased form so downstream comparisons against
  :email/author-address are case-insensitive."
  [report-eid email-eid set-map]
  (into []
        (mapcat (fn [[attr addr]]
                  [[:db/add report-eid attr email-eid]
                   [:db/add report-eid (address-attrs attr) (str/lower-case addr)]]))
        set-map))

(defn build-word-tx
  "Build transaction data for bareword detection results.
  `current` is the report's current state (pulled with
  `proxy-state-attrs`).  The `-address` cache is stored lowercased
  so downstream comparisons against :email/author-address are
  case-insensitive.
  Returns [tx-data new-sets] or nil if nothing to do."
  [report-eid word-result email-eid from-addr current]
  (let [close-reason (:report/close-reason word-result)
        ref-result   (dissoc word-result :report/close-reason)
        new-sets     (into {} (remove (fn [[k _]] (get current k))) ref-result)
        addr-lc      (some-> from-addr str/lower-case)
        all-tx       (cond-> (when (seq new-sets)
                               (into [(into {:db/id report-eid} (map (fn [[k _]] [k email-eid])) new-sets)]
                                     (map (fn [[k _]] [:db/add report-eid (address-attrs k) addr-lc]))
                                     new-sets))
                       (and close-reason (:report/closed new-sets))
                       (conj [:db/add report-eid :report/close-reason close-reason]))]
    (when (seq all-tx) [(vec all-tx) new-sets close-reason])))

(defn apply-words! [conn report-eid word-result email-eid email-mid from-addr source-cfg]
  (when word-result
    (let [db      (d/db conn)
          current (d/pull db proxy-state-attrs report-eid)
          rtype   (:report/type (d/pull db [:report/type] report-eid))]
      (when-let [[all-tx new-sets close-reason]
                 (build-word-tx report-eid word-result email-eid from-addr current)]
        (d/transact! conn all-tx)
        (tracking/bump-report-updated! conn report-eid)
        (log/info (str/join ", " (cond-> (mapv (comp name key) new-sets)
                                   close-reason (conj (str "close-reason:" (name close-reason)))))
                  (str "(by " email-mid ")"))
        ;; Propagate bareword-driven closure of a patch to the
        ;; bugs/requests it resolves (no successor in this path).
        ;; Sources with ":patch-triggers? false" opt out of the :resolved
        ;; propagation; :canceled retraction still runs (no-op when no
        ;; auto-credit was posed in the first place).
        (when (and (= :patch rtype) close-reason (:report/closed new-sets)
                   (or (not= :resolved close-reason)
                       (common/patch-triggers? source-cfg)))
          (rel/propagate-patch-closure! conn report-eid rtype email-eid
                                        close-reason nil))))))

(def ^:private line-pull-pattern
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

(def ^:private paired-line-attrs
  "Directives that share the same {pose-email ref + paired `-value` scalar}
  tx shape.  Each row maps [resolved-set-key resolved-unset-key attr]:
    - set-key   -- value in the resolved map that triggers an assertion
    - unset-key -- flag in the resolved map that triggers a retraction
    - attr      -- report attribute the pair targets"
  [[:deadline :undeadline? :report/deadline]
   [:expiry   :unexpiry?   :report/expiry]
   [:topic    :untopic?    :report/topic]])

(defn- build-paired-line-tx
  "Apply every set/unset from `paired-line-attrs` to `tx`, reading
  the value/flag pair from `resolved`."
  [tx report-eid email-eid current resolved]
  (reduce (fn [tx [set-k unset-k attr]]
            (cond-> tx
              (get resolved set-k)
              (into (set-ref-value-tx report-eid email-eid attr (get resolved set-k)))
              (get resolved unset-k)
              (into (retract-ref-value-tx report-eid current attr))))
          tx paired-line-attrs))

(defn- close-with-reason-tx
  "Tx datoms to close `report-eid` with `close-reason`.  The relation
  pose (:supersedes / :duplicates plus :related-to) is done separately
  by `apply-lines!` via bark.relations."
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

(def ^:private close-state-pull-pattern
  [:report/closed :report/closed-address :report/close-reason])

(defn- reopen-report!
  "Pull `report-eid`'s close state and transact a reopen iff it is
  closed.  Returns true when a reopen actually ran.  Used in the
  closure-relation flip and clear paths, where the report to reopen
  may differ from the email's anchor report."
  [conn report-eid]
  (let [pulled (d/pull (d/db conn) close-state-pull-pattern report-eid)
        tx     (reopen-tx report-eid pulled)]
    (when (seq tx)
      (d/transact! conn tx)
      true)))

(defn build-lines-tx
  "Build attribute-level transaction data from resolved commands and
  current report state.  Returns the tx vector (may be empty).
  Closure-relation effects (Superseded-by / Supersedes / Duplicate-of
  posing, close/reopen) are handled separately by `apply-lines!`."
  [report-eid email-eid resolved current]
  (let [{:keys [set unset]} resolved]
    (-> []
        (into (build-line-set-tx report-eid email-eid set))
        (cond-> (and (contains? set :report/closed)
                     (not (:report/close-reason current)))
          (conj [:db/add report-eid :report/close-reason :resolved]))
        (into (build-unset-tx report-eid current unset))
        (cond-> (and (contains? unset :report/closed) (:report/close-reason current))
          (conj [:db/retract report-eid :report/close-reason (:report/close-reason current)]))
        (build-paired-line-tx report-eid email-eid current resolved))))

(defn describe-lines
  "Build a human-readable summary of applied colon-line commands."
  [resolved]
  (let [{:keys [set unset deadline undeadline? expiry unexpiry?
                topic untopic?
                superseded-by unsuperseded-by? unsuperseded-by-mid
                supersedes    unsupersedes?    unsupersedes-mid
                duplicate-of  unduplicate-of?  unduplicate-of-mid
                related-to-set related-to-unset]} resolved]
    (str/join
     ", "
     (concat (map (fn [[attr addr]] (str (name attr) " -> " addr)) set)
             (map #(str "un-" (name %)) unset)
             (keep identity
                   [(when deadline         (str "deadline " deadline))
                    (when undeadline?      "no deadline")
                    (when expiry           (str "expiry " expiry))
                    (when unexpiry?        "no expiry")
                    (when topic            (str "topic:" topic))
                    (when untopic?         "no topic")
                    (when superseded-by    (str "superseded-by:" superseded-by))
                    (when unsuperseded-by? (str "not superseded-by:" unsuperseded-by-mid))
                    (when supersedes       (str "supersedes:" supersedes))
                    (when unsupersedes?    (str "not supersedes:" unsupersedes-mid))
                    (when duplicate-of     (str "duplicate-of:" duplicate-of))
                    (when unduplicate-of?  (str "not duplicate-of:" unduplicate-of-mid))])
             (map #(str "related-to:" %) related-to-set)
             (map #(str "not-related-to:" %) related-to-unset)))))

(defn- setter-address
  "Return the address credited as the setter of `attr` on `current`.
  For proxy-capable state attrs, reads the `-address` cache (which
  captures the designated setter in the `-by` case).  For all other
  setter-tracked attrs, follows the email ref and reads the pose
  email's :email/author-address."
  [current attr]
  (or (get current (address-attrs attr))
      (:email/author-address (get current attr))))

(defn- scope-permits?
  "Check whether a command scope permits `from-addr` to act on `attr`.
  `current-d` is a `delay` that pulls the report's current state -- it
  is only forced in the :setter-or-maintainer branch, so emails that
  contain only :user/:maintainer-scoped commands pay no pull cost.

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
    (do (log/warn "Unknown command scope on line:" scope)
        false)))

(defn- describe-denied-line
  "Rebuild a short, human-readable form of a parsed colon-line command
  for use in failure records and logs.  Dates are formatted as ISO
  (yyyy-MM-dd) so the failure record stays readable when rendered
  verbatim in a notification email.  Falls back to the :syntax of the
  looked-up command when the command carries no parameter."
  [{:keys [id action email-address date topic target-message-id]}]
  (let [cmd    (get commands-by-id id)
        syntax (or (:syntax cmd) (some-> id name))]
    (case action
      :set              (str syntax ": " email-address)
      :set-deadline     (str syntax ": " (common/format-date-iso date))
      :set-expiry       (str syntax ": " (common/format-date-iso date))
      :set-topic        (str syntax ": " topic)
      :set-superseded   (str syntax ": " target-message-id)
      :set-supersedes   (str syntax ": " target-message-id)
      :set-duplicate    (str syntax ": " target-message-id)
      :set-related      (str syntax ": " target-message-id)
      :unset-related    (str syntax ": " target-message-id)
      syntax)))

(defn- filter-permitted-lines
  "Return the subset of `lines` whose scope permits `from-addr` to
  act, further filtered by `action-pred`. `current-d` is the delay
  passed through to `scope-permits?` -- only forced when at least one
  line has scope :setter-or-maintainer.

  When `failure-ctx` is non-nil, lines that are rejected by the
  scope check (but pass `action-pred`) are written to the failures
  file as :insufficient-scope, audience :maintainers, so they
  surface in the next notification round."
  [lines current-d from-addr is-maintainer? action-pred failure-ctx]
  ;; Eager realization via `filterv` so the recording side effect in
  ;; the denial branch fires deterministically, regardless of whether
  ;; callers seq or reduce over the result.
  (filterv (fn [{:keys [scope attr action] :as line}]
             (and (action-pred action)
                  (or (scope-permits? scope attr from-addr is-maintainer? current-d)
                      (do (when failure-ctx
                            (record-failure!
                             (assoc failure-ctx
                                    :reason    :insufficient-scope
                                    :audience  :maintainers
                                    :command   (describe-denied-line line))))
                          false))))
           lines))

(defn- report-eid-by-mid
  "Look up a report by `target-mid`, matching either as the report's
  root (:report/message-id) or as a descendant email
  (:report/descendants -> :email/message-id).  Returns the report eid
  or nil.  Aligned with `digest/lookup-reports-by-mid`: a mid that
  points at a thread descendant should resolve to its containing
  report, not be reported as :unknown-target.  Returns nil for
  oversized mids -- such mids cannot have been stored."
  [db target-mid]
  (when (common/indexable-mid? target-mid)
    (or (d/entid db [:report/message-id target-mid])
        (d/q '[:find ?r . :in $ ?mid
               :where [?r :report/descendants ?e] [?e :email/message-id ?mid]]
             db target-mid))))

(defn- resolve-target
  "Look up the target report-eid for a relation command (Superseded-by:
  or Duplicate-of:).  Returns {:target-eid :target-type :valid?} where
  :valid? is true iff the target exists AND the type constraint passes
  for `kind`.  A `target-mid` that resolves to a descendant of a report
  resolves to the containing report (parity with threading)."
  [db kind report-eid source-type target-mid]
  (let [target-eid  (report-eid-by-mid db target-mid)
        target-type (when target-eid
                      (:report/type (d/pull db [:report/type] target-eid)))]
    {:target-eid  target-eid
     :target-type target-type
     :valid?      (and target-eid
                       (rel/valid-pose? kind report-eid target-eid
                                        source-type target-type))}))

(defn- record-target-failures!
  "Record a failure when a relation command's target lookup or
  validation didn't pan out.  Distinguishes three modes:
  - :unknown-target -- mid was given but no report matches.
  - :self-loop      -- target resolves to the current report itself
                        (a no-op self-reference, common when a new
                        bug filed as a reply names its own thread root).
  - :type-mismatch  -- target exists but the type constraint fails."
  [failure-ctx syntax target-mid target-eid valid? report-eid]
  (when failure-ctx
    (cond
      (and target-mid (nil? target-eid))
      (record-failure! (assoc failure-ctx
                              :reason :unknown-target
                              :audience :author
                              :command (str syntax ": " target-mid)))
      (and target-eid (= target-eid report-eid))
      (record-failure! (assoc failure-ctx
                              :reason :self-loop
                              :audience :author
                              :command (str syntax ": " target-mid)))
      (and target-eid (not valid?))
      (record-failure! (assoc failure-ctx
                              :reason :type-mismatch
                              :audience :author
                              :command (str syntax ": " target-mid))))))

(defn- apply-related-to!
  "Pose / retract :related-to relations from a resolved commands map.
  Independent of report closure state; called from both `apply-lines!`
  (open path) and `try-unclosed!` (closed path).  Records failures
  for unknown targets and self-loops.

  Target mids resolve via `report-eid-by-mid` -- root or descendant --
  so a mid pointing at any email in a report's thread reaches that
  report (parity with threading)."
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
      (let [target-eid (report-eid-by-mid db mid)]
        (cond
          (nil? target-eid)
          (when failure-ctx
            (record-failure! (assoc failure-ctx
                                    :reason   :unknown-target
                                    :audience :author
                                    :command  (str "Related-to: " mid))))
          (= report-eid target-eid)
          (do (log/warn (str "Related-to: " mid
                             " -- targets the same report (self-loop) -- ignored"))
              (when failure-ctx
                (record-failure! (assoc failure-ctx
                                        :reason   :self-loop
                                        :audience :author
                                        :command  (str "Related-to: " mid)))))
          :else
          (do (rel/pose-if-absent! conn {:from-eid  report-eid
                                         :to-eid    target-eid
                                         :kind      :related-to
                                         :setter    from-addr :email-eid email-eid
                                         :posed-at  posed-at  :value nil})
              (tracking/bump-report-updated! conn target-eid)
              (log/info "Related-to:" mid "(by" from-addr ")")))))
    (doseq [mid to-clear]
      (let [target-eid (report-eid-by-mid db mid)]
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

(defn- relation-source-eid
  "Eid of the source report of an active incoming relation of `kind`
  pointing at `report-eid`, or nil.  Mirror of `relation-target-eid`."
  [db report-eid kind]
  (d/q '[:find ?from . :in $ ?to ?kind
         :where
         [?e :rel/from ?from]
         [?e :rel/kind ?kind]
         [?e :rel/active? true]
         [?e :rel/to ?to]]
       db report-eid kind))

(defn- relation-counterparty-eid
  "Eid of the other endpoint of an active `kind` relation involving
  `report-eid`.  For :current-as-from, returns the relation's :rel/to;
  for :current-as-to, returns the relation's :rel/from."
  [db report-eid kind role]
  (case role
    :current-as-from (relation-target-eid db report-eid kind)
    :current-as-to   (relation-source-eid db report-eid kind)))

(defn- relation-setter
  ":rel/setter address of the active `kind` relation involving
  `report-eid`, or nil.  `role` controls which side `report-eid` is on."
  [db report-eid kind role]
  (case role
    :current-as-from (d/q '[:find ?setter . :in $ ?from ?kind
                            :where
                            [?e :rel/from ?from]
                            [?e :rel/kind ?kind]
                            [?e :rel/active? true]
                            [?e :rel/setter ?setter]]
                          db report-eid kind)
    :current-as-to   (d/q '[:find ?setter . :in $ ?to ?kind
                            :where
                            [?e :rel/to ?to]
                            [?e :rel/kind ?kind]
                            [?e :rel/active? true]
                            [?e :rel/setter ?setter]]
                          db report-eid kind)))

(defn- relation-setters-as-pull
  "Build a partial pull map exposing relation setters under :setter-attr
  so `scope-permits?` can resolve :setter-or-maintainer on relation
  unset lines.  `rows` is a seq carrying :kind, :role and
  :setter-attr.

  Batches the per-row d/q calls into at most two queries (one per role)
  to avoid N+1 lookups when several rows share a role."
  [db report-eid rows]
  (let [by-role      (group-by :role rows)
        from-kinds   (some->> (get by-role :current-as-from) seq (map :kind) vec)
        to-kinds     (some->> (get by-role :current-as-to) seq (map :kind) vec)
        from-setters (when from-kinds
                       (into {} (d/q '[:find ?kind ?setter
                                       :in $ ?from [?kind ...]
                                       :where
                                       [?e :rel/from ?from]
                                       [?e :rel/kind ?kind]
                                       [?e :rel/active? true]
                                       [?e :rel/setter ?setter]]
                                     db report-eid from-kinds)))
        to-setters   (when to-kinds
                       (into {} (d/q '[:find ?kind ?setter
                                       :in $ ?to [?kind ...]
                                       :where
                                       [?e :rel/to ?to]
                                       [?e :rel/kind ?kind]
                                       [?e :rel/active? true]
                                       [?e :rel/setter ?setter]]
                                     db report-eid to-kinds)))]
    (into {}
          (keep (fn [{:keys [kind role setter-attr]}]
                  (when-let [s (case role
                                 :current-as-from (get from-setters kind)
                                 :current-as-to   (get to-setters kind))]
                    [setter-attr {:email/author-address s}])))
          rows)))

(def ^:private closure-relation-rows
  "Specs for command-driven closure relations (Superseded-by /
  Supersedes / Duplicate-of).  Each row drives an iteration in
  `apply-lines!`:
    :id            -- unique row id (matches the registry command :id)
    :kind          -- relation kind to pose
    :role          -- :current-as-from when the current report is the
                      one being closed (Superseded-by:, Duplicate-of:);
                      :current-as-to when the current report is the
                      replacement and the target is the one being closed
                      (Supersedes:).  The :rel/from of the posed
                      relation is always the report being closed.
    :propagate     -- close-reason passed to propagate-patch-closure!
    :propagate-tgt -- when true, pass the replacement eid as `successor-eid`
    :syntax        -- human-readable command name (for failure logs)
    :mid-key       -- key in resolved holding the target message-id
    :unset-key     -- key in resolved holding the unset flag
    :setter-attr   -- key under which the relation's :rel/setter is
                      surfaced in the pull map so `scope-permits?` can
                      resolve :setter-or-maintainer on the unset
                      command in the open-report path."
  ;; :setter-attr must be UNIQUE per row.  Both :supersedes rows share
  ;; the `:rel/supersedes` schema kind; if we reused `:rel/supersedes`
  ;; as the pull-map key, `relation-setters-as-pull` would overwrite
  ;; the from-setter with the to-setter on a chained report (one that
  ;; supersedes X AND is superseded by Y), so the scope check on the
  ;; `Not superseded-by:` unset would see the wrong setter.  The
  ;; suffix `-from` / `-to` records the row's role.  These keys are
  ;; internal to the pull map; nothing else reads them as schema attrs.
  [{:id :superseded-by :kind :supersedes :role :current-as-from
    :propagate :superseded :propagate-tgt true
    :syntax "Superseded-by"
    :mid-key :superseded-by :unset-key :unsuperseded-by? :unset-mid-key :unsuperseded-by-mid
    :setter-attr :rel/supersedes-from}
   {:id :supersedes :kind :supersedes :role :current-as-to
    :propagate :superseded :propagate-tgt true
    :syntax "Supersedes"
    :mid-key :supersedes :unset-key :unsupersedes? :unset-mid-key :unsupersedes-mid
    :setter-attr :rel/supersedes-to}
   {:id :duplicate-of :kind :duplicates :role :current-as-from
    :propagate :canceled :propagate-tgt false
    :syntax "Duplicate-of"
    :mid-key :duplicate-of :unset-key :unduplicate-of? :unset-mid-key :unduplicate-of-mid
    :setter-attr :rel/duplicates-from}])

(defn- compute-closure-rows
  "Enrich `closure-relation-rows` with per-row decisions derived from
  `resolved` and current DB state.  Each row gains:
    :target-mid       -- string from `resolved`'s mid-key (or nil)
    :resolved         -- {:target-eid :target-type :valid?} from `resolve-target`
    :pose-from        -- eid that becomes :rel/from on the pose (= the
                         report being closed; either current or target
                         depending on role)
    :pose-to          -- eid that becomes :rel/to on the pose (the replacement)
    :counterparty     -- eid of the other endpoint of the currently-active
                         relation involving the current report (or nil)
    :unset-target-mid -- string from `resolved`'s unset-mid-key (or nil)
    :unset-target-eid -- eid the unset's mid resolves to (or nil)
    :clear-now?       -- true when an explicit unset command targets the
                         exact mid of the currently-active relation.
                         A mismatch (or absent active relation) is a
                         no-op so other relations of the same kind
                         posed from elsewhere are not collateral damage."
  [db report-eid source-type resolved]
  (mapv (fn [{:keys [kind role unset-key unset-mid-key mid-key] :as row}]
          (let [target-mid (get resolved mid-key)
                r          (resolve-target db kind report-eid
                                            source-type target-mid)
                tgt        (:target-eid r)
                [pose-from pose-to] (case role
                                      :current-as-from [report-eid tgt]
                                      :current-as-to   [tgt report-eid])
                counterparty     (relation-counterparty-eid db report-eid kind role)
                unset-target-mid (get resolved unset-mid-key)
                unset-target-eid (when unset-target-mid
                                   (report-eid-by-mid db unset-target-mid))]
            (assoc row
                   :target-mid       target-mid
                   :resolved         r
                   :pose-from        pose-from
                   :pose-to          pose-to
                   :counterparty     counterparty
                   :unset-target-mid unset-target-mid
                   :unset-target-eid unset-target-eid
                   :clear-now?       (and (boolean (get resolved unset-key))
                                          (some? counterparty)
                                          (= unset-target-eid counterparty)))))
        closure-relation-rows))

(defn- apply-closure-set-row!
  "Process one valid closure-relation set command.  Steps:
  1. Last-write-wins flip: if a relation of `kind` is active in the
     reversed direction, retract its :supersedes pair (the :related-to
     companion is symmetric and stays active -- the two reports remain
     related either way) and reopen pose-to (which was the :rel/from
     of the inverse, hence previously closed).
  2. Close pose-from with the row's propagate close-reason.
  3. Pose the new closure relation + :related-to companion.
  4. Propagate the patch closure when pose-from is a patch."
  [conn {:keys [kind resolved target-mid pose-from pose-to
                propagate propagate-tgt]}
   email-eid from-addr posed-at source-type]
  (when (rel/active-inverse-relation (d/db conn) pose-from pose-to kind)
    (rel/retract-pair! conn pose-to kind pose-from email-eid)
    (reopen-report! conn pose-to)
    (tracking/bump-report-updated! conn pose-to))
  (d/transact! conn (close-with-reason-tx pose-from email-eid from-addr propagate))
  (let [opts {:from-eid pose-from :to-eid pose-to
              :setter from-addr :email-eid email-eid
              :posed-at posed-at}]
    (rel/pose-if-absent! conn (assoc opts :kind kind :value target-mid))
    (rel/pose-if-absent! conn (assoc opts :kind :related-to :value nil)))
  (tracking/bump-report-updated! conn pose-from)
  (tracking/bump-report-updated! conn pose-to)
  (when (= :patch source-type)
    (rel/propagate-patch-closure!
     conn pose-from :patch email-eid
     propagate (when propagate-tgt pose-to))))

(defn- apply-closure-clear-row!
  "Process one closure-relation unset command that has a matching
  active relation.  Retracts the :supersedes pair AND the :related-to
  companion, then reopens the previously-closed party.  Which side is
  current vs counterparty depends on :role."
  [conn report-eid {:keys [kind role counterparty]} email-eid]
  (case role
    :current-as-from
    (do (rel/retract-by-from! conn report-eid kind email-eid)
        (when counterparty
          (rel/retract-pair! conn report-eid :related-to counterparty email-eid))
        (reopen-report! conn report-eid)
        (tracking/bump-report-updated! conn report-eid)
        (when counterparty (tracking/bump-report-updated! conn counterparty)))
    :current-as-to
    (do (rel/retract-by-to! conn report-eid kind email-eid)
        (when counterparty
          (rel/retract-pair! conn counterparty :related-to report-eid email-eid)
          (reopen-report! conn counterparty)
          (tracking/bump-report-updated! conn counterparty))
        (tracking/bump-report-updated! conn report-eid))))

(defn apply-lines! [conn report-eid lines email-eid from-addr is-maintainer?
                    failure-ctx]
  (let [db         (d/db conn)
        ;; Surface relation setters under their :setter-attr so the
        ;; scope check on :unsuperseded-by / :unsupersedes / :unduplicate-of
        ;; works on the open-report path (mirrors the same trick in
        ;; `try-unclosed!`).
        current-d  (delay (merge (d/pull db line-pull-pattern report-eid)
                                 (relation-setters-as-pull db report-eid
                                                           closure-relation-rows)))
        permitted  (filter-permitted-lines lines current-d from-addr
                                           is-maintainer? (constantly true)
                                           failure-ctx)]
    (when (seq permitted)
      (let [current     @current-d
            resolved    (resolve-commands permitted)
            source-type (:report/type current)
            rows        (compute-closure-rows db report-eid source-type resolved)
            valid-rows  (filterv (comp :valid? :resolved) rows)
            clear-rows  (filterv :clear-now? rows)
            attr-tx     (build-lines-tx report-eid email-eid resolved current)
            posed-at    (Date.)
            did-work?   (or (seq attr-tx) (seq valid-rows) (seq clear-rows))]
        (when (seq attr-tx)
          (d/transact! conn attr-tx))
        (doseq [row valid-rows]
          (apply-closure-set-row! conn row email-eid from-addr posed-at source-type))
        (doseq [row clear-rows]
          (apply-closure-clear-row! conn report-eid row email-eid))
        (when did-work?
          (tracking/bump-report-updated! conn report-eid)
          (doseq [{:keys [resolved]} rows
                  :let [t (:target-eid resolved)]
                  :when t]
            (tracking/bump-report-updated! conn t))
          (log/info "Commands:"
                    (describe-lines resolved)
                    (str "(by " from-addr ")")))
        (doseq [{:keys [resolved syntax target-mid]} rows
                :let [tgt-eid (:target-eid resolved)
                      valid?  (:valid? resolved)]]
          (record-target-failures! failure-ctx syntax target-mid tgt-eid valid? report-eid)
          (when (and tgt-eid (not valid?))
            (if (= tgt-eid report-eid)
              (log/warn (str syntax ": " target-mid
                             " -- targets the same report (self-loop) -- ignored"))
              (log/warn (str syntax ": type mismatch -- source")
                        source-type "vs target" (:target-type resolved)))))
        (apply-related-to! conn report-eid resolved email-eid from-addr failure-ctx)))))

(def ^:private unclose-relation-rows
  "Subset of `closure-relation-rows` used by `try-unclosed!` to retract
  a closure relation on a closed report.  Filters to :current-as-from
  rows only (rows where current can ever be the closed party).
  Derived to keep both row sets in lockstep when a new kind is added."
  (into []
        (comp (filter #(= :current-as-from (:role %)))
              (map #(select-keys % [:id :kind :role :unset-key :unset-mid-key
                                    :setter-attr])))
        closure-relation-rows))

(defn- try-unclosed!
  "If a closed report has a Not closed / Not superseded-by / Not duplicate-of
  line, retract the closure (and the relation if any).  Explicit unsets
  must name the exact mid of the currently-active relation; a mismatch
  is a no-op (so an unrelated relation of the same kind is not collateral
  damage).  `Not closed.` retracts whatever closure relation drove the
  closure, regardless of mid."
  [conn report-eid lines email-eid is-maintainer? from-addr failure-ctx]
  (let [db          (d/db conn)
        current-d   (delay (merge (d/pull db close-state-pull-pattern report-eid)
                                  (relation-setters-as-pull
                                   db report-eid unclose-relation-rows)))
        permitted   (filter-permitted-lines
                     lines current-d from-addr is-maintainer?
                     #{:unset :unset-superseded :unset-duplicate
                       :set-related :unset-related} failure-ctx)
        resolved    (resolve-commands permitted)
        unset       (:unset resolved)
        unset-closed? (contains? unset :report/closed)]
    (apply-related-to! conn report-eid resolved email-eid from-addr failure-ctx)
    (when (or unset-closed? (:unsuperseded-by? resolved) (:unduplicate-of? resolved))
      (let [current  @current-d
            ;; For each row: target-eid + whether to clear this relation.
            ;; Explicit `Not X-by: <mid>` requires the mid to match the
            ;; active counterparty.  Implicit `Not closed` retracts whatever
            ;; is active (the closure was driven by exactly that relation).
            rows     (mapv (fn [{:keys [kind unset-key unset-mid-key] :as row}]
                             (let [target (relation-target-eid db report-eid kind)
                                   explicit-unset? (boolean (get resolved unset-key))
                                   unset-mid       (get resolved unset-mid-key)
                                   unset-eid       (when unset-mid
                                                     (report-eid-by-mid db unset-mid))]
                               (assoc row
                                      :target target
                                      :unset-target-mid unset-mid
                                      :unset-target-eid unset-eid
                                      :clear? (or (and explicit-unset?
                                                       target
                                                       (= unset-eid target))
                                                  (and unset-closed? target)))))
                           unclose-relation-rows)
            any-clear?     (some :clear? rows)
            ;; Reopen only when we will actually retract a relation OR when
            ;; the email said "Not closed." outright.  An explicit unset whose
            ;; mid does not match any active relation is a no-op -- the
            ;; report stays closed.
            should-reopen? (or unset-closed? any-clear?)
            attr-tx        (when should-reopen?
                             (-> []
                                 (into (build-unset-tx report-eid current #{:report/closed}))
                                 (cond-> (:report/close-reason current)
                                   (conj [:db/retract report-eid :report/close-reason
                                          (:report/close-reason current)]))))]
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
                         (cond (:unsuperseded-by? resolved) "not superseded-by"
                               (:unduplicate-of?  resolved) "not duplicate-of"
                               :else                        "not closed")
                         " (by " from-addr ")")))))))

(defn- word-scope-permits?
  "Scope check for bareword commands.

  Barewords always *set* an attribute, so the scope values behave as
  follows:
  - :user                 -- anyone
  - :maintainer           -- maintainer only
  - :setter-or-maintainer -- equivalent to :user here; the sender of
                            the bareword IS the setter.  The validator
                            rejects this value on barewords, so this
                            branch only fires for configs that bypass
                            validation.  We fall back to :user silently
                            rather than logging on every incoming
                            bareword.

  Truly unknown scopes (e.g. typos that also bypass validation) are
  rejected with a warning so they surface in the logs."
  [scope is-maintainer?]
  (case scope
    :user                 true
    :setter-or-maintainer true
    :maintainer           (boolean is-maintainer?)
    (do (log/warn "Unknown command scope on bareword:" scope)
        false)))

(defn- describe-denied-word
  "Short, human-readable form of a bareword refused by scope -- always
  the canonical English command word (\"Closed.\", \"Acked.\", ...)
  derived from the command id.  Source-level word overrides aren't
  reflected: the goal is a stable label in failure logs, not a
  round-trip of the user's exact input."
  [attr]
  (some-> (attr->word-cmd attr) :id name str/capitalize (str ".")))

(defn- filter-words-by-scope
  "Filter `word-result` to the subset allowed by the effective scope
  for each bareword.  When `failure-ctx` is non-nil, barewords that fail
  the scope check are recorded as :insufficient-scope failures with
  \":audience :maintainers\", so denied attempts surface to maintainer
  subscribers via the notification loop."
  [word-result overrides is-maintainer? failure-ctx]
  (when word-result
    (let [filtered (into {}
                         (keep (fn [[attr :as entry]]
                                 (let [cmd   (attr->word-cmd attr)
                                       scope (or (:scope (get overrides (:id cmd))) (:scope cmd))]
                                   (if (word-scope-permits? scope is-maintainer?)
                                     entry
                                     (do (when failure-ctx
                                           (record-failure!
                                            (assoc failure-ctx
                                                   :reason   :insufficient-scope
                                                   :audience :maintainers
                                                   :command  (describe-denied-word attr))))
                                         nil)))))
                         (dissoc word-result :report/close-reason))]
      (when (seq filtered)
        (cond-> filtered
          (:report/close-reason word-result) (assoc :report/close-reason
                                                    (:report/close-reason word-result)))))))

(def carrier-eligible-ids
  "Command ids whose intent is unambiguously cross-report: when carried
  by a mail that also creates a new report, they apply to that new
  report rather than the thread parent.  Restricted to two relation
  annotations with explicit external mids:
  - :supersedes  -- 'this new report supersedes <old>'
  - :related-to  -- 'this new report is related to <other>'

  Excluded by design:
  - :superseded-by / :duplicate-of (no one opens a new report just to
    declare it's superseded or a duplicate);
  - state-change triggers (close/ack/own) -- ambiguous on a brand-new
    report;
  - non-relation annotations (urgent/important/topic/deadline/expiry)
    -- the intent is usually 'this thread', not the new report.

  Unsets (:unsupersedes, :unrelated-to) are intentionally NOT in the
  carrier set: there is nothing to undo on a freshly-created report."
  #{:supersedes :related-to})

(defn apply-commands!
  "Apply commands from `email` against `report-eid`.
  A reply shipping patch content also fires an implicit `Acked. Owned.`,
  gated by :patch-triggers? and report-type ∈ #{:bug :request}.
  Returns true if anything was applied.

  `line-filter` is one of:
    nil           -- process every command, no filtering.
    :carrier-only -- process ONLY lines whose id is in
                     `carrier-eligible-ids`; skip words, votes,
                     and implicit ack/own.  Used when this email
                     is a reply that also creates a new report:
                     Supersedes:/Related-to: in its body apply to
                     the new report unambiguously, the rest goes
                     to the thread parent via a separate call.
    :no-carrier   -- process everything EXCEPT carrier-eligible
                     lines.  Used for the thread-parent call in the
                     same scenario, so carrier lines are not
                     double-applied."
  [conn report-eid report-type email source-map roles delivery line-filter]
  (let [carrier-only? (= :carrier-only line-filter)
        no-carrier?   (= :no-carrier   line-filter)
        body-text     (common/email-body-text email)
        db            (d/db conn)
        from-addr     (:email/author-address email)
        eid           (:db/id email)
        report-mid    (:report/message-id (d/entity db report-eid))
        src-name      (d/q '[:find ?src . :in $ ?rid
                             :where [?rid :report/email ?e] [?e :email/source ?src]] db report-eid)
        source-cfg    (when-let [cfg (get source-map src-name)]
                        (periods/source-cfg-at-date cfg (:email/date-sent email)))
        src-cmds      (build-source-commands source-cfg)
        overrides     (:overrides src-cmds)
        is-maint?     (common/maintainer? roles from-addr (:email/date-sent email))
        fail-ctx      (when (and from-addr src-name)
                        {:source     src-name
                         :from-addr  from-addr
                         :email-date (:email/date-sent email)
                         :report-mid report-mid})
        ;; In :carrier-only mode we don't credit anyone on the brand-
        ;; new report -- words, votes, implicit ack/own all go to the
        ;; thread parent through the sibling :no-carrier call.
        body-words    (when (and body-text (not carrier-only?))
                        (detect-words report-type body-text src-cmds))
        implicit      (when (and (not carrier-only?)
                                 (contains? #{:bug :request} report-type)
                                 (common/patch-triggers? source-cfg)
                                 (:email/in-reply-to email)
                                 (detect/has-patch-content? email))
                        {:report/acked true :report/owned true})
        reporter      (some-> (d/pull db [{:report/email [:email/author-address]}]
                                      report-eid)
                              :report/email :email/author-address str/lower-case)
        self-ack?     (and from-addr reporter (= (str/lower-case from-addr) reporter))
        word-result   (cond-> (merge implicit body-words)
                        self-ack? (dissoc :report/acked)
                        :always   (filter-words-by-scope overrides is-maint? fail-ctx))
        keep-line?    (case line-filter
                        :carrier-only #(contains? carrier-eligible-ids (:id %))
                        :no-carrier   #(not (contains? carrier-eligible-ids (:id %)))
                        (constantly true))
        lines         (when body-text
                        (->> (detect-lines report-type body-text overrides
                                           (:email/date-sent email)
                                           (:line-patterns src-cmds))
                             (filter keep-line?)
                             (remove (fn [d]
                                       (and (= :set (:action d))
                                            (= :report/acked (:attr d))
                                            reporter
                                            (= (some-> (:email-address d) str/lower-case)
                                               reporter))))
                             vec))
        closed?       (some? (:report/closed (d/pull db [:report/closed] report-eid)))]
    (if closed?
      (do (when (seq lines)
            (try-unclosed! conn report-eid lines eid is-maint? from-addr fail-ctx))
          (boolean (seq lines)))
      (let [voted? (when (and (not carrier-only?) body-text)
                     (when-let [vote (and (= :request report-type) from-addr
                                          (detect-vote body-text))]
                       (apply-vote! conn report-eid from-addr vote email delivery source-cfg)
                       true))]
        (apply-words! conn report-eid word-result eid (:email/message-id email) from-addr
                      source-cfg)
        (apply-lines! conn report-eid lines eid from-addr is-maint? fail-ctx)
        (boolean (or (seq word-result) (seq lines) voted?))))))
