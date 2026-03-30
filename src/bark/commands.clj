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
            [bark.tracking :as tracking]
            [bark.roles :as roles])
  (:import [java.time LocalDate ZoneOffset]
           [java.util Date]))

;; ---------------------------------------------------------------------------
;; Trailing punctuation
;; ---------------------------------------------------------------------------

(def trailing-punct "[.,;:?!]")

;; ---------------------------------------------------------------------------
;; Command registry
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
   ;; Proxy directives
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
   ;; Unset directives
   {:id :unacked     :kind :directive :action :unset :attr :report/acked    :scope :maintainer
    :syntax "Not acked" :report-types #{:bug :patch :request}}
   {:id :unowned     :kind :directive :action :unset :attr :report/owned    :scope :maintainer
    :syntax "Not owned" :report-types #{:bug :patch :request}}
   {:id :unclosed    :kind :directive :action :unset :attr :report/closed   :scope :maintainer :syntax "Not closed"}
   {:id :unurgent    :kind :directive :action :unset :attr :report/urgent   :scope :maintainer :syntax "Not urgent"}
   {:id :unimportant :kind :directive :action :unset :attr :report/important :scope :maintainer :syntax "Not important"}
   ;; Deadline / topic
   {:id :deadline    :kind :directive :action :set-deadline   :attr :report/deadline :scope :maintainer
    :syntax "Deadline" :param :date-or-duration :report-types #{:bug :patch :request}}
   {:id :undeadline  :kind :directive :action :unset-deadline :attr :report/deadline :scope :maintainer
    :syntax "No deadline" :report-types #{:bug :patch :request}}
   {:id :expiry      :kind :directive :action :set-expiry   :attr :report/expiry :scope :maintainer
    :syntax "Expiry" :param :date-or-duration :report-types #{:bug :patch :request}}
   {:id :unexpiry    :kind :directive :action :unset-expiry :attr :report/expiry :scope :maintainer
    :syntax "No expiry" :report-types #{:bug :patch :request}}
   {:id :topic       :kind :directive :action :set-topic :attr :report/topic :scope :maintainer
    :syntax "Topic" :param :word}
   {:id :untopic     :kind :directive :action :unset-topic :attr :report/topic :scope :maintainer
    :syntax "No topic"}
   ;; Supersede
   {:id :superseded-by  :kind :directive :action :set-superseded :attr :report/superseded-by :scope :maintainer
    :syntax "Superseded-by" :param :message-id}
   {:id :unsuperseded   :kind :directive :action :unset-superseded :attr :report/superseded-by :scope :maintainer
    :syntax "Not superseded"}])

;; Derived indexes
(def trigger-commands  (filterv #(= :trigger  (:kind %)) commands))
(def directive-commands (filterv #(= :directive (:kind %)) commands))

(def commands-by-id (into {} (map (juxt :id identity)) commands))

(def attr->trigger-cmd
  (into {} (map (juxt :attr identity)) trigger-commands))

(def state-attrs
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

(defn- trigger-pattern [& words]
  (re-pattern
   (str "(?m)^(" (str/join "|" (map #(java.util.regex.Pattern/quote %) words))
        ")(?:" trailing-punct "|$)")))

(defn- directive-pattern [{:keys [syntax param]}]
  (let [qs (java.util.regex.Pattern/quote syntax)]
    (re-pattern
     (case param
       :email-address (str "^" qs ":\\s+(\\S+@\\S+)" trailing-punct "?\\s*$")
       :date          (str "^" qs ":\\s+(\\d{4}-\\d{2}-\\d{2})" trailing-punct "?\\s*$")
       :date-or-duration (str "^" qs ":\\s+(\\d{4}-\\d{2}-\\d{2}|\\d+[dwmy](?:\\s+\\d+[dwmy])*)" trailing-punct "?\\s*$")
       :word          (str "^" qs ":\\s+([a-zA-Z0-9_-]+)" trailing-punct "?\\s*$")
       :message-id    (str "^" qs ":\\s+<?([^<>\\s]+@[^<>\\s]+)>?" trailing-punct "?\\s*$")
       (str "^" qs trailing-punct "?\\s*$")))))

(defn- compile-trigger-words [action-map]
  (update-vals action-map #(apply trigger-pattern %)))

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
                        ")(?:" trailing-punct "|$)"))]
      (when-let [[_ matched] (re-find pattern body-text)]
        (get common/close-reasons matched :resolved)))))

(defn- parse-date-iso [s]
  (try
    (-> (LocalDate/parse s) (.atStartOfDay ZoneOffset/UTC) .toInstant Date/from)
    (catch Exception _ nil)))

(defn- parse-date-or-duration
  "Parse a YYYY-MM-DD date string or a duration like '2d', '3w', '1m 2w'.
  Durations are resolved to an absolute date relative to `as-of` (a
  java.util.Date, typically the email's date-sent).  Falls back to today
  when `as-of` is nil."
  [s as-of]
  (if (re-matches #"\d{4}-\d{2}-\d{2}" s)
    (parse-date-iso s)
    (when-let [days (common/parse-delay s)]
      (let [base (if as-of
                   (-> as-of ^Date .toInstant (LocalDate/ofInstant ZoneOffset/UTC))
                   (LocalDate/now ZoneOffset/UTC))]
        (-> base (.plusDays days) (.atStartOfDay ZoneOffset/UTC) .toInstant Date/from)))))

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
                                               :set            (when-let [addr (nth m 1 nil)]
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
                                    (when base (assoc base :scope sc)))))))
                          all-directives)))
            vec)))))

;; ---------------------------------------------------------------------------
;; Command failure recording (file-based)
;; ---------------------------------------------------------------------------

(def ^:private failures-file "public/.failures.edn")
(def ^:private max-failure-age-ms (* 365 24 60 60 1000))

(defn- load-failures []
  (let [f (io/file failures-file)]
    (if (.exists f)
      (try (edn/read-string (slurp f)) (catch Exception _ []))
      [])))

(defn- save-failures! [failures]
  (io/make-parents failures-file)
  (spit failures-file (pr-str failures)))

(defn record-failure!
  "Append a command failure to the failures file for later notification.
  Prunes entries older than 1 year."
  [{:keys [source from-addr email-date reason command report-mid]}]
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
                  :report-mid (or report-mid "")}]
    (save-failures! (conj pruned entry))
    (log/info "Command failure:" reason command "from" from-addr)))

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

(defn find-or-create-synthetic-email! [conn addr report-message-id attr-name]
  (let [synthetic-mid (str "<bark-synthetic-" (name attr-name) "-"
                           addr "-" report-message-id ">")
        existing      (d/q '[:find ?e . :in $ ?mid :where [?e :email/message-id ?mid]]
                           (d/db conn) synthetic-mid)]
    (or existing
        (let [tempid -1
              tx     (d/transact! conn [{:db/id          tempid
                                         :email/message-id   synthetic-mid
                                         :email/from-address addr
                                         :email/date-sent    (Date.)
                                         :email/subject      (str "Synthetic: " (name attr-name)
                                                                   " for " report-message-id)}])]
          (get (:tempids tx) tempid)))))

(defn- apply-vote! [conn report-eid from-addr body-text email delivery source-cfg]
  (when-let [vote (detect-vote body-text)]
    (if-not (common/sent-via-source-channel? delivery source-cfg)
      (log/info "Vote ignored (private email on public source)" from-addr)
      (let [report-mid (d/q '[:find ?mid . :in $ ?r :where [?r :report/message-id ?mid]]
                            (d/db conn) report-eid)
            vote-key   (str report-mid ":" from-addr)]
        ;; :vote/key has :db.unique/identity — if this voter already voted,
        ;; the upsert overwrites silently (first vote wins in practice,
        ;; since we only transact when key is new).
        (when-not (d/q '[:find ?v . :in $ ?k :where [?v :vote/key ?k]]
                       (d/db conn) vote-key)
          (d/transact! conn [{:vote/key    vote-key
                              :vote/report report-eid
                              :vote/email  (:db/id email)
                              :vote/value  vote
                              :vote/voter  from-addr}])
          (tracking/bump-report-updated! conn report-eid)
          (log/info "Vote" (case vote :up "+1" :down "-1" "0") "by" from-addr))))))

(defn- build-unset-tx
  "Build retraction datoms for unsetting attributes and their proxies."
  [report-eid current attrs]
  (into []
        (mapcat (fn [attr]
                  (let [cur       (get current attr)
                        proxy-cur (get current (proxy-attrs attr))]
                    (cond-> []
                      cur       (conj [:db/retract report-eid attr (ref-eid cur)])
                      proxy-cur (conj [:db/retract report-eid (proxy-attrs attr) (ref-eid proxy-cur)])))))
        attrs))

(defn- build-directive-set-tx
  "Build assertion datoms for setting attributes via proxy directives.
  Creates synthetic emails and sets both the attribute and its -proxy counterpart."
  [conn report-eid report-mid email-eid set-map]
  (into []
        (mapcat (fn [[attr addr]]
                  (let [target-eid (find-or-create-synthetic-email! conn addr report-mid attr)]
                    [[:db/add report-eid attr target-eid]
                     [:db/add report-eid (proxy-attrs attr) email-eid]])))
        set-map))

(defn apply-triggers! [conn report-eid trig-result email-eid email-mid]
  (when trig-result
    (let [close-reason (:report/close-reason trig-result)
          ref-result   (dissoc trig-result :report/close-reason)
          current      (d/pull (d/db conn) state-attrs report-eid)
          new-sets     (into {} (remove (fn [[k _]] (get current k))) ref-result)
          all-tx       (cond-> (when (seq new-sets)
                                 [(into {:db/id report-eid} (map (fn [[k _]] [k email-eid])) new-sets)])
                         (and close-reason (:report/closed new-sets))
                         (conj [:db/add report-eid :report/close-reason close-reason]))]
      (when (seq all-tx)
        (d/transact! conn (vec all-tx))
        (tracking/bump-report-updated! conn report-eid)
        (log/info (str/join ", " (cond-> (mapv (comp name key) new-sets)
                                   close-reason (conj (str "close-reason:" (name close-reason)))))
                  (str "(by " email-mid ")"))))))

(defn apply-directives! [conn report-eid directives email-eid from-addr is-maintainer?
                         failure-ctx]
  (let [permitted (filter (fn [{:keys [scope]}]
                            (or (= :user scope) (and (= :maintainer scope) is-maintainer?)))
                          directives)]
    (when (seq permitted)
      (let [db      (d/db conn)
            {:keys [set unset deadline undeadline? expiry unexpiry?
                    topic untopic? superseded-by unsuperseded?]}
            (resolve-commands permitted)
            report-mid (d/q '[:find ?mid . :in $ ?r :where [?r :report/message-id ?mid]] db report-eid)
            current    (d/pull db
                               (into state-attrs [:report/deadline :report/expiry
                                                  :report/close-reason :report/topic
                                                  :report/superseded-by
                                                  :report/closed-proxy :report/acked-proxy
                                                  :report/owned-proxy :report/urgent-proxy
                                                  :report/important-proxy])
                               report-eid)
            ;; Resolve superseded-by message-id to a report entity
            target-eid (when superseded-by
                         (d/q '[:find ?r . :in $ ?mid
                                :where [?r :report/message-id ?mid]]
                              db superseded-by))
            all-tx (-> []
                       (into (build-directive-set-tx conn report-eid report-mid email-eid set))
                       (cond-> (and (contains? set :report/closed)
                                    (not (:report/close-reason current)))
                         (conj [:db/add report-eid :report/close-reason :resolved]))
                       (into (build-unset-tx report-eid current unset))
                       (cond-> (and (contains? unset :report/closed) (:report/close-reason current))
                         (conj [:db/retract report-eid :report/close-reason (:report/close-reason current)]))
                       (cond-> deadline
                         (conj [:db/add report-eid :report/deadline deadline]))
                       (cond-> (and undeadline? (:report/deadline current))
                         (conj [:db/retract report-eid :report/deadline (:report/deadline current)]))
                       (cond-> expiry
                         (conj [:db/add report-eid :report/expiry expiry]))
                       (cond-> (and unexpiry? (:report/expiry current))
                         (conj [:db/retract report-eid :report/expiry (:report/expiry current)]))
                       (cond-> topic
                         (conj [:db/add report-eid :report/topic topic]))
                       (cond-> (and untopic? (:report/topic current))
                         (conj [:db/retract report-eid :report/topic (:report/topic current)]))
                       ;; Supersede: set ref, close with reason, link related
                       (cond-> target-eid
                         (into [[:db/add report-eid :report/superseded-by target-eid]
                                [:db/add report-eid :report/closed email-eid]
                                [:db/add report-eid :report/close-reason :superseded]
                                [:db/add report-eid :report/related target-eid]
                                [:db/add target-eid :report/related report-eid]]))
                       ;; Unsupersede: retract ref, reopen, clear reason
                       (cond-> (and unsuperseded? (:report/superseded-by current))
                         (into (cond-> [[:db/retract report-eid :report/superseded-by
                                         (ref-eid (:report/superseded-by current))]]
                                 (:report/closed current)
                                 (conj [:db/retract report-eid :report/closed
                                        (ref-eid (:report/closed current))])
                                 (:report/close-reason current)
                                 (conj [:db/retract report-eid :report/close-reason
                                        (:report/close-reason current)])))))]
        (when (seq all-tx)
          (d/transact! conn all-tx)
          (tracking/bump-report-updated! conn report-eid)
          ;; Also bump the target report if we linked it
          (when target-eid (tracking/bump-report-updated! conn target-eid))
          (log/info "Commands:"
                    (str/join ", " (concat (map (fn [[attr addr]] (str (name attr) " -> " addr)) set)
                                           (map #(str "un-" (name %)) unset)
                                           (when deadline [(str "deadline " deadline)])
                                           (when undeadline? ["no deadline"])
                                           (when expiry [(str "expiry " expiry)])
                                           (when unexpiry? ["no expiry"])
                                           (when topic [(str "topic:" topic)])
                                           (when untopic? ["no topic"])
                                           (when target-eid [(str "superseded-by:" superseded-by)])
                                           (when (and unsuperseded? (:report/superseded-by current))
                                             ["not superseded"])))
                    (str "(proxy by " from-addr ")")))
        ;; Record unknown superseded-by target
        (when (and superseded-by (nil? target-eid))
          (log/warn "Superseded-by: unknown message-id" superseded-by)
          (when failure-ctx
            (record-failure! (assoc failure-ctx
                                    :reason :unknown-target
                                    :command (str "Superseded-by: " superseded-by)
                                    :report-mid report-mid))))))))

(defn- try-unclosed!
  "If a closed report has a Not closed or Not superseded directive, retract the closure."
  [conn report-eid directives is-maintainer? from-addr]
  (let [permitted (filter (fn [{:keys [action scope]}]
                            (and (#{:unset :unset-superseded} action)
                                 (or (= :user scope) (and (= :maintainer scope) is-maintainer?))))
                          directives)
        {:keys [unset unsuperseded?]} (resolve-commands permitted)]
    (when (or (contains? unset :report/closed) unsuperseded?)
      (let [current  (d/pull (d/db conn) [:report/closed :report/closed-proxy :report/close-reason
                                          :report/superseded-by :report/related] report-eid)
            superseded-ref (:report/superseded-by current)
            clear-supersede? (or unsuperseded? (and (contains? unset :report/closed) superseded-ref))
            all-tx   (-> []
                         (into (build-unset-tx report-eid current #{:report/closed}))
                         (cond-> (:report/close-reason current)
                           (conj [:db/retract report-eid :report/close-reason (:report/close-reason current)]))
                         (cond-> (and clear-supersede? superseded-ref)
                           (into (let [target-eid (ref-eid superseded-ref)]
                                   [[:db/retract report-eid :report/superseded-by target-eid]
                                    [:db/retract report-eid :report/related target-eid]
                                    [:db/retract target-eid :report/related report-eid]]))))]
        (when (seq all-tx)
          (d/transact! conn all-tx)
          (when (and clear-supersede? superseded-ref)
            (tracking/bump-report-updated! conn (ref-eid superseded-ref)))
          (tracking/bump-report-updated! conn report-eid)
          (log/info (str "Commands: "
                         (if unsuperseded? "not superseded" "not closed")
                         " (proxy by " from-addr ")")))))))

(defn- filter-triggers-by-scope [trig-result overrides is-maintainer?]
  (when trig-result
    (let [filtered (into {}
                         (keep (fn [[attr :as entry]]
                                 (let [cmd   (attr->trigger-cmd attr)
                                       scope (or (:scope (get overrides (:id cmd))) (:scope cmd))]
                                   (when (or (= :user scope) is-maintainer?) entry))))
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
          src-name    (d/q '[:find ?src . :in $ ?rid
                             :where [?rid :report/email ?e] [?e :email/source ?src]] db report-eid)
          source-cfg  (get source-map src-name)
          src-cmds    (build-source-commands source-cfg)
          overrides   (:overrides src-cmds)
          is-maint?   (roles/maintainer? roles from-addr (:email/date-sent email))
          trig-result (-> (detect-triggers report-type body-text src-cmds)
                          (filter-triggers-by-scope overrides is-maint?))
          aliases     (compile-directive-aliases (:command-aliases source-cfg))
          directives  (detect-directives report-type body-text overrides (:email/date-sent email) aliases)
          closed?     (some? (:report/closed (d/pull db [:report/closed] report-eid)))
          fail-ctx    (when (and from-addr src-name)
                        {:source     src-name
                         :from-addr  from-addr
                         :email-date (:email/date-sent email)
                         :email-mid  (:email/message-id email)})]

      (if closed?
        (do (try-unclosed! conn report-eid directives is-maint? from-addr)
            (boolean (seq directives)))
        (let [voted? (when (and (= :request report-type) from-addr)
                       (apply-vote! conn report-eid from-addr body-text email delivery source-cfg)
                       (some? (detect-vote body-text)))]
          (apply-triggers! conn report-eid trig-result eid (:email/message-id email))
          (apply-directives! conn report-eid directives eid from-addr is-maint? fail-ctx)
          (boolean (or (seq trig-result) (seq directives) voted?)))))
    false))
