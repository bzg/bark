;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.commands
  "Unified command detection, resolution, and application."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [taoensso.timbre :as log]
            [bark.common :as common]
            [bark.roles :as roles])
  (:import [java.text SimpleDateFormat]
           [java.util Date TimeZone]))

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
    :syntax "Unacked" :report-types #{:bug :patch :request}}
   {:id :unowned     :kind :directive :action :unset :attr :report/owned    :scope :maintainer
    :syntax "Unowned" :report-types #{:bug :patch :request}}
   {:id :unclosed    :kind :directive :action :unset :attr :report/closed   :scope :maintainer :syntax "Unclosed"}
   {:id :unurgent    :kind :directive :action :unset :attr :report/urgent   :scope :maintainer :syntax "Unurgent"}
   {:id :unimportant :kind :directive :action :unset :attr :report/important :scope :maintainer :syntax "Unimportant"}
   ;; Deadline / topic
   {:id :deadline    :kind :directive :action :set-deadline   :attr :report/deadline :scope :maintainer
    :syntax "Deadline" :param :date :report-types #{:bug :patch :request}}
   {:id :undeadline  :kind :directive :action :unset-deadline :attr :report/deadline :scope :maintainer
    :syntax "Undeadline" :report-types #{:bug :patch :request}}
   {:id :topic       :kind :directive :action :set-topic :attr :report/topic :scope :maintainer
    :syntax "Topic" :param :word}])

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
       :word          (str "^" qs ":\\s+([a-zA-Z0-9_-]+)" trailing-punct "?\\s*$")
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
    (let [fmt (SimpleDateFormat. "yyyy-MM-dd")]
      (.setTimeZone fmt (TimeZone/getTimeZone "UTC"))
      (.parse fmt s))
    (catch Exception _ nil)))

(defn- match-triggers [triggers body-text]
  (into {} (keep (fn [[k p]] (when (re-find p body-text) [(keyword "report" (name k)) true]))) triggers))

(defn detect-triggers [report-type body-text source-commands]
  (when body-text
    (let [compiled  (:compiled source-commands)
          overrides (:overrides source-commands)
          all-sets  (match-triggers compiled body-text)
          filtered (into {}
                         (keep (fn [[attr :as entry]]
                                 (let [cmd (attr->trigger-cmd attr)
                                       rt  (or (:report-types (get overrides (:id cmd)))
                                               (:report-types cmd))]
                                   (when (or (nil? rt) (contains? rt report-type)) entry))))
                         all-sets)
          reason   (when (:report/closed filtered)
                     (detect-close-reason (get-in source-commands [:words :closed]) body-text))
          result   (cond-> filtered reason (assoc :report/close-reason reason))]
      (when (seq result) result))))

(defn detect-directives
  ([report-type body-text] (detect-directives report-type body-text nil))
  ([report-type body-text overrides]
   (when body-text
     (let [lines (str/split-lines body-text)]
       (->> lines
            (keep (fn [line]
                    (some (fn [[{:keys [id action attr param scope report-types]} pattern]]
                            (let [rt (or (:report-types (get overrides id)) report-types)
                                  sc (or (:scope (get overrides id)) scope)]
                              (when (or (nil? rt) (contains? rt report-type))
                                (when-let [m (re-matches pattern line)]
                                  (let [base (case action
                                               :set            {:action :set :attr attr :email-address (nth m 1)}
                                               :unset          {:action :unset :attr attr}
                                               :set-deadline   (when-let [d (parse-date-iso (nth m 1))]
                                                                 {:action :set-deadline :date d})
                                               :unset-deadline {:action :unset-deadline}
                                               :set-topic      {:action :set-topic :topic (nth m 1)})]
                                    (when base (assoc base :scope sc)))))))
                          compiled-directives)))
            vec)))))

(defn resolve-commands [directives]
  (reduce (fn [acc {:keys [action attr email-address date topic]}]
            (case action
              :set   (-> acc (assoc-in [:set attr] email-address) (update :unset disj attr))
              :unset (-> acc (update :set dissoc attr) (update :unset conj attr))
              :set-deadline   (-> acc (assoc :deadline date) (dissoc :undeadline?))
              :unset-deadline (-> acc (dissoc :deadline) (assoc :undeadline? true))
              :set-topic      (assoc acc :topic topic)))
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

(defn- vote-allowed? [email source-cfg]
  (let [hdrs (:email/headers-edn email)]
    (cond
      (:list-id source-cfg)
      (and (some? (common/get-header hdrs "List-Id"))
           (some? (common/get-header hdrs "List-Post")))

      (:delivered-to source-cfg)
      (if-let [dt (common/get-header hdrs "Delivered-To")]
        (= (str/lower-case dt) (str/lower-case (:delivered-to source-cfg)))
        false)

      :else true)))

(defn- apply-vote! [conn report-eid from-addr body-text email source-cfg]
  (when-let [vote (detect-vote body-text)]
    (if-not (vote-allowed? email source-cfg)
      (log/info "Vote ignored (private email on public source)" from-addr)
      (let [db      (d/db conn)
            current (d/pull db [:report/voters :report/votes-up :report/votes-down
                                :report/votes-null] report-eid)
            voters  (set (:report/voters current))]
        (when-not (contains? voters from-addr)
          (let [attr (case vote :up :report/votes-up :down :report/votes-down :report/votes-null)
                n    (or (get current attr) 0)]
            (d/transact! conn [[:db/add report-eid attr (inc n)]
                               [:db/add report-eid :report/voters from-addr]])
            (common/bump-report-updated! conn report-eid)
            (log/info "Vote" (case vote :up "+1" :down "-1" "0") "by" from-addr)))))))

(defn apply-triggers! [conn report-eid trig-result email-eid email-mid]
  (when trig-result
    (let [close-reason (:report/close-reason trig-result)
          ref-result   (dissoc trig-result :report/close-reason)
          current      (d/pull (d/db conn) state-attrs report-eid)
          new-sets     (into {} (remove (fn [[k _]] (get current k))) ref-result)
          set-tx       (when (seq new-sets)
                         [(into {:db/id report-eid} (map (fn [[k _]] [k email-eid])) new-sets)])
          reason-tx    (when (and close-reason (:report/closed new-sets))
                         [[:db/add report-eid :report/close-reason close-reason]])
          all-tx       (vec (concat set-tx reason-tx))]
      (when (seq all-tx)
        (d/transact! conn all-tx)
        (common/bump-report-updated! conn report-eid)
        (log/info (str/join ", " (cond-> (mapv (comp name key) new-sets)
                                   close-reason (conj (str "close-reason:" (name close-reason)))))
                  "(by" email-mid ")")))))

(defn apply-directives! [conn report-eid directives email-eid from-addr is-maintainer?]
  (let [permitted (filter (fn [{:keys [scope]}]
                            (or (= :user scope) (and (= :maintainer scope) is-maintainer?)))
                          directives)]
    (when (seq permitted)
      (let [{:keys [set unset deadline undeadline? topic]} (resolve-commands permitted)
            report-mid (d/q '[:find ?mid . :in $ ?r :where [?r :report/message-id ?mid]]
                            (d/db conn) report-eid)
            current    (d/pull (d/db conn)
                               (into state-attrs [:report/deadline :report/close-reason
                                                  :report/closed-proxy :report/acked-proxy
                                                  :report/owned-proxy :report/urgent-proxy
                                                  :report/important-proxy])
                               report-eid)
            set-tx (mapcat (fn [[attr addr]]
                             (let [target-eid (find-or-create-synthetic-email!
                                               conn addr report-mid attr)]
                               [[:db/add report-eid attr target-eid]
                                [:db/add report-eid (proxy-attrs attr) email-eid]]))
                           set)
            close-reason-tx (when (contains? set :report/closed)
                              [[:db/add report-eid :report/close-reason :resolved]])
            unset-tx (mapcat (fn [attr]
                               (when-let [cur (get current attr)]
                                 (let [retract    [[:db/retract report-eid attr (ref-eid cur)]]
                                       proxy-attr (proxy-attrs attr)
                                       proxy-cur  (get current proxy-attr)]
                                   (if proxy-cur
                                     (conj retract [:db/retract report-eid proxy-attr (ref-eid proxy-cur)])
                                     retract))))
                             unset)
            unclose-reason-tx (when (and (contains? unset :report/closed)
                                         (:report/close-reason current))
                                [[:db/retract report-eid :report/close-reason
                                  (:report/close-reason current)]])
            deadline-tx (cond
                          deadline    [[:db/add report-eid :report/deadline deadline]]
                          undeadline? (when (:report/deadline current)
                                        [[:db/retract report-eid :report/deadline
                                          (:report/deadline current)]])
                          :else       nil)
            topic-tx (when topic [[:db/add report-eid :report/topic topic]])
            all-tx (vec (concat set-tx close-reason-tx unset-tx unclose-reason-tx
                                deadline-tx topic-tx))]
        (when (seq all-tx)
          (d/transact! conn all-tx)
          (common/bump-report-updated! conn report-eid)
          (let [desc (concat (map (fn [[attr addr]] (str (name attr) " -> " addr)) set)
                             (map #(str "un-" (name %)) unset)
                             (when deadline [(str "deadline " deadline)])
                             (when undeadline? ["undeadline"])
                             (when topic [(str "topic:" topic)]))]
            (log/info "Commands:" (str/join ", " desc) "(proxy by" from-addr ")")))))))

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
  "Detect and apply all commands from an email's body text."
  [conn report-eid report-type email source-map roles]
  (let [body-text (common/email-body-text email)
        from-addr (:email/from-address email)
        eid       (:db/id email)]
    (when body-text
      (let [src-name    (d/q '[:find ?src . :in $ ?rid :where
                                [?rid :report/email ?e] [?e :email/source ?src]]
                             (d/db conn) report-eid)
            source-cfg  (get source-map src-name)
            src-cmds    (build-source-commands source-cfg)
            overrides   (:overrides src-cmds)
            trig-result (detect-triggers report-type body-text src-cmds)
            directives  (detect-directives report-type body-text overrides)
            is-maintainer? (roles/maintainer? roles from-addr (:email/date-sent email))
            trig-result (filter-triggers-by-scope trig-result overrides is-maintainer?)
            closed?     (some? (:report/closed (d/pull (d/db conn) [:report/closed] report-eid)))]

        (when (and (= :request report-type) from-addr (not closed?))
          (apply-vote! conn report-eid from-addr body-text email source-cfg))

        (if closed?
          (let [unclosed-dirs (filter (fn [{:keys [action scope]}]
                                        (and (= :unset action)
                                             (or (= :user scope)
                                                 (and (= :maintainer scope) is-maintainer?))))
                                      directives)]
            (when (seq unclosed-dirs)
              (let [{:keys [unset]} (resolve-commands unclosed-dirs)]
                (when (contains? unset :report/closed)
                  (let [current (d/pull (d/db conn) [:report/closed :report/closed-proxy
                                                     :report/close-reason] report-eid)
                        retract-tx (when-let [cur (:report/closed current)]
                                     [[:db/retract report-eid :report/closed (ref-eid cur)]])
                        proxy-tx   (when-let [cur (:report/closed-proxy current)]
                                     [[:db/retract report-eid :report/closed-proxy (ref-eid cur)]])
                        reason-tx  (when (:report/close-reason current)
                                     [[:db/retract report-eid :report/close-reason
                                       (:report/close-reason current)]])
                        all-tx     (vec (concat retract-tx proxy-tx reason-tx))]
                    (when (seq all-tx)
                      (d/transact! conn all-tx)
                      (common/bump-report-updated! conn report-eid)
                      (log/info "Commands: un-closed (proxy by" from-addr ")")))))))

          (do
            (apply-triggers! conn report-eid trig-result eid (:email/message-id email))
            (apply-directives! conn report-eid directives eid from-addr is-maintainer?)))))))
