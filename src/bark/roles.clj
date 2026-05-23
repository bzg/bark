;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.roles
  "Maintainer tenure management, controls, and permission checks."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [taoensso.timbre :as log]
            [bark.common :as common]
            [bark.commands :as commands]
            [bark.periods :as periods]
            [bark.tracking :as tracking])
  (:import [java.util Date]))

;; ---------------------------------------------------------------------------
;; Tenure queries
;; ---------------------------------------------------------------------------

(defn get-tenures
  "Return the seq of all tenure maps (active and closed) for `source-name`."
  [db source-name]
  (let [eids (d/q '[:find [?e ...]
                    :in $ ?src
                    :where [?e :maint-tenure/source ?src]]
                  db source-name)]
    (mapv #(common/tenure-map (d/pull db common/tenure-pull-pattern %)) eids)))

(defn- active-tenure-eid
  "Return the :db/id of the currently-active tenure for (source, email), or nil."
  [db source-name email]
  (let [a (str/lower-case email)]
    (->> (get-tenures db source-name)
         (filter #(and (= a (:email %)) (nil? (:to %))))
         first
         :eid)))

;; ---------------------------------------------------------------------------
;; Config seeding -- per-period sync
;; ---------------------------------------------------------------------------

(defn- active-as-of
  "Tenures whose half-open window [:from, :to) contains `as-of`.
  nil `as-of` (dawn of time) returns []."
  [tenures ^Date as-of]
  (when as-of
    (filter (fn [{:keys [^Date from ^Date to]}]
              (and (or (nil? from) (not (.after from as-of)))
                   (or (nil? to)   (.after to as-of))))
            tenures)))

(defn- covering-tenure
  "Return the tenure for `email` whose window contains `as-of`, or nil."
  [tenures email ^Date as-of]
  (first (filter (fn [{:keys [^Date from ^Date to] e :email}]
                   (and (= e email)
                        (or (nil? from) (not (.after from as-of)))
                        (or (nil? to)   (.after to as-of))))
                 tenures)))

(defn- existing-tenure-with-from
  "Any tenure for `email` whose :from matches `from` (regardless of :to)."
  [tenures email from]
  (some #(and (= email (:email %)) (= from (:from %)) %) tenures))

(defn- sync-period-boundary!
  "Reconcile tenures with `period` at its start: open declared emails
  missing a tenure at :from, close active emails dropped from the list.
  Idempotent and respects mail-directive closures (use --fresh to
  replay).  The first unbounded period (`:from` nil) only adds; it
  never closes, since `active-as-of` returns no tenure without a
  reference instant."
  [conn source-name {:keys [^Date from maintainers]}]
  (let [all       (get-tenures (d/db conn) source-name)
        declared  (into [] (distinct (map str/lower-case (or maintainers []))))
        declared? (set declared)
        active?   (set (map :email (active-as-of all from)))
        adds      (into []
                        (keep (fn [[idx email]]
                                (when-not (or (active? email)
                                              (existing-tenure-with-from all email from))
                                  (cond-> {:maint-tenure/source source-name
                                           :maint-tenure/email  email
                                           :maint-tenure/order  idx}
                                    from (assoc :maint-tenure/from from)))))
                        (map-indexed vector declared))
        drops     (remove declared? active?)
        closes    (when from
                    (into []
                          (keep (fn [email]
                                  (when-let [t (covering-tenure all email from)]
                                    [[:db/add (:eid t) :maint-tenure/to from] email])))
                          drops))]
    (when (seq adds)
      (d/transact! conn adds)
      (doseq [{email :maint-tenure/email f :maint-tenure/from} adds]
        (log/info (str "Config maintainer: " email
                       (when f (str " (since " (common/format-date-iso f) ")"))
                       " (for " source-name ")"))))
    (when (seq closes)
      (d/transact! conn (mapv first closes))
      (doseq [[_ email] closes]
        (log/info (str "Config sync: closed tenure for " email
                       " (for " source-name " at "
                       (common/format-date-iso from) ")"))))))

(defn sync-source-tenures!
  "Walk `source` periods chronologically, syncing tenures at each."
  [conn source]
  (doseq [period (periods/source-periods source)]
    (sync-period-boundary! conn (:name source) period)))

(defn sync-all-sources!
  "Sync tenures for every source in `config`."
  [conn config]
  (doseq [src (:sources config)]
    (sync-source-tenures! conn src)))

;; ---------------------------------------------------------------------------
;; Role control parsing and application
;; ---------------------------------------------------------------------------

(defn role-control-pattern
  "Build the role-control regex for the given syntax mode."
  [strict-syntax?]
  (re-pattern (str "(?m)^" (common/bang-prefix strict-syntax?)
                   "(Add maintainer|Remove maintainer):\\s+(.+)$")))

(def ^:private address-pattern
  (let [addr "[^@\\s<>]+@[^@\\s<>]+\\.[^@\\s<>]+"]
    (re-pattern (str "<(" addr ")>|(" addr ")"))))

(defn- parse-addresses
  "Extract email addresses from a maintainer-directive argument.
  Accepts bare `a@b` or RFC 5322 `Name <a@b>` forms; junk words are
  ignored.  Order-preserving."
  [s]
  (when s
    (->> (re-seq address-pattern s)
         (keep (fn [[_ bracketed bare]] (or bracketed bare)))
         vec)))

(defn parse-role-controls
  ([body-text] (parse-role-controls body-text false))
  ([body-text strict-syntax?]
   (when body-text
     (->> (re-seq (role-control-pattern strict-syntax?) body-text)
          (mapv (fn [[_ cmd addrs]]
                  {:command cmd :addresses (parse-addresses addrs)}))))))

(defn- open-tenure!
  "Open a new tenure for each address that does not already have an active one.
  Returns the list of addresses for which a tenure was actually created."
  [conn source-name addresses email-date]
  (let [db    (d/db conn)
        opens (remove #(active-tenure-eid db source-name %) addresses)]
    (when (seq opens)
      (d/transact! conn
                   (mapv (fn [addr]
                           {:maint-tenure/source source-name
                            :maint-tenure/email  (str/lower-case addr)
                            :maint-tenure/from   email-date})
                         opens))
      (mapv str/lower-case opens))))

(defn- close-tenure!
  "Close active tenures for `addresses` (setting :to = email-date).
  The lead maintainer is never closed (attempts are logged + recorded
  as :insufficient-scope failures via `failure-ctx`)."
  [conn tenures source-name addresses email-date failure-ctx]
  (let [lead (common/lead-maintainer tenures)]
    (->> addresses
         (keep (fn [addr]
                 (let [a (str/lower-case addr)]
                   (cond
                     (= a lead)
                     (do (log/warn "Denied: cannot remove lead maintainer" a
                                   "(for" source-name ")")
                         (when failure-ctx
                           (commands/record-failure!
                            (assoc failure-ctx
                                   :reason   :insufficient-scope
                                   :audience :maintainers
                                   :command  (str "Remove maintainer: " a))))
                         nil)
                     :else
                     ;; Per-iteration DB refresh: closing one tenure
                     ;; mutates state that later lookups must see.
                     (when-let [eid (active-tenure-eid (d/db conn) source-name a)]
                       (d/transact! conn [[:db/add eid :maint-tenure/to email-date]])
                       a)))))
         vec)))

(defn apply-role-controls!
  "Apply Add/Remove maintainer directives from `body-text`. `tenures`
  is the pre-directive snapshot for permission checks; DB is re-read
  between operations.  Denied attempts go to the failures file as
  :insufficient-scope/:maintainers so the lead sees them."
  ([conn tenures source-name from-addr body-text email-date]
   (apply-role-controls! conn tenures source-name from-addr body-text email-date false))
  ([conn tenures source-name from-addr body-text email-date strict-syntax?]
   (let [controls    (parse-role-controls body-text strict-syntax?)
         is-maint    (common/maintainer? tenures from-addr)
         is-lead     (common/lead-maintainer? tenures from-addr)
         failure-ctx (when (and from-addr source-name)
                       {:source     source-name
                        :from-addr  from-addr
                        :email-date email-date
                        :report-mid ""})]
     (doseq [{:keys [command addresses]} controls]
      (case command
        "Add maintainer"
        (if is-maint
          (when-let [opened (seq (open-tenure! conn source-name addresses email-date))]
            (tracking/bump-global-modified! conn)
            (log/info "add maintainer:" (str/join " " opened)
                      (str "(for " source-name ")")))
          (do (log/warn "Denied:" from-addr "lacks permission for: Add maintainer")
              (when failure-ctx
                (commands/record-failure!
                 (assoc failure-ctx
                        :reason   :insufficient-scope
                        :audience :maintainers
                        :command  (str "Add maintainer: " (str/join " " addresses)))))))

        "Remove maintainer"
        (if is-lead
          ;; Always re-read tenures before closing so the lead check uses
          ;; the latest state (e.g. if this directive follows an Add).
          (let [current (get-tenures (d/db conn) source-name)]
            (when-let [closed (seq (close-tenure! conn current source-name
                                                  addresses email-date failure-ctx))]
              (tracking/bump-global-modified! conn)
              (log/info "remove maintainer:" (str/join " " closed)
                        (str "(for " source-name ")"))))
          (do (log/warn "Denied:" from-addr
                        "lacks permission for: Remove maintainer (lead only)")
              (when failure-ctx
                (commands/record-failure!
                 (assoc failure-ctx
                        :reason   :insufficient-scope
                        :audience :maintainers
                        :command  (str "Remove maintainer: " (str/join " " addresses)))))))

        nil)))))

;; ---------------------------------------------------------------------------
;; Permission check for report creation (pure)
;; ---------------------------------------------------------------------------

(def ^:private default-restricted-types #{:announcement :release :change})

(defn can-create-report?
  "True iff `from-addr` may create this report type.  Report types in
  the source's `:restricted-types` set require maintainer status;
  other types pass (the source-match gate already filtered).

  Default restricted set: #{:announcement :release :change}.  An
  explicit empty set opens every type to any sender."
  [roles from-addr report-info email source-cfg]
  (let [restricted (get source-cfg :restricted-types default-restricted-types)]
    (if (contains? restricted (:type report-info))
      (common/maintainer? roles from-addr (:email/date-sent email))
      true)))
