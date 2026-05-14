;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.relations
  "Qualified relations between reports.  Pure helpers (no datalevin)
  for IDs, validation, tx-builders, then IO helpers that pose/retract.
  Asymmetric kinds (:resolves, :supersedes, :duplicates) store two
  datoms (one per direction); :related-to stores one canonicalized
  by ascending eid order.  See bark-schema.edn for the :rel/* attrs."
  (:require [clojure.string :as str]
            [datalevin.core :as d]))

(def all-kinds
  #{:resolves :resolved-by
    :supersedes :superseded-by
    :duplicates :duplicated-by
    :related-to})

(def asymmetric-kinds
  #{:resolves :resolved-by
    :supersedes :superseded-by
    :duplicates :duplicated-by})

(def symmetric-kinds #{:related-to})

(def inverse-kinds
  {:resolves      :resolved-by
   :resolved-by   :resolves
   :supersedes    :superseded-by
   :superseded-by :supersedes
   :duplicates    :duplicated-by
   :duplicated-by :duplicates
   :related-to    :related-to})

(def same-type-kinds
  "Kinds requiring (:report/type from) == (:report/type to)."
  #{:supersedes :superseded-by :duplicates :duplicated-by})

(def report-typed-kinds
  "Kinds requiring both ends to be actionable (:bug, :patch, :request)."
  #{:resolves :resolved-by
    :supersedes :superseded-by
    :duplicates :duplicated-by})

(def actionable-types #{:bug :patch :request})

(defn make-relation-id
  [from-eid kind to-eid]
  (str from-eid ":" (name kind) ":" to-eid))

(defn canonicalize
  "Symmetric kinds: [from to] in ascending eid order (so reciprocal
  poses produce the same :rel/id).  Asymmetric kinds pass through."
  [kind from-eid to-eid]
  (if (and (symmetric-kinds kind)
           (neg? (compare to-eid from-eid)))
    [to-eid from-eid]
    [from-eid to-eid]))

(defn- valid-resolves-pair?
  "True iff source/target form a legal :resolves pose (patch resolves
  bug or request).  :resolved-by is checked by inverting the pair."
  [source-type target-type]
  (and (= source-type :patch)
       (contains? #{:bug :request} target-type)))

(defn valid-pose?
  "True when a relation of `kind` is legal between these reports
  (kind known, no self-loop, type constraints satisfied)."
  [kind source-eid target-eid source-type target-type]
  (boolean
   (and (all-kinds kind)
        (not= source-eid target-eid)
        (or (not (report-typed-kinds kind))
            (and (actionable-types source-type)
                 (actionable-types target-type)))
        (or (not (same-type-kinds kind))
            (= source-type target-type))
        (case kind
          :resolves    (valid-resolves-pair? source-type target-type)
          :resolved-by (valid-resolves-pair? target-type source-type)
          true))))

(defn pose-tx
  "Datoms to pose a relation: 2 entity maps for asymmetric kinds, 1
  for symmetric.  Caller must have validated via `valid-pose?` and
  must check idempotence via :rel/id (do not reactivate a retracted
  relation by re-posing).  `value` is optional."
  [{:keys [from-eid to-eid kind setter email-eid posed-at value]}]
  (let [mk-rel (fn [f t k]
                 (cond-> {:rel/id       (make-relation-id f k t)
                          :rel/from     f
                          :rel/to       t
                          :rel/kind     k
                          :rel/setter   (some-> setter str/lower-case)
                          :rel/email    email-eid
                          :rel/posed-at posed-at
                          :rel/active?  true}
                   value (assoc :rel/value value)))]
    (if (symmetric-kinds kind)
      (let [[f t] (canonicalize kind from-eid to-eid)]
        [(mk-rel f t kind)])
      [(mk-rel from-eid to-eid kind)
       (mk-rel to-eid from-eid (inverse-kinds kind))])))

(defn retract-tx
  "Datoms to mark a relation retracted (active? false + audit ref)."
  [rel-eid retracted-by-email-eid]
  [{:db/id            rel-eid
    :rel/active?      false
    :rel/retracted-by retracted-by-email-eid}])

(defn paired-relation-ids
  "Pair of :rel/id strings for both directions of an asymmetric kind,
  or single canonical id for a symmetric one."
  [kind from-eid to-eid]
  (if (symmetric-kinds kind)
    (let [[f t] (canonicalize kind from-eid to-eid)]
      [(make-relation-id f kind t)])
    [(make-relation-id from-eid kind to-eid)
     (make-relation-id to-eid (inverse-kinds kind) from-eid)]))

;; ---------------------------------------------------------------------------
;; IO helpers (require datalevin)
;; ---------------------------------------------------------------------------

(defn pose-if-absent!
  "Pose a relation iff no datom with the same :rel/id exists (active
  or retracted).  Idempotent; never reactivates a retract."
  [conn opts]
  (let [db        (d/db conn)
        ids       (paired-relation-ids (:kind opts) (:from-eid opts) (:to-eid opts))
        existing? (some #(d/entid db [:rel/id %]) ids)]
    (when-not existing?
      (d/transact! conn (pose-tx opts)))))

(defn pose-from-email!
  "Pose a relation triggered by `email` (a pull/entity with :db/id,
  :email/author-address, :email/date-sent).  Sets :setter, :email-eid,
  :posed-at from the email; :from-eid/:to-eid/:kind come from `opts`.
  Caller must have validated via `valid-pose?`."
  [conn email opts]
  (pose-if-absent! conn
                   (merge {:setter    (:email/author-address email)
                           :email-eid (:db/id email)
                           :posed-at  (or (:email/date-sent email) (java.util.Date.))
                           :value     nil}
                          opts)))

(defn retract-pair!
  "Retract a (from, kind, to) relation if active.  Symmetric kinds
  accept either direction.  Returns true when anything was retracted."
  [conn from-eid kind to-eid retracted-by-email-eid]
  (let [db        (d/db conn)
        ids       (paired-relation-ids kind from-eid to-eid)
        active    (filterv (fn [rid]
                             (when-let [e (d/entid db [:rel/id rid])]
                               (= true (d/q '[:find ?a . :in $ ?e
                                              :where [?e :rel/active? ?a]]
                                            db e))))
                           ids)]
    (when (seq active)
      (let [tx (mapcat (fn [rid]
                         (retract-tx (d/entid db [:rel/id rid])
                                     retracted-by-email-eid))
                       active)]
        (d/transact! conn (vec tx))
        true))))

(defn active-inverse-relation
  "Eid of an active relation of `kind` posed in the reverse direction
  (:rel/from = to-eid, :rel/to = from-eid), or nil.  Used to enforce
  last-write-wins on conflicting closure-relation directives."
  [db from-eid to-eid kind]
  (d/q '[:find ?e .
         :in $ ?new-from ?new-to ?kind
         :where
         [?e :rel/from ?new-to]
         [?e :rel/to ?new-from]
         [?e :rel/kind ?kind]
         [?e :rel/active? true]]
       db from-eid to-eid kind))

(defn retract-by-from!
  "Retract every active relation of `kind` with :rel/from = `from-eid`
  (plus its inverse-direction sibling for asymmetric kinds)."
  [conn from-eid kind retracted-by-email-eid]
  (let [db   (d/db conn)
        eids (d/q '[:find [?e ...]
                    :in $ ?from ?kind
                    :where
                    [?e :rel/from ?from]
                    [?e :rel/kind ?kind]
                    [?e :rel/active? true]]
                  db from-eid kind)]
    (when (seq eids)
      (let [;; Retract the direct datoms
            tx (mapcat #(retract-tx % retracted-by-email-eid) eids)
            ;; For asymmetric kinds, also retract the inverse datoms
            inv (when (asymmetric-kinds kind)
                  (let [inv-kind (inverse-kinds kind)
                        inv-eids (d/q '[:find [?e ...]
                                        :in $ ?to ?inv-kind
                                        :where
                                        [?e :rel/to ?to]
                                        [?e :rel/kind ?inv-kind]
                                        [?e :rel/active? true]]
                                      db from-eid inv-kind)]
                    (mapcat #(retract-tx % retracted-by-email-eid) inv-eids)))]
        (d/transact! conn (vec (concat tx inv)))
        (count eids)))))

(defn retract-by-to!
  "Mirror of `retract-by-from!` keyed on :rel/to."
  [conn to-eid kind retracted-by-email-eid]
  (let [db   (d/db conn)
        eids (d/q '[:find [?e ...]
                    :in $ ?to ?kind
                    :where
                    [?e :rel/to ?to]
                    [?e :rel/kind ?kind]
                    [?e :rel/active? true]]
                  db to-eid kind)]
    (when (seq eids)
      (let [tx (mapcat #(retract-tx % retracted-by-email-eid) eids)
            inv (when (asymmetric-kinds kind)
                  (let [inv-kind (inverse-kinds kind)
                        inv-eids (d/q '[:find [?e ...]
                                        :in $ ?from ?inv-kind
                                        :where
                                        [?e :rel/from ?from]
                                        [?e :rel/kind ?inv-kind]
                                        [?e :rel/active? true]]
                                      db to-eid inv-kind)]
                    (mapcat #(retract-tx % retracted-by-email-eid) inv-eids)))]
        (d/transact! conn (vec (concat tx inv)))
        (count eids)))))

;; ---------------------------------------------------------------------------
;; Patch-closure propagation helpers
;; ---------------------------------------------------------------------------

(defn auto-credit?
  "True iff `bug-eid`.`attr` was set by a labelled patch report that
  :resolves the bug (i.e. via the implicit hook on a \"[PATCH]\" reply).
  Labelless credits (\"Re: [BUG]\" + diff) return false -- no :resolves
  exists, so R3/R4 don't retract them.  `attr` is :report/acked or
  :report/owned."
  [db bug-eid attr]
  (when-let [pose-eid (some-> (d/pull db [{attr [:db/id]}] bug-eid)
                              (get attr)
                              :db/id)]
    (boolean
     (d/q '[:find ?p . :in $ ?bug ?pose
            :where
            [?r :rel/from ?bug]
            [?r :rel/kind :resolved-by]
            [?r :rel/active? true]
            [?r :rel/to ?p]
            [?p :report/email ?pose]]
          db bug-eid pose-eid))))

(defn active-targets
  "Vector of `to`-eids for active outgoing relations of `kind` from `from-eid`."
  [db from-eid kind]
  (d/q '[:find [?to ...] :in $ ?from ?kind
         :where
         [?r :rel/from ?from]
         [?r :rel/kind ?kind]
         [?r :rel/active? true]
         [?r :rel/to ?to]]
       db from-eid kind))

(defn- retract-auto-credit-tx
  [db bug-eid attr addr-attr]
  (let [pulled (d/pull db [{attr [:db/id]} addr-attr] bug-eid)
        pose   (get pulled attr)
        addr   (get pulled addr-attr)]
    (cond-> []
      pose (conj [:db/retract bug-eid attr (:db/id pose)])
      addr (conj [:db/retract bug-eid addr-attr addr]))))

(defn- transfer-auto-credit-tx
  [bug-eid attr addr-attr new-email-eid new-addr]
  [{:db/id bug-eid
    attr new-email-eid
    addr-attr (some-> new-addr str/lower-case)}])

(defn propagate-patch-closure!
  "Propagate a patch's closure to the bugs/requests it :resolves:
  :resolved closes them, :canceled retracts auto-credits, :superseded
  transfers them to `successor-eid`.  No-op if `patch-type` ≠ :patch."
  [conn patch-eid patch-type email-eid close-reason successor-eid]
  (when (= :patch patch-type)
    (let [db   (d/db conn)
          bugs (active-targets db patch-eid :resolves)]
      (case close-reason
        :resolved
        ;; One snapshot is safe: bug-eids are distinct, so closing
        ;; bug-A doesn't change bug-B's :report/closed.
        (let [closed-bugs (when (seq bugs)
                            (set (d/q '[:find [?b ...]
                                        :in $ [?b ...]
                                        :where [?b :report/closed _]]
                                      db bugs)))]
          (doseq [bug-eid bugs]
            (when-not (contains? closed-bugs bug-eid)
              (d/transact! conn [{:db/id bug-eid
                                  :report/closed email-eid
                                  :report/close-reason :resolved}]))))

        :canceled
        (doseq [bug-eid bugs]
          (let [db' (d/db conn)
                tx  (cond-> []
                      (auto-credit? db' bug-eid :report/acked)
                      (into (retract-auto-credit-tx db' bug-eid
                                                    :report/acked :report/acked-address))
                      (auto-credit? db' bug-eid :report/owned)
                      (into (retract-auto-credit-tx db' bug-eid
                                                    :report/owned :report/owned-address)))]
            (when (seq tx)
              (d/transact! conn tx))))

        :superseded
        (when successor-eid
          (let [succ (d/pull db [{:report/email [:db/id :email/author-address
                                                 :email/date-sent]}]
                             successor-eid)
                succ-eml-eid (some-> succ :report/email :db/id)
                succ-addr    (some-> succ :report/email :email/author-address)
                succ-date    (or (some-> succ :report/email :email/date-sent)
                                 (java.util.Date.))]
            (doseq [bug-eid bugs]
              (let [db' (d/db conn)
                    tx  (cond-> []
                          (auto-credit? db' bug-eid :report/acked)
                          (into (transfer-auto-credit-tx
                                  bug-eid :report/acked :report/acked-address
                                  succ-eml-eid succ-addr))
                          (auto-credit? db' bug-eid :report/owned)
                          (into (transfer-auto-credit-tx
                                  bug-eid :report/owned :report/owned-address
                                  succ-eml-eid succ-addr)))]
                (when (seq tx)
                  (d/transact! conn tx))
                ;; Successor inherits the :resolves link (idempotent).
                (pose-if-absent! conn {:from-eid successor-eid :to-eid bug-eid
                                       :kind :resolves
                                       :setter succ-addr :email-eid succ-eml-eid
                                       :posed-at succ-date :value nil})))))

        ;; Other reasons (:expired, etc.) do not propagate.
        nil))))
