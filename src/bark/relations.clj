;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.relations
  "Helpers for qualified relations between reports.

  Two layers:
  - Pure helpers (no datalevin call): relation IDs, kind metadata,
    validation, tx-data builders.  The namespace itself requires
    datalevin for the IO layer below; if a Babashka consumer ever
    needs the pure layer, split it into a dedicated `bark.relations.pure`.
  - IO (with datalevin): pose/retract helpers that read the current
    DB to enforce idempotence and resolve relation entities.

  A relation is a Datalevin entity carrying:
    :rel/id        -- deterministic '<from>:<kind>:<to>' (unique identity)
    :rel/from      -- source report eid
    :rel/to        -- target report eid
    :rel/kind      -- keyword from `all-kinds`
    :rel/setter    -- credited address (lowercased)
    :rel/email     -- email eid that posed/deduced the relation
    :rel/posed-at  -- instant
    :rel/value     -- optional payload (e.g. target message-id for :supersedes)
    :rel/active?   -- boolean (false after retract)
    :rel/retracted-by -- email eid that retracted (audit)

  Asymmetric kinds (:resolves, :supersedes, :duplicates) store two
  datoms -- one per direction -- so queries are O(1) in either sense.
  The symmetric kind :related-to stores a single datom canonicalized
  by ascending eid order."
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
  "For symmetric kinds, return [from to] in ascending eid order so two
  reciprocal poses produce the same :rel/id. Asymmetric kinds pass through."
  [kind from-eid to-eid]
  (if (and (symmetric-kinds kind)
           (neg? (compare to-eid from-eid)))
    [to-eid from-eid]
    [from-eid to-eid]))

(defn valid-pose?
  "True when a relation of `kind` can legally be posed between these
  reports. False if: kind unknown, self-loop, type constraint violated."
  [kind source-eid target-eid source-type target-type]
  (boolean
   (and (all-kinds kind)
        (not= source-eid target-eid)
        (or (not (report-typed-kinds kind))
            (and (actionable-types source-type)
                 (actionable-types target-type)))
        (or (not (same-type-kinds kind))
            (= source-type target-type)))))

(defn pose-tx
  "Datoms to pose a relation. Returns 2 entity maps for asymmetric
  kinds (one per direction) or 1 for symmetric.

  Caller responsibilities:
  - validate via `valid-pose?` first;
  - check idempotence via :rel/id lookup before transacting (do not
    reactivate a retracted relation by re-posing).

  `value` is optional (nil = no :rel/value datom written)."
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
  "Pose a relation iff no datom with the same :rel/id exists (active or
  retracted).  Caller has already validated via `valid-pose?`.
  Idempotent on replay; never reactivates a retracted relation."
  [conn opts]
  (let [db        (d/db conn)
        ids       (paired-relation-ids (:kind opts) (:from-eid opts) (:to-eid opts))
        existing? (some #(d/entid db [:rel/id %]) ids)]
    (when-not existing?
      (d/transact! conn (pose-tx opts)))))

(defn retract-pair!
  "Retract a specific (from, kind, to) relation if active.  Handles
  symmetric kinds by canonicalizing the lookup, so callers can pass
  either direction.  Returns true when something was retracted."
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

(defn retract-by-from!
  "Retract (set :rel/active? false) all active relations of `kind`
  whose :rel/from is `from-eid`.  For asymmetric kinds, also retracts
  the inverse direction.  `retracted-by-email-eid` is recorded for audit."
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

;; ---------------------------------------------------------------------------
;; Patch-closure propagation helpers
;; ---------------------------------------------------------------------------

(defn auto-credit?
  "True iff the pose-email currently set on `bug-eid`.`attr` is the
  source-email of a patch that resolves `bug-eid`, i.e. it was posted
  by the auto-credit hook rather than by a human command.
  `attr` is :report/acked or :report/owned."
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
  "When a patch transitions to :report/closed with a given
  `close-reason`, propagate to the bugs/requests it resolves:

  - :resolved   close them too
  - :canceled   retract auto-credits posed by the patch
  - :superseded transfer auto-credits to `successor-eid`

  Caller MUST pass the patch's report-type; no-op if not :patch.
  `successor-eid` is the report :supersedes points to (nil if absent
  or unknown, only meaningful for :superseded)."
  [conn patch-eid patch-type email-eid close-reason successor-eid]
  (when (= :patch patch-type)
    (let [db   (d/db conn)
          bugs (active-targets db patch-eid :resolves)]
      (case close-reason
        :resolved
        (doseq [bug-eid bugs]
          (let [closed? (some? (:report/closed
                                (d/pull (d/db conn) [:report/closed] bug-eid)))]
            (when-not closed?
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
