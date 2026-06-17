;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.commands.registry
  "Pure command registry -- static data, no runtime deps (loadable from bb)."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Vocabulary
;;
;;   :kind :trigger    -- state-change command.  Acts only on existing
;;                        reports; never on the carrying mail, even when
;;                        that mail opens its thread.  Examples: Closed.,
;;                        Acked., Owned-by:, Superseded-by:, Duplicate-of:.
;;
;;   :kind :annotation -- property-set command.  When the carrying mail
;;                        creates a new report, applies to it; otherwise
;;                        to the nearest report in the thread.  Examples:
;;                        Urgent., Important., Topic:, Deadline:,
;;                        Supersedes:, Related-to:.
;;
;; The syntactic form is orthogonal: bareword (`:words` key) or colon-
;; prefixed line (`:syntax` key + optional `:param`).
;;
;; :scope values:
;;   :user                 -- anyone
;;   :maintainer           -- any maintainer
;;   :setter-or-maintainer -- the address that set the attribute, plus
;;                            any maintainer.  Only meaningful for unset
;;                            commands whose target has a recoverable
;;                            setter (`setter-ref-attrs` /
;;                            `setter-scoped-command-ids`).
;; ---------------------------------------------------------------------------

(def commands
  [;; --- Triggers: state-change ----------------------------------------------
   ;; Bareword (sender = setter)
   {:id :acked    :kind :trigger :action :set :attr :report/acked  :scope :user
    :words :acked  :report-types #{:bug :patch :request}}
   {:id :owned    :kind :trigger :action :set :attr :report/owned  :scope :user
    :words :owned  :report-types #{:bug :patch :request}}
   {:id :closed   :kind :trigger :action :set :attr :report/closed :scope :user :words :closed}
   ;; -by lines (maintainer credits a third party)
   {:id :acked-by  :kind :trigger :action :set :attr :report/acked  :scope :maintainer
    :syntax "Acked-by"  :param :email-address :report-types #{:bug :patch :request}}
   {:id :owned-by  :kind :trigger :action :set :attr :report/owned  :scope :maintainer
    :syntax "Owned-by"  :param :email-address :report-types #{:bug :patch :request}}
   {:id :closed-by :kind :trigger :action :set :attr :report/closed :scope :maintainer
    :syntax "Closed-by" :param :email-address}
   ;; Unset lines
   {:id :unacked  :kind :trigger :action :unset :attr :report/acked  :scope :setter-or-maintainer
    :syntax "Not acked"  :report-types #{:bug :patch :request}}
   {:id :unowned  :kind :trigger :action :unset :attr :report/owned  :scope :setter-or-maintainer
    :syntax "Not owned"  :report-types #{:bug :patch :request}}
   {:id :unclosed :kind :trigger :action :unset :attr :report/closed :scope :setter-or-maintainer
    :syntax "Not closed"}
   ;; Closure relations -- backed by :rel/supersedes / :rel/duplicates;
   ;; :attr kept for registry shape.
   ;; The relation unsets use a direction-suffixed :attr so that
   ;; `scope-permits?` finds the right setter in the pull map built
   ;; by `relation-setters-as-pull` -- a chained report shares the
   ;; `:rel/supersedes` schema kind across both directions, so the
   ;; pull-map key must distinguish them.
   {:id :superseded-by    :kind :trigger :action :set-superseded   :attr :rel/supersedes      :scope :user
    :syntax "Superseded-by"     :param :message-id :report-types #{:bug :patch :request}}
   {:id :unsuperseded-by  :kind :trigger :action :unset-superseded :attr :rel/supersedes-from :scope :setter-or-maintainer
    :syntax "Not superseded-by" :param :message-id :report-types #{:bug :patch :request}}
   {:id :duplicate-of    :kind :trigger :action :set-duplicate   :attr :rel/duplicates      :scope :user
    :syntax "Duplicate-of"     :param :message-id :report-types #{:bug :patch :request}}
   {:id :unduplicate-of  :kind :trigger :action :unset-duplicate :attr :rel/duplicates-from :scope :setter-or-maintainer
    :syntax "Not duplicate-of" :param :message-id :report-types #{:bug :patch :request}}

   ;; --- Annotations: property-set -------------------------------------------
   ;; Bareword
   {:id :urgent    :kind :annotation :action :set :attr :report/urgent    :scope :user :words :urgent}
   {:id :important :kind :annotation :action :set :attr :report/important :scope :user :words :important}
   ;; Unset lines for bareword annotations
   {:id :unurgent    :kind :annotation :action :unset :attr :report/urgent    :scope :setter-or-maintainer
    :syntax "Not urgent"}
   {:id :unimportant :kind :annotation :action :unset :attr :report/important :scope :setter-or-maintainer
    :syntax "Not important"}
   ;; Deadline / expiry / topic
   {:id :deadline    :kind :annotation :action :set-deadline   :attr :report/deadline :scope :user
    :syntax "Deadline" :param :date-or-duration :report-types #{:bug :patch :request}}
   {:id :undeadline  :kind :annotation :action :unset-deadline :attr :report/deadline :scope :setter-or-maintainer
    :syntax "No deadline" :report-types #{:bug :patch :request}}
   {:id :expiry      :kind :annotation :action :set-expiry   :attr :report/expiry :scope :user
    :syntax "Expiry" :param :date-or-duration}
   {:id :unexpiry    :kind :annotation :action :unset-expiry :attr :report/expiry :scope :setter-or-maintainer
    :syntax "No expiry"}
   {:id :topic       :kind :annotation :action :set-topic   :attr :report/topic :scope :user
    :syntax "Topic" :param :word}
   {:id :untopic     :kind :annotation :action :unset-topic :attr :report/topic :scope :setter-or-maintainer
    :syntax "No topic"}
   ;; Supersedes -- inverse role of Superseded-by (posed on the replacement;
   ;; current = :rel/to, target = :rel/from = the closed report).
   {:id :supersedes   :kind :annotation :action :set-supersedes   :attr :rel/supersedes    :scope :user
    :syntax "Supersedes"     :param :message-id :report-types #{:bug :patch :request}}
   {:id :unsupersedes :kind :annotation :action :unset-supersedes :attr :rel/supersedes-to :scope :setter-or-maintainer
    :syntax "Not supersedes" :param :message-id :report-types #{:bug :patch :request}}
   ;; Related-to -- neutral cross-reference (no closure, multi-target,
   ;; symmetric canonicalised by :rel/id).  Multi-target makes a clean
   ;; setter check impractical, so :unrelated-to stays at :user.
   {:id :related-to   :kind :annotation :action :set-related   :attr :rel/related-to :scope :user
    :syntax "Related-to"     :param :message-id}
   {:id :unrelated-to :kind :annotation :action :unset-related :attr :rel/related-to :scope :user
    :syntax "Not related-to" :param :message-id}])

;; ---------------------------------------------------------------------------
;; Derived indexes
;; ---------------------------------------------------------------------------

;; Syntactic groupings (derived from :words vs :syntax presence).
;; The semantic axis (`:kind :trigger` / `:annotation`) is metadata
;; on each entry, not surfaced as a derived index -- nothing consumes
;; the full lists today; if you need them, write the `filterv` at
;; the call site.
(def word-commands (filterv :words  commands))
(def line-commands (filterv :syntax commands))

(def commands-by-id (into {} (map (juxt :id identity)) commands))

;; Attr lookup for bareword commands -- one bareword per attr, so this
;; mapping is unambiguous (unlike the broader :attr-to-trigger mapping
;; which would collide for attrs that also have a -by line form).
(def attr->word-cmd
  (into {} (map (juxt :attr identity)) word-commands))

;; Cross-reference annotations -- commands that pose a neutral link
;; between reports without changing the report's state (Supersedes:,
;; Related-to: and their unsets).  Identified by `:kind :annotation`
;; combined with a `:rel/`-namespaced `:attr`.  Closure relations
;; (Superseded-by:, Duplicate-of:) are excluded because they're
;; triggers -- they DO change state and propagate through a series
;; like any other trigger.
;;
;; Used by `apply-commands!` to skip these four when broadcasting
;; cover-letter commands to the rest of a patch series: broadcasting
;; a neutral cross-reference would pose N redundant edges to the
;; same target.
(def cross-ref-line-ids
  (into #{}
        (comp (filter #(and (= :annotation (:kind %))
                            (some-> % :attr namespace (= "rel"))))
              (map :id))
        commands))

;; State attrs that support the `-by` proxy form (e.g. Acked-by:
;; bob@x credits Bob instead of the sender).  The credited address
;; lives in `address-attrs`.
(def proxy-state-attrs
  [:report/acked :report/owned :report/closed :report/urgent :report/important])

(def address-attrs
  {:report/acked     :report/acked-address
   :report/owned     :report/owned-address
   :report/closed    :report/closed-address
   :report/urgent    :report/urgent-address
   :report/important :report/important-address})

;; Report attrs tracked as refs to the pose-email.
;;   {ref-attr paired-value-attr-or-nil}
;; The paired attr holds the business datum (topic/deadline/expiry use
;; `-value`).  Proxy-state attrs carry no paired value -- the credited
;; address lives in `address-attrs`.  Supersede left the table when it
;; migrated to :rel/supersedes (setter now in :rel/setter).
(def setter-ref-attrs
  {:report/acked         nil
   :report/owned         nil
   :report/closed        nil
   :report/urgent        nil
   :report/important     nil
   :report/topic         :report/topic-value
   :report/deadline      :report/deadline-value
   :report/expiry        :report/expiry-value})

;; Commands accepting :scope :setter-or-maintainer:
;; - unset lines keyed on a setter-ref attribute, plus
;; - explicit relation-backed unsets (:unsuperseded-by, :unsupersedes,
;;   :unduplicate-of).
(def setter-scoped-command-ids
  (into #{:unsuperseded-by :unsupersedes :unduplicate-of}
        (comp (filter #(str/starts-with? (name (:action %)) "unset"))
              (filter #(contains? setter-ref-attrs (:attr %)))
              (map :id))
        commands))
