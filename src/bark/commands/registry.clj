;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.commands.registry
  "Pure command registry -- static data only, no runtime dependencies.
  Loadable by Babashka scripts (e.g. validate-config.clj) since it does
  not pull in datalevin."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Command registry
;;
;; :scope values:
;;   :user                 -- anyone
;;   :maintainer           -- any maintainer
;;   :setter-or-maintainer -- the address that previously set the attribute,
;;                           or any maintainer (maintainers keep their
;;                           administrative override)
;;
;; :setter-or-maintainer is only meaningful for unset-style directives
;; whose target attribute is tracked by a ref to the pose-email (see
;; `setter-ref-attrs` below).  These are the five original state
;; attrs (acked/owned/closed/urgent/important) plus topic, deadline,
;; expiry and superseded-by -- a total of nine `:un*` commands.
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
   ;; Unset directives -- :setter-or-maintainer lets the user who previously
   ;; set the attribute retract it (and maintainers retain full override).
   {:id :unacked     :kind :directive :action :unset :attr :report/acked    :scope :setter-or-maintainer
    :syntax "Not acked" :report-types #{:bug :patch :request}}
   {:id :unowned     :kind :directive :action :unset :attr :report/owned    :scope :setter-or-maintainer
    :syntax "Not owned" :report-types #{:bug :patch :request}}
   {:id :unclosed    :kind :directive :action :unset :attr :report/closed   :scope :setter-or-maintainer :syntax "Not closed"}
   {:id :unurgent    :kind :directive :action :unset :attr :report/urgent   :scope :setter-or-maintainer :syntax "Not urgent"}
   {:id :unimportant :kind :directive :action :unset :attr :report/important :scope :setter-or-maintainer :syntax "Not important"}
   ;; Deadline / topic
   {:id :deadline    :kind :directive :action :set-deadline   :attr :report/deadline :scope :user
    :syntax "Deadline" :param :date-or-duration :report-types #{:bug :patch :request}}
   {:id :undeadline  :kind :directive :action :unset-deadline :attr :report/deadline :scope :setter-or-maintainer
    :syntax "No deadline" :report-types #{:bug :patch :request}}
   {:id :expiry      :kind :directive :action :set-expiry   :attr :report/expiry :scope :user
    :syntax "Expiry" :param :date-or-duration :report-types #{:bug :patch :request}}
   {:id :unexpiry    :kind :directive :action :unset-expiry :attr :report/expiry :scope :setter-or-maintainer
    :syntax "No expiry" :report-types #{:bug :patch :request}}
   {:id :topic       :kind :directive :action :set-topic :attr :report/topic :scope :user
    :syntax "Topic" :param :word}
   {:id :untopic     :kind :directive :action :unset-topic :attr :report/topic :scope :setter-or-maintainer
    :syntax "No topic"}
   ;; Supersede -- backed by qualified relations (:rel/supersedes) since the
   ;; qualified-links refactor; :attr is retained for registry shape
   ;; compatibility but no longer drives schema lookup (the attr is gone).
   {:id :superseded-by  :kind :directive :action :set-superseded   :attr :rel/supersedes :scope :user
    :syntax "Superseded-by" :param :message-id :report-types #{:bug :patch :request}}
   {:id :unsuperseded   :kind :directive :action :unset-superseded :attr :rel/supersedes :scope :setter-or-maintainer
    :syntax "Not superseded" :report-types #{:bug :patch :request}}
   ;; Duplicate-of -- same shape as Superseded-by, but encodes "this is a
   ;; duplicate of another report" (close-reason :canceled).  Same-type
   ;; constraint enforced (bug=>bug, patch=>patch, request=>request).
   {:id :duplicate-of   :kind :directive :action :set-duplicate    :attr :rel/duplicates :scope :user
    :syntax "Duplicate-of" :param :message-id :report-types #{:bug :patch :request}}
   {:id :unduplicate    :kind :directive :action :unset-duplicate  :attr :rel/duplicates :scope :setter-or-maintainer
    :syntax "Not duplicate" :report-types #{:bug :patch :request}}
   ;; Related-to -- neutral cross-reference. No type constraint, no
   ;; closure effect, multiple targets per report allowed.  Scope :user
   ;; on both ends because the link is informational and the symmetric
   ;; kind canonicalises duplicates away by :rel/id.
   {:id :related-to :kind :directive :action :set-related   :attr :rel/related-to :scope :user
    :syntax "Related-to"     :param :message-id}
   {:id :unrelated  :kind :directive :action :unset-related :attr :rel/related-to :scope :user
    :syntax "Not related-to" :param :message-id}])

;; Derived indexes
(def trigger-commands   (filterv #(= :trigger   (:kind %)) commands))
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
;; author-address when the `-by` form is used.
(def address-attrs
  {:report/acked     :report/acked-address
   :report/owned     :report/owned-address
   :report/closed    :report/closed-address
   :report/urgent    :report/urgent-address
   :report/important :report/important-address})

;; All report attributes that Bark tracks as refs to the pose-email.
;; Shape: `{ref-attr paired-value-attr-or-nil}`.
;; The paired attr holds the business datum posed alongside the
;; setter identity -- a scalar for topic/deadline/expiry (`-value`).
;; The five proxy-state attrs (acked/owned/closed/urgent/important)
;; carry no paired value -- their "value" is just the fact that the
;; state was set, and the proxy-designated address lives in
;; `address-attrs`.
;;
;; Note: :report/superseded-by left the table when supersede was
;; migrated to qualified relations (:rel/supersedes).  The setter for
;; that command is now read from :rel/setter, not from a paired-attr.
(def setter-ref-attrs
  {:report/acked         nil
   :report/owned         nil
   :report/closed        nil
   :report/urgent        nil
   :report/important     nil
   :report/topic         :report/topic-value
   :report/deadline      :report/deadline-value
   :report/expiry        :report/expiry-value})

;; Commands accepting :scope :setter-or-maintainer.  Two flavours:
;; - unset-style directives whose target attribute is tracked by a ref
;;   to the pose-email (derived from `setter-ref-attrs`);
;; - explicitly-listed commands backed by qualified relations whose
;;   setter is recorded in :rel/setter (:unsuperseded, :unduplicate).
(def setter-scoped-command-ids
  (into #{:unsuperseded :unduplicate}
        (comp (filter #(str/starts-with? (name (:action %)) "unset"))
              (filter #(contains? setter-ref-attrs (:attr %)))
              (map :id))
        commands))
