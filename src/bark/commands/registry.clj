;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.commands.registry
  "Pure command registry -- static data, no runtime deps (loadable from bb)."
  (:require [clojure.string :as str]))

;; Command registry.  :scope values:
;;   :user                 -- anyone
;;   :maintainer           -- any maintainer
;;   :setter-or-maintainer -- the address that set the attribute, plus
;;                            any maintainer.  Only meaningful for unset
;;                            directives whose target has a recoverable
;;                            setter (`setter-ref-attrs` /
;;                            `setter-scoped-command-ids`).
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
   ;; Supersede -- backed by :rel/supersedes; :attr kept for registry shape.
   {:id :superseded-by  :kind :directive :action :set-superseded   :attr :rel/supersedes :scope :user
    :syntax "Superseded-by" :param :message-id :report-types #{:bug :patch :request}}
   {:id :unsuperseded   :kind :directive :action :unset-superseded :attr :rel/supersedes :scope :setter-or-maintainer
    :syntax "Not superseded" :report-types #{:bug :patch :request}}
   ;; Supersedes -- inverse role of Superseded-by (posed on the replacement;
   ;; current = :rel/to, target = :rel/from = the closed report).
   {:id :supersedes     :kind :directive :action :set-supersedes   :attr :rel/supersedes :scope :user
    :syntax "Supersedes" :param :message-id :report-types #{:bug :patch :request}}
   {:id :unsupersedes   :kind :directive :action :unset-supersedes :attr :rel/supersedes :scope :setter-or-maintainer
    :syntax "Not superseding" :report-types #{:bug :patch :request}}
   ;; Duplicate-of -- same shape as Superseded-by; close-reason :canceled.
   ;; Same-type constraint enforced (bug=>bug, patch=>patch, request=>request).
   {:id :duplicate-of   :kind :directive :action :set-duplicate    :attr :rel/duplicates :scope :user
    :syntax "Duplicate-of" :param :message-id :report-types #{:bug :patch :request}}
   {:id :unduplicate    :kind :directive :action :unset-duplicate  :attr :rel/duplicates :scope :setter-or-maintainer
    :syntax "Not duplicate" :report-types #{:bug :patch :request}}
   ;; Related-to -- neutral cross-reference (no closure, multi-target,
   ;; symmetric canonicalised by :rel/id).
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
;; - unset directives keyed on a setter-ref attribute, plus
;; - explicit relation-backed unsets (:unsuperseded, :unduplicate).
(def setter-scoped-command-ids
  (into #{:unsuperseded :unsupersedes :unduplicate}
        (comp (filter #(str/starts-with? (name (:action %)) "unset"))
              (filter #(contains? setter-ref-attrs (:attr %)))
              (map :id))
        commands))
