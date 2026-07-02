;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.series
  "Patch series management."
  (:require [clojure.string :as str]
            [datalevin.core :as d]
            [bone.relations :as rel]
            [bone.tracking :as tracking]
            [taoensso.timbre :as log])
  (:import [java.util Date]))

(defn parse-seq
  "Parse \"2/5\" into [2 5], or nil."
  [s]
  (when s
    (let [[_ n m] (re-find #"(\d+)/(\d+)" s)]
      (when (and n m) [(parse-long n) (parse-long m)]))))

(defn series-id [topic sender total]
  (str (or topic "") "|" sender "|" total))

(defn- next-series-id [db topic sender total]
  (let [base     (series-id topic sender total)
        existing (d/q '[:find [?sid ...]
                        :in $ ?prefix
                        :where [?s :series/id ?sid]
                        [(clojure.string/starts-with? ?sid ?prefix)]]
                      db base)
        ;; "base" itself counts as suffix 1; "base#N" yields N.  The
        ;; starts-with? query prefix also matches longer totals
        ;; ("t|s|5" matches "t|s|55#3"), so re-check the boundary here.
        suffix-n (fn [sid]
                   (cond
                     (= sid base) 1
                     (str/starts-with? sid (str base "#"))
                     (some-> (re-find #"#(\d+)$" sid) second parse-long)))
        max-n    (reduce max 0 (keep suffix-n existing))]
    (if (zero? max-n) base (str base "#" (inc max-n)))))

(defn find-open-series [db topic sender total]
  (d/q '[:find ?s .
         :in $ ?topic ?sender ?exp
         :where
         [?s :series/topic ?topic] [?s :series/sender ?sender]
         [?s :series/expected ?exp] (not [?s :series/closed _])]
       db (or topic "") sender total))

(defn find-open-series-by-topic-sender [db topic sender]
  (when (and topic sender)
    (d/q '[:find [?s ...]
           :in $ ?topic ?sender
           :where
           [?s :series/topic ?topic] [?s :series/sender ?sender]
           (not [?s :series/closed _])]
         db topic sender)))

(defn find-open-series-via-parents
  "Open series reachable via parents' :report/series, restricted to
  `sender`/`total`.  Threading is more reliable than per-message
  :topic, which can diverge across patches in a series."
  [db parent-report-eids sender total]
  (when (seq parent-report-eids)
    (d/q '[:find ?s .
           :in $ [?r ...] ?sender ?total
           :where
           [?r :report/series ?s]
           [?s :series/sender ?sender]
           [?s :series/expected ?total]
           (not [?s :series/closed _])]
         db parent-report-eids sender total)))

(defn create-series! [conn topic sender total]
  (let [sid (next-series-id (d/db conn) topic sender total)]
    (d/transact! conn [{:series/id       sid
                        :series/topic    (or topic "")
                        :series/sender   sender
                        :series/expected total}])
    (d/entid (d/db conn) [:series/id sid])))

(defn close-series! [conn series-eid email-eid]
  (d/transact! conn [{:db/id series-eid :series/closed email-eid}]))

(defn- supersede-series-reports!
  "Close all open reports in a superseded series with reason :superseded.
  One transaction + one bump for the whole batch."
  [conn series-eid email-eid]
  (let [db (d/db conn)
        report-eids (d/q '[:find [?r ...]
                           :in $ ?s
                           :where [?s :series/patches ?r]
                           (not [?r :report/closed _])]
                         db series-eid)]
    (when (seq report-eids)
      (d/transact! conn (mapv (fn [rid]
                                {:db/id rid
                                 :report/closed email-eid
                                 :report/close-reason :superseded})
                              report-eids))
      (tracking/bump-report-updated! conn report-eids))))

(defn add-patch-to-series! [conn series-eid report-eid email]
  (let [existing (d/q '[:find [?r ...]
                         :in $ ?s ?self
                         :where [?s :series/patches ?r]
                         [(not= ?r ?self)]]
                       (d/db conn) series-eid report-eid)]
    (d/transact! conn [[:db/add series-eid :series/patches report-eid]
                       {:db/id report-eid :report/series series-eid}])
    ;; Cross-link siblings via :related-to (idempotent via :rel/id).
    ;; Siblings that gain a link must re-export -- contextual change
    ;; (a new patch joined the series), not a state change.
    (let [opts {:from-eid  report-eid
                :kind      :related-to
                :setter    (:email/author-address email)
                :email-eid (:db/id email)
                :posed-at  (or (:email/date-sent email) (Date.))
                :value     nil}
          linked (filterv (fn [sibling-eid]
                            (rel/pose-if-absent! conn (assoc opts :to-eid sibling-eid)))
                          existing)]
      (when (seq linked)
        (tracking/bump-report-updated! conn linked false)))))

(defn set-cover-letter! [conn series-eid email-eid]
  (d/transact! conn [{:db/id series-eid :series/cover-letter email-eid}]))

(defn manage-series!
  "After creating a patch report, manage its series membership."
  [conn report-eid email report-info from-addr parent-report-eids]
  (when-let [[n m] (parse-seq (:patch-seq report-info))]
    (let [email-eid (:db/id email)
          topic  (:topic report-info)
          db     (d/db conn)
          existing-series (when topic
                            (find-open-series-by-topic-sender db topic from-addr))
          restart? (and (seq existing-series)
                        (or (zero? n)
                            (and (= 1 n)
                                 (let [existing-seqs
                                       (d/q '[:find [?seq ...]
                                              :in $ [?s ...]
                                              :where [?s :series/patches ?r]
                                              [?r :report/patch-seq ?seq]]
                                            db existing-series)]
                                   (some #(str/starts-with? % "1/") existing-seqs)))))
          ancestor? (when restart?
                      (let [old-mids (into
                                      (set (d/q '[:find [?mid ...]
                                                  :in $ [?s ...]
                                                  :where [?s :series/patches ?r]
                                                  [?r :report/message-id ?mid]]
                                                db existing-series))
                                      (d/q '[:find [?mid ...]
                                             :in $ [?s ...]
                                             :where [?s :series/cover-letter ?e]
                                             [?e :email/message-id ?mid]]
                                           db existing-series))
                            parent-mids (when (seq parent-report-eids)
                                          (set (d/q '[:find [?mid ...]
                                                      :in $ [?r ...]
                                                      :where [?r :report/message-id ?mid]]
                                                    db parent-report-eids)))]
                        (some old-mids parent-mids)))]
      (when (and restart? ancestor?)
        (doseq [sid existing-series]
          (close-series! conn sid email-eid)
          (when (:version report-info)
            (supersede-series-reports! conn sid email-eid))
          (log/info "Auto-closed series"
                    (pr-str (:series/id (d/pull (d/db conn) [:series/id] sid)))
                    "(superseded)")))
      (let [series-eid (or (when (pos? n)
                             (find-open-series-via-parents
                              (d/db conn) parent-report-eids from-addr m))
                           (find-open-series (d/db conn) topic from-addr m)
                           (let [sid (create-series! conn topic from-addr m)]
                             (log/info "New series:"
                                       (pr-str (series-id topic from-addr m))
                                       "(expecting" m "patches)")
                             sid))]
        ;; Series supersession is implicit: the old series carries
        ;; :series/closed pointing to the triggering email; the new
        ;; series lives in the same thread.
        (if (zero? n)
          (do (set-cover-letter! conn series-eid email-eid)
              (d/transact! conn [{:db/id report-eid :report/series series-eid}]))
          (add-patch-to-series! conn series-eid report-eid email))))))
