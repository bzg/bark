;; bark-series.clj — Patch series management.
;;
;; Pure functions: parse-seq, series-id
;; Effectful:      manage-series!, create-series!, close-series!, etc.
;;
;; Usage: (load-file "scripts/bark-series.clj")

(require '[clojure.string :as str])

;; ---------------------------------------------------------------------------
;; Series helpers (pure)
;; ---------------------------------------------------------------------------

(defn parse-seq
  "Parse \"2/5\" into [2 5], or nil."
  [s]
  (when s
    (let [[_ n m] (re-find #"(\d+)/(\d+)" s)]
      (when (and n m)
        [(parse-long n) (parse-long m)]))))

(defn series-id
  "Compute a stable series identity from topic, sender, and total count."
  [topic sender total]
  (str (or topic "") "|" sender "|" total))

;; ---------------------------------------------------------------------------
;; Series DB operations
;; ---------------------------------------------------------------------------

(defn- next-series-id
  "Find the next available series-id by appending a revision suffix.
  E.g. parser|user@test.org|3, parser|user@test.org|3#2, etc."
  [db topic sender total]
  (let [base (series-id topic sender total)
        existing (d/q '[:find [?sid ...]
                        :in $ ?prefix
                        :where
                        [?s :series/id ?sid]
                        [(clojure.string/starts-with? ?sid ?prefix)]]
                      db base)]
    (if (empty? existing)
      base
      (str base "#" (inc (count existing))))))

(defn find-open-series
  "Find an open series matching topic+sender+total."
  [db topic sender total]
  (d/q '[:find ?s .
         :in $ ?topic ?sender ?exp
         :where
         [?s :series/topic ?topic]
         [?s :series/sender ?sender]
         [?s :series/expected ?exp]
         (not [?s :series/closed _])]
       db (or topic "") sender total))

(defn find-open-series-by-topic-sender
  "Find any open series matching topic+sender (any total)."
  [db topic sender]
  (when (and topic sender)
    (d/q '[:find [?s ...]
           :in $ ?topic ?sender
           :where
           [?s :series/topic ?topic]
           [?s :series/sender ?sender]
           (not [?s :series/closed _])]
         db topic sender)))

(defn create-series!
  "Create a new series entity. Returns the series eid."
  [conn topic sender total]
  (let [sid (next-series-id (d/db conn) topic sender total)]
    (d/transact! conn [{:series/id       sid
                        :series/topic    (or topic "")
                        :series/sender   sender
                        :series/expected total}])
    (d/q '[:find ?s . :in $ ?sid :where [?s :series/id ?sid]]
         (d/db conn) sid)))

(defn close-series!
  "Close a series, setting :series/closed to the given email eid."
  [conn series-eid email-eid]
  (d/transact! conn [{:db/id series-eid :series/closed email-eid}]))

(defn add-patch-to-series!
  "Link a patch report to a series and set :report/series back-ref."
  [conn series-eid report-eid]
  (d/transact! conn [[:db/add series-eid :series/patches report-eid]
                     {:db/id report-eid :report/series series-eid}]))

(defn set-cover-letter!
  "Set the cover letter (0/N email) on a series."
  [conn series-eid email-eid]
  (d/transact! conn [{:db/id series-eid :series/cover-letter email-eid}]))

(defn manage-series!
  "After creating a patch report, manage its series membership.
  - Finds or creates the series
  - If this is a cover letter (0/M) or a duplicate 1/M arriving as a
    descendant of an old series, close the old one and start fresh
  - Adds the patch to the series (or sets cover letter for 0/M)"
  [conn report-eid email-eid report-info from-addr parent-report-eids]
  (when-let [[n m] (parse-seq (:patch-seq report-info))]
    (let [topic  (:topic report-info)
          db     (d/db conn)
          ;; Find existing open series for this topic+sender
          existing-series (when topic
                            (find-open-series-by-topic-sender db topic from-addr))
          ;; 0/M always signals a restart (new cover letter).
          ;; 1/M signals restart only if the existing series already
          ;; contains a 1/* patch (i.e. the sequence is truly restarting).
          restart? (and (seq existing-series)
                        (or (zero? n)
                            (and (= 1 n)
                                 (let [existing-seqs
                                       (d/q '[:find [?seq ...]
                                              :in $ [?s ...]
                                              :where
                                              [?s :series/patches ?r]
                                              [?r :report/patch-seq ?seq]]
                                            db existing-series)]
                                   (some #(str/starts-with? % "1/") existing-seqs)))))
          ;; Is this a descendant of any of those existing series' patches?
          ancestor? (when restart?
                      (let [old-mids (into
                                      ;; patch report message-ids
                                      (set (d/q '[:find [?mid ...]
                                                  :in $ [?s ...]
                                                  :where [?s :series/patches ?r]
                                                  [?r :report/message-id ?mid]]
                                                db existing-series))
                                      ;; cover letter email message-ids
                                      (d/q '[:find [?mid ...]
                                             :in $ [?s ...]
                                             :where [?s :series/cover-letter ?e]
                                             [?e :email/message-id ?mid]]
                                           db existing-series))
                            parent-mids (set (keep (fn [rid]
                                                     (d/q '[:find ?mid .
                                                            :in $ ?r
                                                            :where [?r :report/message-id ?mid]]
                                                          db rid))
                                                   parent-report-eids))]
                        (some old-mids parent-mids)))]
      ;; Close old series if this is a restart and a descendant
      (when (and restart? ancestor?)
        (doseq [sid existing-series]
          (close-series! conn sid email-eid)
          (println (str "    → auto-closed series "
                        (pr-str (:series/id (d/pull (d/db conn) [:series/id] sid)))
                        " (superseded)"))))
      ;; Find or create the series for this patch
      (let [series-eid (or (find-open-series (d/db conn) topic from-addr m)
                           (let [sid (create-series! conn topic from-addr m)]
                             (println (str "    → new series: "
                                           (pr-str (series-id topic from-addr m))
                                           " (expecting " m " patches)"))
                             sid))]
        (if (zero? n)
          (set-cover-letter! conn series-eid email-eid)
          (add-patch-to-series! conn series-eid report-eid))))))
