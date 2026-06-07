(ns bone.digest-test
  "Integration tests for bone.digest.
  Creates a temporary Datalevin DB, inserts test emails, processes them
  via bone.digest/process-email!, and verifies reports, commands,
  threading, votes, roles, and permissions.

  Ported from test/bone-digest-test.clj (bb version)."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [datalevin.core :as d]
            [bone.commands :as commands]
            [bone.common :as common]
            [bone.digest :as digest]
            [bone.roles :as roles]
            [bone.test-helpers :as th])
  (:import [java.text SimpleDateFormat]
           [java.util TimeZone]))

(use-fixtures :once th/with-temp-failures-file)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- parse-date-iso [s]
  (let [fmt (doto (SimpleDateFormat. "yyyy-MM-dd")
              (.setTimeZone (TimeZone/getTimeZone "UTC")))]
    (.parse fmt s)))

(defn- get-report
  "Pull a report by message-id, including the qualified-relation reverse refs.
  After the qualified-links refactor: :report/superseded-by(-target) and
  :report/related are gone -- see :rel/_from / :rel/_to instead."
  [db message-id]
  (d/pull db
          '[:report/type :report/version :report/topic-value
            :report/patch-seq :report/patch-source :report/message-id
            {:report/patches [:patch/filename :patch/source :patch/author
                              :patch/subject :patch/date :patch/text]}
            :report/acked :report/owned :report/closed
            :report/close-reason
            :report/urgent :report/important
            :report/deadline-value :report/expiry-value
            :report/acked-address :report/owned-address
            :report/closed-address :report/urgent-address
            :report/important-address
            {:report/descendants [:email/message-id]}
            {:rel/_from [:rel/kind :rel/active? :rel/setter :rel/posed-at :rel/value
                         {:rel/to [:db/id :report/type :report/message-id
                                   {:report/email [:email/subject]}]}]}
            {:rel/_to [:rel/kind :rel/active? :rel/setter :rel/posed-at :rel/value
                       {:rel/from [:db/id :report/type :report/message-id
                                   {:report/email [:email/subject]}]}]}
            {:report/series [:series/id :series/expected :series/closed
                             {:series/patches [:db/id]}
                             {:series/cover-letter [:email/message-id]}]}
            {:report/email [:email/subject :email/author-address
                            :email/headers-edn]}]
          [:report/message-id message-id]))

;; --- Test helpers to aggregate :rel/* facts for assertion convenience ---

(defn- active-out-rels
  "All active outgoing relations of `kind` from `report-pull`."
  [report-pull kind]
  (->> (:rel/_from report-pull)
       (filter :rel/active?)
       (filter #(= kind (:rel/kind %)))))

(defn- all-related
  "Aggregate-related accessor: collects every report linked via
  :related-to (canonical may live on either side), :resolves/:resolved-by,
  or :supersedes/:superseded-by -- mirrors what a bidirectional
  :report/related ref would surface."
  [report-pull]
  (let [outs (->> (:rel/_from report-pull)
                  (filter :rel/active?)
                  (map :rel/to))
        ins  (->> (:rel/_to report-pull)
                  (filter :rel/active?)
                  (map :rel/from))]
    (vec (distinct (concat outs ins)))))

(defn- superseded-by-target [report-pull]
  (some-> (first (active-out-rels report-pull :supersedes)) :rel/to))

(defn- report-exists? [db message-id]
  (some? (d/q '[:find ?r . :in $ ?mid :where [?r :report/message-id ?mid]]
              db message-id)))

(defn- get-votes
  "Return all votes for a report as a seq of {:value :voter} maps."
  [db message-id]
  (let [rid (d/q '[:find ?r . :in $ ?mid :where [?r :report/message-id ?mid]]
                 db message-id)]
    (when rid
      (mapv (fn [[val voter]]
              {:value val :voter voter})
            (d/q '[:find ?val ?voter :in $ ?r
                   :where [?v :vote/report ?r] [?v :vote/value ?val] [?v :vote/voter ?voter]]
                 db rid)))))

(defn- get-series-by-id [db sid]
  (when-let [eid (d/q '[:find ?s . :in $ ?sid :where [?s :series/id ?sid]] db sid)]
    (d/pull db
            '[:series/id :series/topic :series/sender :series/expected
              :series/closed :series/patches
              {:series/cover-letter [:email/message-id]}]
            eid)))

(defn- series-patch-count [db sid]
  (when-let [eid (d/q '[:find ?s . :in $ ?sid :where [?s :series/id ?sid]] db sid)]
    (count (d/q '[:find [?r ...] :in $ ?s :where [?s :series/patches ?r]]
                db eid))))

(defn- get-participant
  "Return the participant entity for a given source and email address, or nil."
  [db source-name email]
  (let [k (str source-name ":" (.toLowerCase email))]
    (when-let [eid (d/q '[:find ?e . :in $ ?k :where [?e :participant/key ?k]] db k)]
      (d/pull db '[:participant/key :participant/source :participant/email
                   :participant/name :participant/since :participant/contributor-since]
              eid))))

;; ---------------------------------------------------------------------------
;; Fixture data
;; ---------------------------------------------------------------------------

(def source-map {"direct"      {:admin "admin@test.org"
                                :source-type :mailbox
                                :to "direct@test.org"}
                 "public-list" {:admin "admin@test.org"
                                :source-type :mailing-list
                                :list "list.test.org"}
                 "no-triggers" {:admin "admin@test.org"
                                :source-type :mailbox
                                :to "no-triggers@test.org"
                                :patch-triggers? false}})

(def sources [{:name "public-list"
               :list "list.test.org"}
              {:name "direct"
               :to "direct@test.org"}
              {:name "no-triggers"
               :to "no-triggers@test.org"
               :patch-triggers? false}])

;; ---------------------------------------------------------------------------
;; DB setup / teardown
;; ---------------------------------------------------------------------------

(defn- setup-db! []
  (let [db-path (str "/tmp/bone-test-" (System/currentTimeMillis))
        conn    (d/get-conn db-path common/bone-schema)]
    ;; Seed an initial open tenure (from = nil, order = 0) making
    ;; admin@test.org the lead maintainer on both sources.
    (d/transact! conn [{:maint-tenure/source "direct"
                        :maint-tenure/email  "admin@test.org"
                        :maint-tenure/order  0}
                       {:maint-tenure/source "public-list"
                        :maint-tenure/email  "admin@test.org"
                        :maint-tenure/order  0}
                       {:maint-tenure/source "no-triggers"
                        :maint-tenure/email  "admin@test.org"
                        :maint-tenure/order  0}])
    ;; Insert test emails. Fixtures only set :email/from-address /
    ;; :email/from-name; mirror them into :email/author-* (the cleaned
    ;; identity attrs that downstream code reads) so the fixtures stay
    ;; concise and a fixture explicitly setting :email/author-* (e.g.
    ;; for a DMARC-munged scenario) overrides this default.
    (let [emails (edn/read-string (slurp "resources/emails.edn"))]
      (doseq [email emails]
        (d/transact!
         conn
         [(cond-> email
            (and (:email/from-address email) (not (:email/author-address email)))
            (assoc :email/author-address (:email/from-address email))
            (and (:email/from-name email) (not (:email/author-name email)))
            (assoc :email/author-name (:email/from-name email)))])))
    {:conn conn :db-path db-path}))

(def ^:private teardown! th/teardown!)

(defn- process-all-emails!
  "Fetch all emails sorted by ingested-at, process each via digest/process-email!."
  [conn]
  (let [db     (d/db conn)
        emails (d/q '[:find [(pull ?e [:db/id :email/ingested-at :email/date-sent]) ...]
                      :where [?e :email/message-id _]]
                    db)
        sorted (sort-by #(or (:email/ingested-at %) (:email/date-sent %) (java.util.Date. 0))
                        emails)]
    (doseq [{eid :db/id} sorted]
      (let [email (d/pull (d/db conn) digest/email-pull-pattern eid)]
        (try
          (digest/process-email! conn source-map sources email)
          (catch Exception e
            (println "Error processing" (:email/message-id email) (.getMessage e))))))))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(deftest detect-vote-test
  (testing "Up votes"
    (is (= :up (commands/detect-vote "+1")))
    (is (= :up (commands/detect-vote "+1\n")))
    (is (= :up (commands/detect-vote "+1 thanks")))
    (is (= :up (commands/detect-vote "+1, great idea")))
    (is (= :up (commands/detect-vote "+1.")))
    (is (= :up (commands/detect-vote "+1!")))
    (is (= :up (commands/detect-vote "1+")))
    (is (= :up (commands/detect-vote "1+ ")))
    (is (= :up (commands/detect-vote "I agree\n+1\nthanks"))))

  (testing "Down votes"
    (is (= :down (commands/detect-vote "-1")))
    (is (= :down (commands/detect-vote "1-")))
    (is (= :down (commands/detect-vote "-1 nope")))
    (is (= :down (commands/detect-vote "1-."))))

  (testing "Null votes"
    (is (= :null (commands/detect-vote "+0")))
    (is (= :null (commands/detect-vote "-0")))
    (is (= :null (commands/detect-vote "0+")))
    (is (= :null (commands/detect-vote "0-")))
    (is (= :null (commands/detect-vote "+0, meh"))))

  (testing "Non-votes"
    (is (nil? (commands/detect-vote "+10")))
    (is (nil? (commands/detect-vote "+1abc")))
    (is (nil? (commands/detect-vote "1+2")))
    (is (nil? (commands/detect-vote "-10")))
    (is (nil? (commands/detect-vote "-1a")))
    (is (nil? (commands/detect-vote "1-2")))
    (is (nil? (commands/detect-vote "+0x")))
    (is (nil? (commands/detect-vote "nothing here")))
    (is (nil? (commands/detect-vote "")))
    (is (nil? (commands/detect-vote nil)))))

(deftest word-scope-denial-recording
  (testing "non-maintainer bareword on a source that overrides :closed to :maintainer"
    (let [filter-words      (var-get #'bone.commands/filter-words-by-scope)
          describe-word     (var-get #'bone.commands/describe-denied-word)
          recorded          (atom [])]
      (testing "describe-denied-word labels"
        (is (= "Closed." (describe-word :report/closed)))
        (is (= "Acked." (describe-word :report/acked)))
        (is (= "Urgent." (describe-word :report/urgent))))
      (with-redefs [commands/record-failure! (fn [entry] (swap! recorded conj entry))]
        (let [word-result {:report/closed true :report/acked true}
              overrides   {:closed {:scope :maintainer}}
              failure-ctx {:source "src" :from-addr "user@test" :email-date nil :report-mid "<r@test>"}
              filtered    (filter-words word-result overrides false failure-ctx)]
          (is (= {:report/acked true} filtered)
              "acked (default :user scope) survives, closed (maintainer-only override) is dropped")
          (is (= 1 (count @recorded)) "exactly one failure recorded")
          (let [entry (first @recorded)]
            (is (= :insufficient-scope (:reason entry)))
            (is (= :maintainers (:audience entry)))
            (is (= "Closed." (:command entry)))
            (is (= "user@test" (:from-addr entry)))
            (is (= "src" (:source entry)))))))))

(deftest digest-integration-test
  (let [{:keys [conn] :as ctx} (setup-db!)]
    (try
      (process-all-emails! conn)
      (let [db (d/db conn)]

        ;; --- Roles ---
        (testing "Roles (final state)"
          (let [ts (roles/get-tenures db "direct")]
            (is (= "admin@test.org" (common/lead-maintainer ts)))
            (is (common/maintainer? ts "maint@test.org"))))

        ;; --- Bug 02 lifecycle ---
        (testing "Bug 02 lifecycle"
          (let [r (get-report db "<02@test.org>")]
            (is (= :bug (:report/type r)))
            (is (= "9.7" (:report/topic-value r)))
            (is (some? (:report/acked r)))
            (is (some? (:report/owned r)))
            (is (some? (:report/closed r)))
            (is (= :resolved (:report/close-reason r)))
            (is (some? (:report/urgent r)))
            (is (= 3 (count (:report/descendants r))))))

        ;; --- Bug 03 mailing list prefix ---
        (testing "Bug 03 mailing list prefix"
          (let [r (get-report db "<03@test.org>")]
            (is (= :bug (:report/type r)))
            (is (nil? (:report/topic-value r)))))

        ;; --- Patch 07 subject detection ---
        (testing "Patch 07 subject detection"
          (let [r (get-report db "<07@test.org>")]
            (is (= :patch (:report/type r)))
            (is (= "org-agenda" (:report/topic-value r)))
            (is (= "1/2" (:report/patch-seq r)))
            (is (contains? (set (:report/patch-source r)) :subject))
            (is (some? (:report/acked r)))
            (is (some? (:report/closed r)))
            (is (= :resolved (:report/close-reason r)))))

        ;; --- Patch 08 attachment ---
        (testing "Patch 08 attachment detection"
          (let [r (get-report db "<08@test.org>")]
            (is (= :patch (:report/type r)))
            (is (contains? (set (:report/patch-source r)) :attachment))))

        ;; --- Email 09 inline diff, no label: no report (inline-only is not a signal) ---
        (testing "Inline diff alone (no label, no attachment) does not create a patch report"
          (is (false? (report-exists? db "<09@test.org>"))))

        ;; --- POLL 11 votes ---
        (testing "POLL 11 votes"
          (let [r     (get-report db "<11@test.org>")
                votes (get-votes db "<11@test.org>")]
            (is (= :request (:report/type r)))
            (is (= 2 (count votes)))
            (is (= 1 (count (filter #(= :up (:value %)) votes))))
            (is (= 1 (count (filter #(= :down (:value %)) votes))))
            (is (= #{"voter1@test.org" "voter2@test.org"}
                   (set (map :voter votes))))
            (is (= 3 (count (:report/descendants r))))))

        ;; --- Email 15 request lifecycle ---
        (testing "Email 15 request lifecycle"
          (let [r (get-report db "<15@test.org>")]
            (is (= :request (:report/type r)))
            (is (some? (:report/closed r)))
            (is (= :resolved (:report/close-reason r)))))

        ;; --- ANN 17 canceled ---
        (testing "ANN 17 canceled"
          (let [r (get-report db "<17@test.org>")]
            (is (= :announcement (:report/type r)))
            (is (some? (:report/closed r)))
            (is (= :canceled (:report/close-reason r)))))

        ;; --- ANN 18 denied ---
        (testing "ANN 18 permission denied"
          (is (not (report-exists? db "<18@test.org>"))))

        ;; --- CHG 19 / REL 20 ---
        (testing "CHG 19 / REL 20 release closes change"
          (let [chg (get-report db "<19@test.org>")
                rel (get-report db "<20@test.org>")]
            (is (= :change (:report/type chg)))
            (is (= "9.8" (:report/version chg)))
            (is (some? (:report/closed chg)))
            (is (= :resolved (:report/close-reason chg)))
            (is (= :release (:report/type rel)))
            (is (= "9.8" (:report/version rel)))
            (is (some #(= "<19@test.org>" (:report/message-id %))
                      (all-related rel)))
            (is (some #(= "<20@test.org>" (:report/message-id %))
                      (all-related chg)))))

        ;; --- Email 21 (Ignore mechanism removed -- report is now created) ---
        (testing "Email 21 spam user creates report (no Ignore)"
          (is (report-exists? db "<21@test.org>")))

        ;; --- Bug 23 important ---
        (testing "Bug 23 important flag"
          (let [r (get-report db "<23@test.org>")]
            (is (= :bug (:report/type r)))
            (is (some? (:report/important r)))
            (is (= 3 (count (:report/descendants r))))))

        ;; --- FR 26 ---
        (testing "FR 26 request"
          (is (= :request (:report/type (get-report db "<26@test.org>")))))

        ;; --- ANN 27 mailing list ---
        (testing "ANN 27 mailing list + Archived-At"
          (let [r (get-report db "<27@test.org>")]
            (is (= :announcement (:report/type r)))
            (let [headers (some-> (get-in r [:report/email :email/headers-edn])
                                  edn/read-string)]
              (is (= "https://list.example.org/archive/27"
                     (get headers "Archived-At"))))))

        ;; --- Patch 28 mailing list prefix ---
        (testing "Patch 28 mailing list prefix"
          (let [r (get-report db "<28@test.org>")]
            (is (= :patch (:report/type r)))
            (is (= "refactor" (:report/topic-value r)))
            (is (= "2/3" (:report/patch-seq r)))))

        ;; --- Email 29 role control via mailing list (now allowed) ---
        (testing "Email 29 role control via mailing list"
          (is (common/maintainer? (roles/get-tenures db "direct") "evil@hacker.org")))

        ;; --- Email 30 bug via list ---
        (testing "Email 30 bug via list"
          (is (= :bug (:report/type (get-report db "<30@test.org>")))))

        ;; --- Emails 112-113: channel gating ---
        (testing "Channel gating on public-list source"
          (let [r (get-report db "<30@test.org>")]
            ;; Only the public reply (112) is a descendant;
            ;; the private reply (113) is excluded entirely.
            (is (= 1 (count (:report/descendants r))))
            (is (= "<112@test.org>"
                    (:email/message-id (first (:report/descendants r)))))
            ;; Only public commands apply: 112 → acked (Confirmed.)
            ;; Private commands (113 → Owned.) are now blocked.
            (is (some? (:report/acked r)))
            (is (nil? (:report/owned r)))))

        ;; --- Email 31 bypassing list ---
        (testing "Email 31 bypassing list"
          (is (not (report-exists? db "<31@test.org>"))))

        ;; --- Email 32 wrong list ---
        (testing "Email 32 wrong List-Post"
          (is (not (report-exists? db "<32@test.org>"))))

        ;; --- Email 33 admin direct (no bone-source fallback) ---
        (testing "Email 33 admin direct -- no report without list delivery"
          (is (not (report-exists? db "<33@test.org>"))))

        ;; --- Series v1 ---
        (testing "Series v1 emails 34-37"
          (let [v1 (get-series-by-id db "parser|user@test.org|3")]
            (is (= "parser" (:series/topic v1)))
            (is (= 3 (:series/expected v1)))
            (is (= "<34@test.org>" (get-in v1 [:series/cover-letter :email/message-id])))
            (is (= 3 (series-patch-count db "parser|user@test.org|3")))
            (is (some? (:series/closed v1))))
          (let [r35 (get-report db "<35@test.org>")
                r37 (get-report db "<37@test.org>")]
            (is (some? (:report/series r35)))
            (is (= "1/3" (:report/patch-seq r35)))
            ;; All 3 patches in the series are mutually related
            (is (some #(= "<36@test.org>" (:report/message-id %))
                      (all-related r35)))
            (is (some #(= "<37@test.org>" (:report/message-id %))
                      (all-related r35)))
            (is (some #(= "<35@test.org>" (:report/message-id %))
                      (all-related r37)))))

        ;; --- Series v2 ---
        (testing "Series v2 emails 38-39"
          (let [v2 (get-series-by-id db "parser|user@test.org|3#2")]
            (is (some? (:series/id v2)))
            (is (nil? (:series/closed v2)))
            (is (= 3 (:series/expected v2)))
            (is (= "<38@test.org>" (get-in v2 [:series/cover-letter :email/message-id])))
            (is (= 1 (series-patch-count db "parser|user@test.org|3#2"))))
          (let [r39 (get-report db "<39@test.org>")]
            (is (some? (:report/series r39)))
            (is (= "1/3" (:report/patch-seq r39)))))

        ;; --- Series with diverging per-patch :topic (emails 125-127) ---
        (testing "Series held together by threading despite divergent topics"
          ;; Cover letter and patches each have a different `subdir:`
          ;; colon-prefix, so the per-message :topic differs across
          ;; reports. Threading via In-Reply-To must keep all patches
          ;; in the cover letter's series.
          (let [s (get-series-by-id db "orgweb|kana@test.org|2")]
            (is (some? (:series/id s)))
            (is (= 2 (:series/expected s)))
            (is (= "<125@test.org>" (get-in s [:series/cover-letter :email/message-id])))
            (is (= 2 (series-patch-count db "orgweb|kana@test.org|2"))))
          (let [r126 (get-report db "<126@test.org>")
                r127 (get-report db "<127@test.org>")]
            (is (some? (:report/series r126)))
            (is (some? (:report/series r127)))
            (is (= "orgweb|kana@test.org|2"
                   (:series/id (:report/series r126))))
            (is (= "orgweb|kana@test.org|2"
                   (:series/id (:report/series r127))))))

        ;; --- :patch-triggers? false on source (emails 128-129) ---
        (testing "Source with :patch-triggers? false skips auto-credit"
          (let [bug   (get-report db "<128@test.org>")
                patch (get-report db "<129@test.org>")]
            (is (= :bug   (:report/type bug)))
            (is (= :patch (:report/type patch)))
            ;; The :resolves cross-link is still recorded -- only the
            ;; side effects on the bug are gated.
            (is (some #(= "<128@test.org>" (:report/message-id %))
                      (all-related patch))
                ":resolves cross-link is still recorded")
            ;; No auto-credit fired on the bug.
            (is (nil? (:report/acked bug))
                "auto-acked must NOT fire when :patch-triggers? is false")
            (is (nil? (:report/owned bug))
                "auto-owned must NOT fire when :patch-triggers? is false")
            (is (nil? (:report/closed bug))
                "the bug must remain open after the patch arrives")))

        ;; --- Email 40 patch related to bug ---
        (testing "Email 40 patch related to bug"
          (let [patch (get-report db "<40@test.org>")
                bug   (get-report db "<23@test.org>")]
            (is (= :patch (:report/type patch)))
            (is (some #(= "<23@test.org>" (:report/message-id %))
                      (all-related patch)))
            (is (some #(= "<40@test.org>" (:report/message-id %))
                      (all-related bug)))))

        ;; --- Emails 41-42 add then remove maintainer ---
        (testing "Emails 41-42 add then remove maintainer"
          (is (not (common/maintainer? (roles/get-tenures db "direct") "maint2@test.org"))))

        ;; --- Email 44 unignore (Ignore mechanism removed) ---

        ;; --- Email 45 maintainer adds peer ---
        (testing "Email 45 maintainer adds peer"
          (is (common/maintainer? (roles/get-tenures db "direct") "maint3@test.org")))

        ;; --- Email 47 user can't add maintainer ---
        (testing "Email 47 user can't add maintainer"
          (is (not (common/maintainer? (roles/get-tenures db "direct") "user@test.org"))))

        ;; --- Emails 48-49 command with semicolon ---
        (testing "Emails 48-49 command with semicolon"
          (let [r (get-report db "<48@test.org>")]
            (is (= :bug (:report/type r)))
            (is (some? (:report/acked r)))))

        ;; --- Emails 50-51 inline command ignored ---
        (testing "Emails 50-51 inline command ignored"
          (let [r (get-report db "<50@test.org>")]
            (is (= :bug (:report/type r)))
            (is (nil? (:report/closed r)))))

        ;; --- Emails 52-53 Closed. on request ---
        (testing "Emails 52-53 Closed. on request"
          (let [r (get-report db "<52@test.org>")]
            (is (= :request (:report/type r)))
            (is (some? (:report/closed r)))))

        ;; --- Emails 54-55 commands on announcement ---
        (testing "Emails 54-55 commands on announcement"
          (let [r (get-report db "<54@test.org>")]
            (is (= :announcement (:report/type r)))
            (is (nil? (:report/acked r)))
            (is (nil? (:report/owned r)))
            (is (some? (:report/urgent r)))))

        ;; --- Email 59 properly tagged [BUG] ---
        (testing "Email 59 properly tagged [BUG]"
          (is (= :bug (:report/type (get-report db "<59@test.org>")))))

        ;; --- Email 60 [ANNOUNCEMENT] long form ---
        (testing "Email 60 [ANNOUNCEMENT] long form"
          (is (= :announcement (:report/type (get-report db "<60@test.org>")))))

        ;; --- Emails 71-73 permission denials ---
        (testing "Emails 71-73 permission denials"
          (is (not (report-exists? db "<71@test.org>")))
          (is (not (report-exists? db "<72@test.org>")))
          (is (not (report-exists? db "<73@test.org>"))))

        ;; --- Email 61 References-only threading ---
        (testing "Email 61 References-only threading"
          (is (some? (:report/acked (get-report db "<59@test.org>")))))

        ;; --- Email 62 deep thread ---
        (testing "Email 62 deep thread command"
          (let [r (get-report db "<59@test.org>")]
            (is (some? (:report/closed r)))
            (is (>= (count (:report/descendants r)) 2))))

        ;; --- Email 63 orphan ---
        (testing "Email 63 orphan"
          (is (not (report-exists? db "<63@test.org>"))))

        ;; --- Email 64 duplicate tag ---
        (testing "Email 64 [BUG] reply creates own report"
          (is (report-exists? db "<64@test.org>")))

        ;; --- Emails 201-202 malformed labels create no report ---
        (testing "Email 201 lowercase [bug] is rejected"
          (is (not (report-exists? db "<201@test.org>"))))
        (testing "Email 202 glued [BUG]subject is rejected"
          (is (not (report-exists? db "<202@test.org>"))))

        ;; --- Emails 65-66 HTML body fallback ---
        (testing "Emails 65-66 HTML body fallback"
          (let [r (get-report db "<65@test.org>")]
            (is (= :bug (:report/type r)))
            (is (some? (:report/acked r)))))

        ;; --- Email 67 Ignore mechanism removed ---

        ;; --- Emails 68-69 series without cover letter ---
        (testing "Emails 68-69 series without cover letter"
          (let [r68 (get-report db "<68@test.org>")
                r69 (get-report db "<69@test.org>")]
            (is (= :patch (:report/type r68)))
            (is (= "1/2" (:report/patch-seq r68)))
            (is (= "2/2" (:report/patch-seq r69)))
            (is (some? (:report/series r68)))
            (is (some? (:report/series r69)))
            ;; Patches in the same series are related to each other
            (is (some #(= "<69@test.org>" (:report/message-id %))
                      (all-related r68)))
            (is (some #(= "<68@test.org>" (:report/message-id %))
                      (all-related r69)))))

        ;; --- Email 70 different sender ---
        (testing "Email 70 different sender same topic"
          (let [r70 (get-report db "<70@test.org>")
                r68 (get-report db "<68@test.org>")]
            (is (= :patch (:report/type r70)))
            (is (not= (get-in r68 [:report/series :series/id])
                      (get-in r70 [:report/series :series/id])))))

        ;; --- POLL 75 vote variants ---
        (testing "POLL 75 vote format variants"
          (let [r     (get-report db "<75@test.org>")
                votes (get-votes db "<75@test.org>")]
            (is (= :request (:report/type r)))
            (is (= 4 (count votes)))
            (is (= 2 (count (filter #(= :up (:value %)) votes))))
            (is (= 1 (count (filter #(= :down (:value %)) votes))))
            (is (= 1 (count (filter #(= :null (:value %)) votes))))
            (is (not (contains? (set (map :voter votes)) "admin@test.org")))))

        ;; --- Directive unit tests ---
        (testing "detect-lines"
          (is (= [{:action :set :attr :report/acked :email-address "a@b.com" :scope :maintainer :id :acked-by}]
                 (commands/detect-lines :bug "Acked-by: a@b.com\n")))
          (is (= [{:action :set :attr :report/owned :email-address "x@y.com" :scope :maintainer :id :owned-by}]
                 (commands/detect-lines :bug "Owned-by: x@y.com\nIgnore this line\n")))
          (is (= [{:action :unset :attr :report/acked :scope :setter-or-maintainer :id :unacked}]
                 (commands/detect-lines :bug "Not acked\n")))
          (is (= [{:action :unset :attr :report/urgent :scope :setter-or-maintainer :id :unurgent}]
                 (commands/detect-lines :bug "Not urgent\n")))
          (is (= [{:action :unset :attr :report/important :scope :setter-or-maintainer :id :unimportant}]
                 (commands/detect-lines :bug "Not important\n")))
          (is (= [{:action :set-deadline :date (parse-date-iso "2026-06-15") :scope :user :id :deadline}]
                 (commands/detect-lines :bug "Deadline: 2026-06-15\n")))
          (is (= [{:action :unset-deadline :scope :setter-or-maintainer :id :undeadline}]
                 (commands/detect-lines :bug "No deadline\n")))
          (is (= [{:action :unset-topic :scope :setter-or-maintainer :id :untopic}]
                 (commands/detect-lines :bug "No topic\n")))
          (is (= [{:action :set-topic :topic "my-topic" :scope :user :id :topic}]
                 (commands/detect-lines :bug "Topic: my-topic\n")))
          (is (= [{:action :set :attr :report/acked :email-address "a@b.com" :scope :maintainer :id :acked-by}
                  {:action :set-deadline :date (parse-date-iso "2026-07-01") :scope :user :id :deadline}
                  {:action :set-topic :topic "urgent-fix" :scope :user :id :topic}]
                 (commands/detect-lines :bug "Acked-by: a@b.com\nDeadline: 2026-07-01\nTopic: urgent-fix\n")))
          (is (= [{:action :set :attr :report/owned :email-address "x@y.com" :scope :maintainer :id :owned-by}]
                 (commands/detect-lines :bug "Thanks for the report.\nOwned-by: x@y.com\nWill look into it.\n")))
          ;; RFC 5322 "Display Name <addr>" format
          (is (= [{:action :set :attr :report/owned :email-address "x@y.com" :scope :maintainer :id :owned-by}]
                 (commands/detect-lines :bug "Owned-by: Some User <x@y.com>\n")))
          ;; Bracketed address without display name: angle brackets must be stripped
          (is (= [{:action :set :attr :report/owned :email-address "x@y.com" :scope :maintainer :id :owned-by}]
                 (commands/detect-lines :bug "Owned-by: <x@y.com>\n")))
          ;; Address must contain a dot in the domain part
          (is (= [] (commands/detect-lines :bug "Owned-by: alice@localhost\n")))
          ;; Address must not contain stray @ characters
          (is (= [] (commands/detect-lines :bug "Owned-by: alice@@host.com\n")))
          (is (= [] (commands/detect-lines :bug "Just a normal reply.\n")))
          (is (nil? (commands/detect-lines :bug nil)))
          ;; Expiry directive
          (is (= [{:action :set-expiry :date (parse-date-iso "2026-09-01") :scope :user :id :expiry}]
                 (commands/detect-lines :bug "Expiry: 2026-09-01\n")))
          (is (= [{:action :unset-expiry :scope :setter-or-maintainer :id :unexpiry}]
                 (commands/detect-lines :bug "No expiry\n")))
          ;; "Expiry: deadline" is no longer a valid command (use :inactive-after :deadline in config)
          (is (= [] (commands/detect-lines :bug "Expiry: deadline\n")))
          ;; Deadline with duration (relative to email date)
          (let [email-date (parse-date-iso "2026-01-10")
                result (commands/detect-lines :bug "Deadline: 30d\n" nil email-date)]
            (is (= 1 (count result)))
            (is (= :set-deadline (:action (first result))))
            (is (= (parse-date-iso "2026-02-09") (:date (first result)))))
          ;; Expiry with duration (relative to email date)
          (let [email-date (parse-date-iso "2026-01-03")
                result (commands/detect-lines :bug "Expiry: 3d\n" nil email-date)]
            (is (= 1 (count result)))
            (is (= :set-expiry (:action (first result))))
            (is (= (parse-date-iso "2026-01-06") (:date (first result)))))
          ;; Compound duration
          (let [email-date (parse-date-iso "2026-01-01")
                result (commands/detect-lines :bug "Expiry: 1m 2w\n" nil email-date)]
            (is (= 1 (count result)))
            (is (= (parse-date-iso "2026-02-14") (:date (first result)))))
          ;; Expiry not applicable to announcements
          (is (= [] (commands/detect-lines :announcement "Expiry: 2026-09-01\n"))))

        ;; --- resolve-commands unit tests ---
        (testing "resolve-commands"
          (is (= {:set {} :unset #{:report/acked}}
                 (commands/resolve-commands [{:action :set :attr :report/acked :email-address "a@b.com"}
                                             {:action :unset :attr :report/acked}])))
          (is (= {:set {:report/acked "a@b.com"} :unset #{}}
                 (commands/resolve-commands [{:action :unset :attr :report/acked}
                                             {:action :set :attr :report/acked :email-address "a@b.com"}])))
          (is (= {:set {} :unset #{} :undeadline? true}
                 (commands/resolve-commands [{:action :set-deadline :date (parse-date-iso "2026-06-01")}
                                             {:action :unset-deadline}])))
          (is (= {:set {} :unset #{} :deadline (parse-date-iso "2026-06-01")}
                 (commands/resolve-commands [{:action :unset-deadline}
                                             {:action :set-deadline :date (parse-date-iso "2026-06-01")}])))
          (is (= {:set {} :unset #{} :topic "second"}
                 (commands/resolve-commands [{:action :set-topic :topic "first"}
                                             {:action :set-topic :topic "second"}])))
          ;; Expiry resolve
          (is (= {:set {} :unset #{} :expiry (parse-date-iso "2026-09-01")}
                 (commands/resolve-commands [{:action :set-expiry :date (parse-date-iso "2026-09-01")}])))
          (is (= {:set {} :unset #{} :unexpiry? true}
                 (commands/resolve-commands [{:action :set-expiry :date (parse-date-iso "2026-09-01")}
                                             {:action :unset-expiry}])))
          ;; Untopic resolve
          (is (= {:set {} :unset #{} :untopic? true}
                 (commands/resolve-commands [{:action :set-topic :topic "foo"}
                                             {:action :unset-topic}]))))

        ;; --- Bug 81 directives ---
        (testing "Bug 81 Acked-by"
          (let [r (get-report db "<81@test.org>")]
            (is (nil? (:report/acked r)))
            (is (nil? (:report/acked-address r)))
            (is (some? (:report/owned r)))
            (is (= "fixer@test.org" (:report/owned-address r)))))

        ;; --- Bug 81 user directive denied ---
        (testing "Bug 81 user directive denied"
          (is (nil? (:report/closed (get-report db "<81@test.org>")))))

        ;; --- Bug 86 last-one-wins ---
        (testing "Bug 86 last-one-wins"
          (is (nil? (:report/acked (get-report db "<86@test.org>")))))

        ;; --- Bug 88 Closed-by ---
        (testing "Bug 88 Closed-by"
          (let [r (get-report db "<88@test.org>")]
            (is (some? (:report/closed r)))
            (is (= :resolved (:report/close-reason r)))
            (is (= "closer@test.org" (:report/closed-address r)))))

        ;; --- Bug 90 trigger + directive ---
        (testing "Bug 90 Confirmed trigger + Owned-by directive"
          (let [r (get-report db "<90@test.org>")]
            (is (some? (:report/acked r)))
            (is (some? (:report/owned r)))
            (is (= "fixer@test.org" (:report/owned-address r)))))

        ;; --- Bug 90 Fixed + Unclosed ---
        (testing "Bug 90 Fixed + Unclosed (directive wins)"
          (is (nil? (:report/closed (get-report db "<90@test.org>")))))

        ;; --- Bug 93 Confirmed ---
        (testing "Bug 93 Confirmed"
          (let [r (get-report db "<93@test.org>")]
            (is (some? (:report/acked r)))))

        ;; --- Bug 90 Topic directive ---
        (testing "Bug 90 Topic directive"
          (is (= "regression" (:report/topic-value (get-report db "<90@test.org>")))))

        ;; --- Bug 93 Undeadline ---
        (testing "Bug 93 Undeadline removes deadline"
          (is (nil? (:report/deadline-value (get-report db "<93@test.org>")))))

        ;; --- Bug 98 standalone deadline ---
        (testing "Bug 98 standalone deadline"
          (is (some? (:report/deadline-value (get-report db "<98@test.org>")))))

        ;; --- Email 100 [source-name] prefix (bone-source fallback removed) ---
        (testing "Email 100 -- no bone-source fallback"
          (is (not (report-exists? db "<100@test.org>"))))

        ;; --- CHG 102+103 / REL 104 ---
        (testing "CHG 102+103 / REL 104 release closes multiple changes"
          (let [chg1 (get-report db "<102@test.org>")
                chg2 (get-report db "<103@test.org>")
                rel  (get-report db "<104@test.org>")
                rel-mids (set (map :report/message-id (all-related rel)))]
            (is (some? (:report/closed chg1)))
            (is (some? (:report/closed chg2)))
            (is (= :resolved (:report/close-reason chg1)))
            (is (= :resolved (:report/close-reason chg2)))
            (is (contains? rel-mids "<102@test.org>"))
            (is (contains? rel-mids "<103@test.org>"))
            (is (some #(= "<104@test.org>" (:report/message-id %))
                      (all-related chg1)))
            (is (some #(= "<104@test.org>" (:report/message-id %))
                      (all-related chg2)))))

        ;; --- Emails 105-107 Superseded-by ---
        (testing "Bug 105 superseded by 106"
          (let [r105 (get-report db "<105@test.org>")
                r106 (get-report db "<106@test.org>")]
            (is (some? (:report/closed r105)) "superseded report is closed")
            (is (= :superseded (:report/close-reason r105)))
            (is (= "<106@test.org>"
                    (some-> (superseded-by-target r105) :report/message-id)))
            ;; Bidirectional related link
            (is (some #(= "<106@test.org>" (:report/message-id %))
                      (all-related r105)))
            (is (some #(= "<105@test.org>" (:report/message-id %))
                      (all-related r106)))))

        ;; --- Emails 108-110 Supersede then unsupersede ---
        ;; 109 admin supersedes 108 by 106, 110 admin "Not superseded-by"
        ;; reopens it.  Email 111 is a neutral user reply (no command),
        ;; so the final state of 108 is cleanly "reopened".
        (testing "Bug 108 superseded then unsuperseded"
          (let [r108 (get-report db "<108@test.org>")
                r106 (get-report db "<106@test.org>")]
            (is (nil? (:report/closed r108)) "unsuperseded report is reopened")
            (is (nil? (:report/close-reason r108)))
            (is (nil? (superseded-by-target r108)))
            ;; Related link from the supersede is removed
            (is (not (some #(= "<106@test.org>" (:report/message-id %))
                           (all-related r108))))
            (is (not (some #(= "<108@test.org>" (:report/message-id %))
                           (all-related r106))))))

        ;; --- Emails 122-124 user Superseded-by ---
        ;; user-a (not a maintainer) marks their own narrow report 122
        ;; as superseded by user-b's broader report 123.  With
        ;; :superseded-by scoped as :user, this must succeed.
        (testing "Bug 122 superseded by 123 via regular user"
          (let [r122 (get-report db "<122@test.org>")
                r123 (get-report db "<123@test.org>")]
            (is (some? (:report/closed r122)) "user supersede closes 122")
            (is (= :superseded (:report/close-reason r122)))
            (is (= "<123@test.org>"
                    (some-> (superseded-by-target r122) :report/message-id)))
            ;; Bidirectional related link set by the supersede
            (is (some #(= "<123@test.org>" (:report/message-id %))
                      (all-related r122)))
            (is (some #(= "<122@test.org>" (:report/message-id %))
                      (all-related r123)))))

        ;; --- Emails 114-116 Not closed on superseded report ---
        (testing "Bug 114 superseded then reopened via Not closed"
          (let [r114 (get-report db "<114@test.org>")
                r106 (get-report db "<106@test.org>")]
            (is (nil? (:report/closed r114)) "report is reopened")
            (is (nil? (:report/close-reason r114)) "close-reason is cleared")
            (is (nil? (superseded-by-target r114)) "superseded-by-target is cleared")
            (is (not (some #(= "<106@test.org>" (:report/message-id %))
                           (all-related r114)))
                "related link to superseder is removed")
            (is (not (some #(= "<114@test.org>" (:report/message-id %))
                           (all-related r106)))
                "reverse related link is removed")))

        ;; --- Directive unit tests for supersede ---
        (testing "detect-lines: Superseded-by with angle brackets"
          (is (= [{:action :set-superseded :attr :rel/supersedes :target-message-id "<msg@example.com>" :scope :user :id :superseded-by}]
                 (commands/detect-lines :bug "Superseded-by: <msg@example.com>\n"))))

        (testing "detect-lines: Superseded-by tolerates an URL prefix"
          (is (= [{:action :set-superseded :attr :rel/supersedes :target-message-id "<msg@example.com>" :scope :user :id :superseded-by}]
                 (commands/detect-lines :bug "Superseded-by: https://orgmode.org/list/<msg@example.com>\n"))))

        (testing "detect-lines: Superseded-by accepts a public-inbox URL"
          (is (= [{:action :set-superseded :attr :rel/supersedes :target-message-id "<msg@example.com>" :scope :user :id :superseded-by}]
                 (commands/detect-lines :bug "Superseded-by: https://list.orgmode.org/orgmode/msg@example.com/\n")))
          (is (= [{:action :set-superseded :attr :rel/supersedes :target-message-id "<msg@example.com>" :scope :user :id :superseded-by}]
                 (commands/detect-lines :bug "Superseded-by: https://list.orgmode.org/orgmode/msg@example.com\n"))))

        (testing "detect-lines: Superseded-by accepts a bare message-id"
          (is (= [{:action :set-superseded :attr :rel/supersedes :target-message-id "<msg@example.com>" :scope :user :id :superseded-by}]
                 (commands/detect-lines :bug "Superseded-by: msg@example.com\n"))))

        (testing "detect-lines: Superseded-by rejects URLs where the @ segment is non-terminal"
          (is (= [] (commands/detect-lines :bug "Superseded-by: https://example.com/foo@bar/baz.html\n"))))

        (testing "detect-lines: Not superseded-by"
          (is (= [{:action :unset-superseded :attr :rel/supersedes-from :scope :setter-or-maintainer :id :unsuperseded-by :target-message-id "<msg@example.com>"}]
                 (commands/detect-lines :bug "Not superseded-by: <msg@example.com>\n"))))

        (testing "detect-lines: Supersedes (symmetric of Superseded-by)"
          (is (= [{:action :set-supersedes :attr :rel/supersedes :target-message-id "<msg@example.com>" :scope :user :id :supersedes}]
                 (commands/detect-lines :bug "Supersedes: <msg@example.com>\n"))))

        (testing "detect-lines: Not supersedes"
          (is (= [{:action :unset-supersedes :attr :rel/supersedes-to :scope :setter-or-maintainer :id :unsupersedes :target-message-id "<msg@example.com>"}]
                 (commands/detect-lines :bug "Not supersedes: <msg@example.com>\n"))))

        (testing "resolve-commands: superseded-by"
          (is (= {:set {} :unset #{} :superseded-by "<mid@host>"}
                 (commands/resolve-commands
                  [{:action :set-superseded :target-message-id "<mid@host>"}]))))

        (testing "resolve-commands: unsuperseded"
          (is (= {:set {} :unset #{} :unsuperseded-by? true :unsuperseded-by-mid "<mid@host>"}
                 (commands/resolve-commands
                  [{:action :unset-superseded :target-message-id "<mid@host>"}]))))

        (testing "resolve-commands: supersede then unsupersede"
          (is (= {:set {} :unset #{} :unsuperseded-by? true :unsuperseded-by-mid "<mid@host>"}
                 (commands/resolve-commands
                  [{:action :set-superseded :target-message-id "<mid@host>"}
                   {:action :unset-superseded :target-message-id "<mid@host>"}]))))

        (testing "resolve-commands: supersedes"
          (is (= {:set {} :unset #{} :supersedes "<mid@host>"}
                 (commands/resolve-commands
                  [{:action :set-supersedes :target-message-id "<mid@host>"}]))))

        (testing "resolve-commands: supersedes then not superseding"
          (is (= {:set {} :unset #{} :unsupersedes? true :unsupersedes-mid "<mid@host>"}
                 (commands/resolve-commands
                  [{:action :set-supersedes :target-message-id "<mid@host>"}
                   {:action :unset-supersedes :target-message-id "<mid@host>"}]))))

        ;; --- Emails 130-131 Supersedes (close target) ---
        (testing "Bug 130 closed by 131 via Supersedes:"
          (let [r130 (get-report db "<130@test.org>")
                r131 (get-report db "<131@test.org>")]
            (is (some? (:report/closed r130)) "target of Supersedes: is closed")
            (is (= :superseded (:report/close-reason r130)))
            (is (nil? (:report/closed r131)) "the directive's report stays open")
            ;; The canonical relation has :rel/from = 130 (closed) and
            ;; :rel/to = 131 (replacement), as if 131 had received a
            ;; Superseded-by: <131> directly.
            (is (= "<131@test.org>"
                   (some-> (superseded-by-target r130) :report/message-id)))
            (is (some #(= "<131@test.org>" (:report/message-id %))
                      (all-related r130)))
            (is (some #(= "<130@test.org>" (:report/message-id %))
                      (all-related r131)))))

        ;; --- Emails 132-134 Supersedes then Not supersedes ---
        (testing "Bug 132 superseded by 133 then reopened via Not supersedes"
          (let [r132 (get-report db "<132@test.org>")
                r133 (get-report db "<133@test.org>")]
            (is (nil? (:report/closed r132)) "Not supersedes reopens the previously-closed target")
            (is (nil? (:report/close-reason r132)))
            (is (nil? (superseded-by-target r132)))
            (is (not (some #(= "<133@test.org>" (:report/message-id %))
                           (all-related r132)))
                "Not supersedes retracts the :related-to companion too")
            (is (nil? (:report/closed r133)) "the superseder stays open throughout")))

        ;; --- Emails 135-137 Cycle flip via cross-directive ---
        (testing "Bug 135 superseded then unsuperseded by 136 flip; 136 closed instead"
          (let [r135 (get-report db "<135@test.org>")
                r136 (get-report db "<136@test.org>")]
            (is (nil? (:report/closed r135)) "135 reopens once 136 flips the direction")
            (is (nil? (:report/close-reason r135)))
            (is (some? (:report/closed r136)) "136 is now closed in the flipped direction")
            (is (= :superseded (:report/close-reason r136)))
            (is (= "<135@test.org>"
                   (some-> (superseded-by-target r136) :report/message-id)))
            (is (some #(= "<135@test.org>" (:report/message-id %))
                      (all-related r136)))))

        ;; ---------------------------------------------------------------
        ;; Participant / contributor tracking
        ;; ---------------------------------------------------------------

        (testing "Bug reporter is a participant without contributor-since"
          (let [p (get-participant db "direct" "user@test.org")]
            (is (some? p) "user@test.org should be a participant")
            (is (some? (:participant/since p)))
            (is (some? (:participant/contributor-since p))
                "user@test.org also submitted patches, so contributor-since must be set")))

        (testing "Patch submitter gets contributor-since stamped"
          ;; user@test.org submitted [PATCH org-agenda 1/2] Fix sorting (<07@test.org>)
          (let [p (get-participant db "direct" "user@test.org")]
            (is (some? (:participant/contributor-since p))
                "contributor-since should be set for a patch submitter")
            (is (inst? (:participant/contributor-since p)))))

        (testing "Command applier becomes a participant"
          ;; maint@test.org applies Confirmed/Urgent/Fixed on bug <02@test.org>
          (let [p (get-participant db "direct" "maint@test.org")]
            (is (some? p) "maint@test.org should be a participant via commands")))

        (testing "Command-only participant has no contributor-since"
          ;; newadmin@test.org only applied "Closed." on <53@test.org>
          (let [p (get-participant db "direct" "newadmin@test.org")]
            (is (some? p) "newadmin@test.org should be a participant via Closed command")
            (is (nil? (:participant/contributor-since p))
                "newadmin@test.org never submitted a patch")))

        (testing "Participant entity is unique per source+email (idempotent)"
          (let [count (d/q '[:find (count ?e) .
                             :where
                             [?e :participant/email "user@test.org"]
                             [?e :participant/source "direct"]]
                           db)]
            (is (= 1 count)
                "Multiple reports/patches from same person should not create duplicates")))

        (testing "Participant on public-list source is separate from direct"
          ;; <30@test.org> is a bug on public-list by user@test.org
          (let [p (get-participant db "public-list" "user@test.org")]
            (is (some? p) "user@test.org should also be a participant on public-list")))

        ;; --- Emails 117-119: Same-subject patch supersession ---
        (testing "Patch 117 superseded by same-subject reply 118"
          (let [r117 (get-report db "<117@test.org>")]
            (is (= :patch (:report/type r117)))
            (is (some? (:report/closed r117)) "first patch should be closed")
            (is (= :superseded (:report/close-reason r117)))
            (is (= "<118@test.org>"
                    (some-> (superseded-by-target r117) :report/message-id)))
            (is (nil? (:report/acked r117))
                "v2 reply must not spuriously credit v1 patch as acked")
            (is (nil? (:report/owned r117))
                "v2 reply must not spuriously credit v1 patch as owned")))

        (testing "Patch 118 superseded by same-subject reply 119"
          (let [r118 (get-report db "<118@test.org>")]
            (is (= :patch (:report/type r118)))
            (is (some? (:report/closed r118)) "second patch should be closed")
            (is (= :superseded (:report/close-reason r118)))
            (is (= "<119@test.org>"
                    (some-> (superseded-by-target r118) :report/message-id)))))

        (testing "Patch 119 is open (latest in chain)"
          (let [r119 (get-report db "<119@test.org>")]
            (is (= :patch (:report/type r119)))
            (is (nil? (:report/closed r119)) "latest patch should remain open")))

        ;; --- Emails 120-121: Inline diff supersession (no [PATCH] tag) ---
        (testing "Inline diff 120 superseded by same-subject reply 121"
          (let [r120 (get-report db "<120@test.org>")]
            (is (= :patch (:report/type r120)))
            (is (some? (:report/closed r120)) "first diff should be closed")
            (is (= :superseded (:report/close-reason r120)))
            (is (= "<121@test.org>"
                    (some-> (superseded-by-target r120) :report/message-id)))))

        (testing "Inline diff 121 is open (latest)"
          (let [r121 (get-report db "<121@test.org>")]
            (is (= :patch (:report/type r121)))
            (is (nil? (:report/closed r121)) "latest diff should remain open")))

        ;; --- Email 203: [BUG] + .patch attachment ---
        (testing "Bug 203 stores attached patch as metadata"
          (let [r203 (get-report db "<203@test.org>")]
            (is (= :bug (:report/type r203))
                "label decides: [BUG] + .patch attachment is a :bug")
            (is (nil? (:report/acked r203))
                "a root [BUG]+.patch must NOT self-credit its reporter")
            (is (nil? (:report/owned r203))
                "the implicit Acked/Owned only fires on replies, not on the root")
            (let [patches (:report/patches r203)]
              (is (= 1 (count patches))
                  ":report/patches is filled on the bug")
              (let [p (first patches)]
                (is (= "fix-save-crash.patch" (:patch/filename p)))
                (is (= :attachment (:patch/source p)))
                (is (= "User A <user-a@test.org>" (:patch/author p))
                    "format-patch headers are parsed and stored")
                (is (= "Fix crash on save" (:patch/subject p)))))))

        ;; --- Emails 204-205: implicit Acked/Owned from patch reply ---
        (testing "Bug 204 auto-credited by patch reply 205"
          (let [r204 (get-report db "<204@test.org>")]
            (is (= :bug (:report/type r204)))
            (is (some? (:report/acked r204))
                "reply with .patch attachment implicitly credits acked")
            (is (some? (:report/owned r204))
                "reply with .patch attachment implicitly credits owned")
            (is (= "fixer@test.org" (:report/acked-address r204)))
            (is (= "fixer@test.org" (:report/owned-address r204)))))

        ;; --- Emails 206-207: garde-fou A preserves manual setter ---
        (testing "Bug 206 keeps manual owner despite implicit signal in 207"
          (let [r206 (get-report db "<206@test.org>")]
            (is (= :bug (:report/type r206)))
            (is (= "admin@test.org" (:report/owned-address r206))
                "garde-fou A: pre-existing Owned-by setter is preserved")))

        ;; --- Emails 208-209: :patch-triggers? false suppresses implicit ---
        (testing "Bug 208 on no-triggers source skips implicit Acked/Owned"
          (let [r208 (get-report db "<208@test.org>")]
            (is (= :bug (:report/type r208)))
            (is (nil? (:report/acked r208))
                ":patch-triggers? false gates the implicit trigger")
            (is (nil? (:report/owned r208)))))

        ;; --- Emails 211-213: Acked requires a distinct second party ---
        (testing "Self-ack ignored, third-party ack succeeds; self-owned allowed"
          ;; Alice (212) tries to Acked + Owned her own bug.  Bob (213)
          ;; later writes "Confirmed".  Alice's self-Acked was dropped,
          ;; so the acked slot was free for Bob.  Alice's self-Owned
          ;; stuck (self-ownership is allowed).
          (let [r211 (get-report db "<211@test.org>")]
            (is (= :bug (:report/type r211)))
            (is (= "bob@test.org" (:report/acked-address r211))
                "Bob (third party) acks -- Alice's self-ack was dropped")
            (is (= "alice@test.org" (:report/owned-address r211))
                "Alice owns her own bug -- self-ownership is allowed")))

        ;; --- Emails 214-215: implicit Acked from patch reply is also gated ---
        (testing "Reporter sending a patch in reply owns but does not ack"
          (let [r214 (get-report db "<214@test.org>")]
            (is (= :bug (:report/type r214)))
            (is (nil? (:report/acked r214))
                "implicit ack is dropped when the patch sender is the reporter")
            (is (= "carol@test.org" (:report/owned-address r214))
                "implicit owned still fires -- self-ownership is allowed")))

        ;; --- Emails 216-217: a reply targets the nearest report,
        ;; even when the reply itself creates a new report. ---
        (testing "Reply that creates v2 patch hands its commands to v1 (nearest)"
          (let [r216 (get-report db "<216@test.org>")
                r217 (get-report db "<217@test.org>")]
            (is (= :patch (:report/type r216)))
            (is (= :patch (:report/type r217)))
            (is (= "lexer" (:report/topic-value r216))
                "Topic directive lands on v1 (nearest), overriding the label topic")
            (is (some? (:report/important r216))
                "Important trigger lands on v1 (nearest), not on the new v2")
            (is (= "parser" (:report/topic-value r217))
                "v2 keeps the topic from its own [PATCH parser] label")
            (is (nil? (:report/important r217))
                "v2 carries no directive: the body annotates the thread, not the carrier"))))
      (finally
        (teardown! ctx)))))

;; ---------------------------------------------------------------------------
;; Pending-thread (out-of-order delivery rescue)
;; ---------------------------------------------------------------------------

(defn- mk-email
  "Build a minimal email entity map for pending-thread tests."
  [{:keys [mid subject from date in-reply-to refs body]}]
  (let [refs-vec    (vec refs)
        ancestor    (vec (distinct (cond-> refs-vec
                                     (and in-reply-to (not (some #{in-reply-to} refs-vec)))
                                     (conj in-reply-to))))]
    (cond-> {:email/message-id   mid
             :email/subject      subject
             :email/from-address from
             :email/author-address from
             :email/date-sent    date
             :email/ingested-at  date
             :email/body-text    (or body "")}
      in-reply-to        (assoc :email/in-reply-to in-reply-to)
      (seq refs)         (assoc :email/references (str/join " " refs))
      (seq ancestor)     (assoc :email/ancestor-mids ancestor))))

(defn- store-and-process!
  "Insert an email entity, then run digest/process-email! on it."
  [conn email-map source-name]
  (let [email-with-source (assoc email-map :email/source source-name)]
    (d/transact! conn [email-with-source])
    (let [eid (d/entid (d/db conn) [:email/message-id (:email/message-id email-map)])
          email (d/pull (d/db conn) digest/email-pull-pattern eid)]
      (digest/process-email! conn source-map sources email))))

(defn- pending? [db mid]
  (boolean
   (d/q '[:find ?p . :in $ ?mid
          :where
          [?e :email/message-id ?mid]
          [?e :email/pending-thread? ?p]]
        db mid)))

(deftest pending-thread-no-irt-not-flagged
  (testing "An email with no In-Reply-To is never flagged pending."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        (store-and-process! conn
                            (mk-email {:mid "<root-1@test.org>"
                                       :subject "[BUG] something"
                                       :from "user@test.org"
                                       :date #inst "2026-05-01T10:00:00"
                                       :body "broken\n"})
                            "direct")
        (let [db (d/db conn)]
          (is (false? (pending? db "<root-1@test.org>"))
              "Root email should not be pending")
          (is (report-exists? db "<root-1@test.org>")
              "Bug report should be created"))
        (finally
          (teardown! ctx))))))

(deftest pending-thread-in-order-typical
  (testing "In-order : reply arrives after parent → no pending flag, command applied."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        (store-and-process! conn
                            (mk-email {:mid "<inord-1@test.org>"
                                       :subject "[BUG] crash"
                                       :from "user@test.org"
                                       :date #inst "2026-05-01T10:00:00"
                                       :body "broken\n"})
                            "direct")
        (store-and-process! conn
                            (mk-email {:mid "<inord-2@test.org>"
                                       :subject "Re: [BUG] crash"
                                       :from "admin@test.org"
                                       :date #inst "2026-05-01T11:00:00"
                                       :in-reply-to "<inord-1@test.org>"
                                       :body "Closed.\n"})
                            "direct")
        (let [db (d/db conn)
              r  (get-report db "<inord-1@test.org>")]
          (is (false? (pending? db "<inord-2@test.org>"))
              "Reply with resolved IRT should not be pending")
          (is (some? (:report/closed r)) "Bug should be closed by the reply"))
        (finally
          (teardown! ctx))))))

(deftest pending-thread-out-of-order-rescue
  (testing "Out-of-order : reply ingested before parent → pending, then retried."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; 1. Reply arrives first, parent missing → flagged pending,
        ;;    command not applied yet.
        (store-and-process! conn
                            (mk-email {:mid "<ooo-reply@test.org>"
                                       :subject "Re: [BUG] crash"
                                       :from "admin@test.org"
                                       :date #inst "2026-05-01T11:00:00"
                                       :in-reply-to "<ooo-bug@test.org>"
                                       :body "Closed.\n"})
                            "direct")
        (let [db (d/db conn)]
          (is (true? (pending? db "<ooo-reply@test.org>"))
              "Reply should be flagged pending while IRT is absent")
          (is (not (report-exists? db "<ooo-bug@test.org>"))
              "No report yet for the missing parent"))

        ;; 2. Parent arrives → its post-process retry hook should re-process
        ;;    the pending reply and apply the command.
        (store-and-process! conn
                            (mk-email {:mid "<ooo-bug@test.org>"
                                       :subject "[BUG] crash"
                                       :from "user@test.org"
                                       :date #inst "2026-05-01T10:00:00"
                                       :body "broken\n"})
                            "direct")
        (let [db (d/db conn)
              r  (get-report db "<ooo-bug@test.org>")]
          (is (false? (pending? db "<ooo-reply@test.org>"))
              "Pending flag must be cleared after retry")
          (is (some? (:report/closed r))
              "Bug should now be closed via the retried 'Closed.' trigger"))
        (finally
          (teardown! ctx))))))

(deftest carrier-dispatch-supersedes-applies-to-new-report
  (testing "A reply that creates a new report AND carries Supersedes:
            <external-mid> applies the relation to the new report
            (closes the external target), not to the thread parent."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; 1. Old bug (will be superseded).
        (store-and-process! conn
                            (mk-email {:mid "<old-bug@test.org>"
                                       :subject "[BUG] narrow framing"
                                       :from "alice@test.org"
                                       :date #inst "2026-05-01T10:00:00"
                                       :body "narrow report\n"})
                            "direct")
        ;; 2. Unrelated parent thread (will be the In-Reply-To target).
        (store-and-process! conn
                            (mk-email {:mid "<thread-root@test.org>"
                                       :subject "[BUG] something else"
                                       :from "bob@test.org"
                                       :date #inst "2026-05-01T11:00:00"
                                       :body "another bug\n"})
                            "direct")
        ;; 3. A reply that itself opens a new [BUG] and supersedes the
        ;;    old bug from step 1.  Before the carrier loosening the
        ;;    Supersedes: would have been dispatched to <thread-root>
        ;;    (the nearest report in the thread), where it would have
        ;;    no useful meaning.  With the loosening it lands on the
        ;;    new report and closes <old-bug>.
        (store-and-process! conn
                            (mk-email {:mid "<new-bug@test.org>"
                                       :subject "[BUG] broader framing"
                                       :from "carol@test.org"
                                       :date #inst "2026-05-01T12:00:00"
                                       :in-reply-to "<thread-root@test.org>"
                                       :body "Supersedes: <old-bug@test.org>\n"})
                            "direct")
        (let [db (d/db conn)
              old   (get-report db "<old-bug@test.org>")
              new   (get-report db "<new-bug@test.org>")
              root  (get-report db "<thread-root@test.org>")]
          (is (some? new)        "new report was created")
          (is (= :bug (:report/type new)))
          (is (some? (:report/closed old))
              "old bug was closed via Supersedes: from the new report's body")
          (is (= :superseded (:report/close-reason old)))
          (is (nil? (:report/closed root))
              "the thread parent (unrelated to the supersede) is untouched"))
        (finally
          (teardown! ctx))))))

(deftest new-report-reply-without-anchorable-parent-applies-commands
  (testing "An email that creates a new report AND replies to a
            non-indexable In-Reply-To (so thread-anchorable? passes but
            nearest-eids is empty) still receives its body commands."
    (let [{:keys [conn] :as ctx} (setup-db!)
          ;; IRT longer than max-indexable-mid-length (200) -- thread-
          ;; anchorable? returns true via the non-indexable branch, but
          ;; ancestor-mids-from filters it out so nearest-eids is empty.
          long-irt (str "<" (apply str (repeat 220 "a")) "@x>")]
      (try
        (store-and-process! conn
                            (mk-email {:mid "<orphan-bug@test.org>"
                                       :subject "[BUG] orphan reply"
                                       :from "alice@test.org"
                                       :date #inst "2026-05-01T10:00:00"
                                       :in-reply-to long-irt
                                       :body "Important.\n"})
                            "direct")
        (let [r (get-report (d/db conn) "<orphan-bug@test.org>")]
          (is (some? r) "new report was created")
          (is (= :bug (:report/type r)))
          (is (some? (:report/important r))
              "Important. trigger applied to the new report"))
        (finally
          (teardown! ctx))))))

(deftest pending-thread-references-anchor
  (testing "Reply with missing IRT but a known References ancestor is threaded immediately."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; 1. Root bug, ingested first → creates a report.
        (store-and-process! conn
                            (mk-email {:mid "<shared-bug@test.org>"
                                       :subject "[BUG] flaky"
                                       :from "user@test.org"
                                       :date #inst "2026-05-01T10:00:00"
                                       :body "broken\n"})
                            "direct")
        ;; 2. Reply whose IRT points to a missing intermediate, but
        ;;    References include the root → no longer pending: the
        ;;    References ancestor anchors the thread.
        (store-and-process! conn
                            (mk-email {:mid "<shared-reply@test.org>"
                                       :subject "Re: [BUG] flaky"
                                       :from "admin@test.org"
                                       :date #inst "2026-05-01T12:00:00"
                                       :in-reply-to "<shared-mid@test.org>"
                                       :refs ["<shared-bug@test.org>"]
                                       :body "Closed.\n"})
                            "direct")
        (let [db (d/db conn)
              r  (get-report db "<shared-bug@test.org>")]
          (is (false? (pending? db "<shared-reply@test.org>"))
              "Reply must not be pending -- References ancestor is in DB")
          (is (some? (:report/closed r))
              "Root bug must be closed by the Closed. trigger right away"))
        (finally
          (teardown! ctx))))))

(deftest pending-thread-splice-through-stored
  (testing "thread-lookup splices through a stored pending intermediate to reach the report."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; 1. Root bug → report exists.
        (store-and-process! conn
                            (mk-email {:mid "<spl-bug@test.org>"
                                       :subject "[BUG] flaky"
                                       :from "user@test.org"
                                       :date #inst "2026-05-01T10:00:00"
                                       :body "broken\n"})
                            "direct")
        ;; 2. An intermediate reply arrives BUT its own IRT points to
        ;;    a missing message (and no References) → flagged pending,
        ;;    *not* attached as a descendant of the bug report.
        (store-and-process! conn
                            (mk-email {:mid "<spl-mid@test.org>"
                                       :subject "Re: [BUG] flaky"
                                       :from "admin@test.org"
                                       :date #inst "2026-05-01T11:00:00"
                                       :in-reply-to "<spl-missing@test.org>"
                                       :body "tracking\n"})
                            "direct")
        ;; Sanity check: intermediate is pending and not attached.
        (is (true? (pending? (d/db conn) "<spl-mid@test.org>"))
            "Setup: intermediate must be pending (its IRT is unknown)")
        ;; 3. Final reply with IRT = the stored pending intermediate
        ;;    (no References). With splicing, thread-lookup walks
        ;;    through the pending intermediate's own ancestor mids and
        ;;    reaches the bug report -- so Closed. applies.
        (store-and-process! conn
                            (mk-email {:mid "<spl-final@test.org>"
                                       :subject "Re: [BUG] flaky"
                                       :from "admin@test.org"
                                       :date #inst "2026-05-01T12:00:00"
                                       :in-reply-to "<spl-mid@test.org>"
                                       :refs ["<spl-bug@test.org>"]
                                       :body "Closed.\n"})
                            "direct")
        (let [db (d/db conn)
              r  (get-report db "<spl-bug@test.org>")]
          (is (false? (pending? db "<spl-final@test.org>"))
              "Final reply not pending -- the stored pending intermediate anchors it")
          (is (some? (:report/closed r))
              "Closed. must reach the bug via splicing through the pending intermediate"))
        (finally
          (teardown! ctx))))))

(deftest pending-thread-splice-no-references
  (testing "Splicing also works when the final reply has no References at all."
    ;; This is the PavoDive case: a late reply whose only ancestor mid
    ;; is the immediate parent, and that parent itself is stored but
    ;; pending (broken by a missing IRT of its own).
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        (store-and-process! conn
                            (mk-email {:mid "<nr-bug@test.org>"
                                       :subject "[BUG] flaky"
                                       :from "user@test.org"
                                       :date #inst "2026-05-01T10:00:00"
                                       :body "broken\n"})
                            "direct")
        (store-and-process! conn
                            (mk-email {:mid "<nr-mid@test.org>"
                                       :subject "Re: [BUG] flaky"
                                       :from "admin@test.org"
                                       :date #inst "2026-05-01T11:00:00"
                                       :in-reply-to "<nr-missing@test.org>"
                                       :refs ["<nr-bug@test.org>"
                                              "<nr-missing@test.org>"]
                                       :body "tracking\n"})
                            "direct")
        ;; Intermediate has a known References ancestor (the bug), so
        ;; with (1) it's NOT pending and is attached as a descendant.
        (is (false? (pending? (d/db conn) "<nr-mid@test.org>"))
            "Intermediate must be threaded via its References anchor")
        ;; Final reply with NO References, IRT to the intermediate.
        (store-and-process! conn
                            (mk-email {:mid "<nr-final@test.org>"
                                       :subject "Re: [BUG] flaky"
                                       :from "admin@test.org"
                                       :date #inst "2026-05-01T12:00:00"
                                       :in-reply-to "<nr-mid@test.org>"
                                       :body "Closed.\n"})
                            "direct")
        (let [db (d/db conn)
              r  (get-report db "<nr-bug@test.org>")]
          (is (false? (pending? db "<nr-final@test.org>"))
              "Final reply is anchored via its IRT (stored intermediate)")
          (is (some? (:report/closed r))
              "Bug closed via the chain: final -> intermediate -> bug"))
        (finally
          (teardown! ctx))))))

(deftest pending-thread-ttl-flush
  (testing "TTL flush forces processing of stale pending emails."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; Create a root report so threading has something to attach to.
        (store-and-process! conn
                            (mk-email {:mid "<ttl-bug@test.org>"
                                       :subject "[BUG] orphan"
                                       :from "user@test.org"
                                       :date #inst "2026-04-01T10:00:00"
                                       :body "broken\n"})
                            "direct")
        ;; Reply whose IRT target never arrives -- manually backdate
        ;; ingested-at to simulate an old pending email.
        (let [old-date #inst "2026-04-01T11:00:00"]
          (d/transact! conn
                       [(assoc (mk-email {:mid "<ttl-orphan@test.org>"
                                          :subject "Re: [BUG] orphan"
                                          :from "admin@test.org"
                                          :date old-date
                                          :in-reply-to "<never-arrives@test.org>"
                                          :refs ["<ttl-bug@test.org>"]
                                          :body "Closed.\n"})
                               :email/source "direct"
                               :email/pending-thread? true
                               :email/ingested-at old-date)]))
        (is (true? (pending? (d/db conn) "<ttl-orphan@test.org>"))
            "Setup: orphan should be pending")

        ;; TTL flush with a 1-day window -- orphan is older than 30 days.
        (digest/flush-stale-pending! conn source-map sources 1)

        (let [db (d/db conn)
              r  (get-report db "<ttl-bug@test.org>")]
          (is (false? (pending? db "<ttl-orphan@test.org>"))
              "Pending flag cleared by TTL flush")
          (is (some? (:report/closed r))
              "Command applied to the available ancestor report after TTL flush"))
        (finally
          (teardown! ctx))))))

;; ---------------------------------------------------------------------------
;; Patch-series cover-letter broadcast
;; ---------------------------------------------------------------------------

(deftest cover-letter-broadcast
  (testing "Commands sent in reply to a patch-series cover letter
            propagate to every patch of the series, EXCEPT relation
            commands which stay on the cover only."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; A standalone bug to be used as the Related-to target.
        (store-and-process! conn
                            (mk-email {:mid "<other-bug@test.org>"
                                       :subject "[BUG] unrelated thing"
                                       :from "third@test.org"
                                       :date #inst "2026-06-01T08:00:00"
                                       :body "another report\n"})
                            "direct")
        ;; Cover letter (0/3) + 3 patches.
        (store-and-process! conn
                            (mk-email {:mid "<cov@test.org>"
                                       :subject "[PATCH cl 0/3] Refactor X"
                                       :from "user@test.org"
                                       :date #inst "2026-06-01T10:00:00"
                                       :body "Series intro.\n"})
                            "direct")
        (doseq [i [1 2 3]]
          (store-and-process! conn
                              (mk-email {:mid (str "<p" i "@test.org>")
                                         :subject (str "[PATCH cl " i "/3] step " i)
                                         :from "user@test.org"
                                         :date #inst "2026-06-01T10:01:00"
                                         :in-reply-to "<cov@test.org>"
                                         :body (str "step " i "\n\n"
                                                    "diff --git a/x.clj b/x.clj\n"
                                                    "--- a/x.clj\n+++ b/x.clj\n"
                                                    "@@ -1,1 +1,1 @@\n-a\n+b\n")})
                              "direct"))
        ;; Reply to the cover with a mix of broadcastable commands
        ;; and one relation command.
        (store-and-process! conn
                            (mk-email {:mid "<rev@test.org>"
                                       :subject "Re: [PATCH cl 0/3] Refactor X"
                                       :from "admin@test.org"
                                       :date #inst "2026-06-02T09:00:00"
                                       :in-reply-to "<cov@test.org>"
                                       :body (str "Acked.\n"
                                                  "Deadline: 30d\n"
                                                  "Related-to: <other-bug@test.org>\n")})
                            "direct")

        (let [db    (d/db conn)
              cover (get-report db "<cov@test.org>")
              p1    (get-report db "<p1@test.org>")
              p2    (get-report db "<p2@test.org>")
              p3    (get-report db "<p3@test.org>")
              related-to-other? (fn [r]
                                  (some #(= "<other-bug@test.org>"
                                            (:report/message-id %))
                                        (all-related r)))]
          (testing "Acked. propagates to cover and every patch"
            (is (some? (:report/acked cover)) "cover acked")
            (is (some? (:report/acked p1))    "patch 1/3 acked")
            (is (some? (:report/acked p2))    "patch 2/3 acked")
            (is (some? (:report/acked p3))    "patch 3/3 acked"))
          (testing "Deadline: propagates to cover and every patch"
            (is (some? (:report/deadline-value cover)) "cover has deadline")
            (is (some? (:report/deadline-value p1))    "patch 1/3 has deadline")
            (is (some? (:report/deadline-value p2))    "patch 2/3 has deadline")
            (is (some? (:report/deadline-value p3))    "patch 3/3 has deadline"))
          (testing "Related-to stays on the cover only"
            (is (related-to-other? cover)
                "cover is related-to the target bug")
            (is (not (related-to-other? p1))
                "patch 1/3 has no relation to the target")
            (is (not (related-to-other? p2))
                "patch 2/3 has no relation to the target")
            (is (not (related-to-other? p3))
                "patch 3/3 has no relation to the target")))
        (finally
          (teardown! ctx))))))

(deftest cover-letter-broadcast-closure
  (testing "Closed. on a cover letter closes every patch of the series."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        (store-and-process! conn
                            (mk-email {:mid "<cov2@test.org>"
                                       :subject "[PATCH topic 0/2] series"
                                       :from "user@test.org"
                                       :date #inst "2026-06-03T10:00:00"
                                       :body "Series\n"})
                            "direct")
        (doseq [i [1 2]]
          (store-and-process! conn
                              (mk-email {:mid (str "<q" i "@test.org>")
                                         :subject (str "[PATCH topic " i "/2] step " i)
                                         :from "user@test.org"
                                         :date #inst "2026-06-03T10:01:00"
                                         :in-reply-to "<cov2@test.org>"
                                         :body (str "step " i "\n\n"
                                                    "diff --git a/y.clj b/y.clj\n"
                                                    "--- a/y.clj\n+++ b/y.clj\n"
                                                    "@@ -1,1 +1,1 @@\n-a\n+b\n")})
                              "direct"))
        (store-and-process! conn
                            (mk-email {:mid "<close-rev@test.org>"
                                       :subject "Re: [PATCH topic 0/2] series"
                                       :from "admin@test.org"
                                       :date #inst "2026-06-04T09:00:00"
                                       :in-reply-to "<cov2@test.org>"
                                       :body "Closed.\n"})
                            "direct")
        (let [db (d/db conn)
              cover (get-report db "<cov2@test.org>")
              q1    (get-report db "<q1@test.org>")
              q2    (get-report db "<q2@test.org>")]
          (is (some? (:report/closed cover)) "cover closed")
          (is (some? (:report/closed q1))    "patch 1/2 closed")
          (is (some? (:report/closed q2))    "patch 2/2 closed")
          (is (= :resolved (:report/close-reason cover)))
          (is (= :resolved (:report/close-reason q1)))
          (is (= :resolved (:report/close-reason q2))))
        (finally
          (teardown! ctx))))))

(deftest cover-letter-broadcast-supersede
  (testing "Superseded-by: on a cover letter supersedes every patch:
            each report closes with reason :superseded and points to
            the same target via :rel/supersedes."
    (let [{:keys [conn] :as ctx} (setup-db!)]
      (try
        ;; The Superseded-by: target must be a :patch (cross-type
        ;; supersede is rejected by the same-type check).
        (store-and-process! conn
                            (mk-email {:mid "<target-bug@test.org>"
                                       :subject "[PATCH replacement 1/1] the fix"
                                       :from "third@test.org"
                                       :date #inst "2026-06-05T08:00:00"
                                       :body (str "the fix\n\n"
                                                  "diff --git a/y.clj b/y.clj\n"
                                                  "--- a/y.clj\n+++ b/y.clj\n"
                                                  "@@ -1,1 +1,1 @@\n-a\n+b\n")})
                            "direct")
        (store-and-process! conn
                            (mk-email {:mid "<cov3@test.org>"
                                       :subject "[PATCH t3 0/2] old series"
                                       :from "user@test.org"
                                       :date #inst "2026-06-05T10:00:00"
                                       :body "Series\n"})
                            "direct")
        (doseq [i [1 2]]
          (store-and-process! conn
                              (mk-email {:mid (str "<r" i "@test.org>")
                                         :subject (str "[PATCH t3 " i "/2] step " i)
                                         :from "user@test.org"
                                         :date #inst "2026-06-05T10:01:00"
                                         :in-reply-to "<cov3@test.org>"
                                         :body (str "step " i "\n\n"
                                                    "diff --git a/z.clj b/z.clj\n"
                                                    "--- a/z.clj\n+++ b/z.clj\n"
                                                    "@@ -1,1 +1,1 @@\n-a\n+b\n")})
                              "direct"))
        (store-and-process! conn
                            (mk-email {:mid "<sup-rev@test.org>"
                                       :subject "Re: [PATCH t3 0/2] old series"
                                       :from "admin@test.org"
                                       :date #inst "2026-06-06T09:00:00"
                                       :in-reply-to "<cov3@test.org>"
                                       :body "Superseded-by: <target-bug@test.org>\n"})
                            "direct")
        (let [db    (d/db conn)
              cover (get-report db "<cov3@test.org>")
              r1    (get-report db "<r1@test.org>")
              r2    (get-report db "<r2@test.org>")
              target-mid (fn [r]
                           (some-> (superseded-by-target r) :report/message-id))]
          (doseq [[label r] [["cover" cover] ["patch 1/2" r1] ["patch 2/2" r2]]]
            (is (some? (:report/closed r))
                (str label " is closed"))
            (is (= :superseded (:report/close-reason r))
                (str label " has :superseded reason"))
            (is (= "<target-bug@test.org>" (target-mid r))
                (str label " is :rel/supersedes -> target-bug"))))
        (finally
          (teardown! ctx))))))

;; ---------------------------------------------------------------------------
;; report-entity :report/has-ics is scoped to announcements
;; ---------------------------------------------------------------------------

(deftest report-entity-has-ics-scoped-to-announcements
  (let [email {:email/attachments [{:attachment/filename     "invite.ics"
                                    :attachment/content-type "text/calendar"}]
               :email/author-address "alice@test.org"}
        has-ics (fn [rtype]
                  (:report/has-ics
                   (digest/report-entity 1 "<m@test.org>" {:type rtype} nil email nil)))]
    (testing "announcement carrying an .ics is flagged"
      (is (true? (has-ics :announcement))))
    (testing "other types are never flagged, even with an .ics attachment"
      (is (false? (has-ics :bug)))
      (is (false? (has-ics :patch))))))
