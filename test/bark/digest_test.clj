(ns bark.digest-test
  "Integration tests for bark.digest.
  Creates a temporary Datalevin DB, inserts test emails, processes them
  via bark.digest/process-email!, and verifies reports, commands,
  threading, votes, roles, and permissions.

  Ported from test/bark-digest-test.clj (bb version)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [datalevin.core :as d]
            [bark.commands :as commands]
            [bark.common :as common]
            [bark.digest :as digest]
            [bark.roles :as roles]
            [bark.test-helpers :as th])
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

(defn- get-report [db message-id]
  (d/pull db
          '[:report/type :report/version :report/topic-value
            :report/patch-seq :report/patch-source :report/message-id
            :report/acked :report/owned :report/closed
            :report/close-reason
            {:report/superseded-by-target [:report/message-id {:report/email [:email/subject]}]}
            :report/urgent :report/important
            :report/deadline-value :report/expiry-value
            :report/acked-address :report/owned-address
            :report/closed-address :report/urgent-address
            :report/important-address
            {:report/descendants [:email/message-id]}
            {:report/related [:report/type :report/message-id]}
            {:report/series [:series/id :series/expected :series/closed
                             {:series/patches [:db/id]}
                             {:series/cover-letter [:email/message-id]}]}
            {:report/email [:email/subject :email/from-address
                            :email/headers-edn]}]
          [:report/message-id message-id]))

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
                                :list "list.test.org"}})

(def sources [{:name "public-list"
               :list "list.test.org"}
              {:name "direct"
               :to "direct@test.org"}])

;; ---------------------------------------------------------------------------
;; DB setup / teardown
;; ---------------------------------------------------------------------------

(defn- setup-db! []
  (let [db-path (str "/tmp/bark-test-" (System/currentTimeMillis))
        conn    (d/get-conn db-path common/bark-schema)]
    ;; Seed an initial open tenure (from = nil, order = 0) making
    ;; admin@test.org the lead maintainer on both sources.
    (d/transact! conn [{:maint-tenure/source "direct"
                        :maint-tenure/email  "admin@test.org"
                        :maint-tenure/order  0}
                       {:maint-tenure/source "public-list"
                        :maint-tenure/email  "admin@test.org"
                        :maint-tenure/order  0}])
    ;; Insert test emails
    (let [emails (edn/read-string (slurp "resources/emails.edn"))]
      (doseq [email emails]
        (d/transact! conn [email])))
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

(deftest trigger-scope-denial-recording
  (testing "non-maintainer trigger on a source that overrides :closed to :maintainer"
    (let [filter-triggers   (var-get #'bark.commands/filter-triggers-by-scope)
          describe-trigger  (var-get #'bark.commands/describe-denied-trigger)
          recorded          (atom [])]
      (testing "describe-denied-trigger labels"
        (is (= "Closed." (describe-trigger :report/closed)))
        (is (= "Acked." (describe-trigger :report/acked)))
        (is (= "Urgent." (describe-trigger :report/urgent))))
      (with-redefs [commands/record-failure! (fn [entry] (swap! recorded conj entry))]
        (let [trig-result {:report/closed true :report/acked true}
              overrides   {:closed {:scope :maintainer}}
              failure-ctx {:source "src" :from-addr "user@test" :email-date nil :report-mid "<r@test>"}
              filtered    (filter-triggers trig-result overrides false failure-ctx)]
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

        ;; --- Patch 09 inline ---
        (testing "Patch 09 inline diff"
          (let [r (get-report db "<09@test.org>")]
            (is (= :patch (:report/type r)))
            (is (contains? (set (:report/patch-source r)) :inline))))

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
                      (:report/related rel)))
            (is (some #(= "<20@test.org>" (:report/message-id %))
                      (:report/related chg)))))

        ;; --- Email 21 (Ignore mechanism removed — report is now created) ---
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
            ;; Private commands (113 → Handled.) are now blocked.
            (is (some? (:report/acked r)))
            (is (nil? (:report/owned r)))))

        ;; --- Email 31 bypassing list ---
        (testing "Email 31 bypassing list"
          (is (not (report-exists? db "<31@test.org>"))))

        ;; --- Email 32 wrong list ---
        (testing "Email 32 wrong List-Post"
          (is (not (report-exists? db "<32@test.org>"))))

        ;; --- Email 33 admin direct (no bark-source fallback) ---
        (testing "Email 33 admin direct — no report without list delivery"
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
                r36 (get-report db "<36@test.org>")
                r37 (get-report db "<37@test.org>")]
            (is (some? (:report/series r35)))
            (is (= "1/3" (:report/patch-seq r35)))
            ;; All 3 patches in the series are mutually related
            (is (some #(= "<36@test.org>" (:report/message-id %))
                      (:report/related r35)))
            (is (some #(= "<37@test.org>" (:report/message-id %))
                      (:report/related r35)))
            (is (some #(= "<35@test.org>" (:report/message-id %))
                      (:report/related r37)))))

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

        ;; --- Email 40 patch related to bug ---
        (testing "Email 40 patch related to bug"
          (let [patch (get-report db "<40@test.org>")
                bug   (get-report db "<23@test.org>")]
            (is (= :patch (:report/type patch)))
            (is (some #(= "<23@test.org>" (:report/message-id %))
                      (:report/related patch)))
            (is (some #(= "<40@test.org>" (:report/message-id %))
                      (:report/related bug)))))

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

        ;; --- Emails 56-57-74 notify prefs (final state after email 74) ---
        (testing "Emails 56-57-74 notify prefs"
          (let [pref (d/pull db '[:notify/enabled :notify/interval-days :notify/min-priority]
                             [:notify/key "direct:maint@test.org"])]
            (is (:notify/enabled pref))
            ;; Email 57 sets d:7, but email 74 (now allowed via list) overrides to d:1
            (is (= 1 (:notify/interval-days pref)))
            (is (= 2 (:notify/min-priority pref)))))

        ;; --- Email 58 notify from regular user ---
        (testing "Email 58 notify from regular user"
          (is (nil? (d/q '[:find ?e . :in $ ?k :where [?e :notify/key ?k]]
                         db "direct:user@test.org"))))

        ;; --- Email 74 notify via mailing list (now allowed) ---
        (testing "Email 74 notify via mailing list"
          (let [pref (d/pull db '[:notify/interval-days]
                             [:notify/key "direct:maint@test.org"])]
            (is (= 1 (:notify/interval-days pref)))))

        ;; --- Email 59 case insensitive [bug] ---
        (testing "Email 59 case insensitive [bug]"
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
        (testing "Email 64 [bug] reply creates own report"
          (is (report-exists? db "<64@test.org>")))

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
                      (:report/related r68)))
            (is (some #(= "<68@test.org>" (:report/message-id %))
                      (:report/related r69)))))

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
        (testing "detect-directives"
          (is (= [{:action :set :attr :report/acked :email-address "a@b.com" :scope :maintainer :id :acked-by}]
                 (commands/detect-directives :bug "Acked-by: a@b.com\n")))
          (is (= [{:action :set :attr :report/owned :email-address "x@y.com" :scope :maintainer :id :owned-by}
                  {:action :set :attr :report/urgent :email-address "x@y.com" :scope :maintainer :id :urgent-by}]
                 (commands/detect-directives :bug "Owned-by: x@y.com\nUrgent-by: x@y.com\n")))
          (is (= [{:action :unset :attr :report/acked :scope :setter-or-maintainer :id :unacked}]
                 (commands/detect-directives :bug "Not acked\n")))
          (is (= [{:action :unset :attr :report/urgent :scope :setter-or-maintainer :id :unurgent}]
                 (commands/detect-directives :bug "Not urgent\n")))
          (is (= [{:action :unset :attr :report/important :scope :setter-or-maintainer :id :unimportant}]
                 (commands/detect-directives :bug "Not important\n")))
          (is (= [{:action :set-deadline :date (parse-date-iso "2026-06-15") :scope :user :id :deadline}]
                 (commands/detect-directives :bug "Deadline: 2026-06-15\n")))
          (is (= [{:action :unset-deadline :scope :setter-or-maintainer :id :undeadline}]
                 (commands/detect-directives :bug "No deadline\n")))
          (is (= [{:action :unset-topic :scope :setter-or-maintainer :id :untopic}]
                 (commands/detect-directives :bug "No topic\n")))
          (is (= [{:action :set-topic :topic "my-topic" :scope :user :id :topic}]
                 (commands/detect-directives :bug "Topic: my-topic\n")))
          (is (= [{:action :set :attr :report/acked :email-address "a@b.com" :scope :maintainer :id :acked-by}
                  {:action :set-deadline :date (parse-date-iso "2026-07-01") :scope :user :id :deadline}
                  {:action :set-topic :topic "urgent-fix" :scope :user :id :topic}]
                 (commands/detect-directives :bug "Acked-by: a@b.com\nDeadline: 2026-07-01\nTopic: urgent-fix\n")))
          (is (= [{:action :set :attr :report/owned :email-address "x@y.com" :scope :maintainer :id :owned-by}]
                 (commands/detect-directives :bug "Thanks for the report.\nOwned-by: x@y.com\nWill look into it.\n")))
          ;; RFC 5322 "Display Name <addr>" format
          (is (= [{:action :set :attr :report/owned :email-address "x@y.com" :scope :maintainer :id :owned-by}]
                 (commands/detect-directives :bug "Owned-by: Some User <x@y.com>\n")))
          (is (= [] (commands/detect-directives :bug "Just a normal reply.\n")))
          (is (nil? (commands/detect-directives :bug nil)))
          ;; Expiry directive
          (is (= [{:action :set-expiry :date (parse-date-iso "2026-09-01") :scope :user :id :expiry}]
                 (commands/detect-directives :bug "Expiry: 2026-09-01\n")))
          (is (= [{:action :unset-expiry :scope :setter-or-maintainer :id :unexpiry}]
                 (commands/detect-directives :bug "No expiry\n")))
          ;; "Expiry: deadline" is no longer a valid command (use :inactive-after :deadline in config)
          (is (= [] (commands/detect-directives :bug "Expiry: deadline\n")))
          ;; Deadline with duration (relative to email date)
          (let [email-date (parse-date-iso "2026-01-10")
                result (commands/detect-directives :bug "Deadline: 30d\n" nil email-date)]
            (is (= 1 (count result)))
            (is (= :set-deadline (:action (first result))))
            (is (= (parse-date-iso "2026-02-09") (:date (first result)))))
          ;; Expiry with duration (relative to email date)
          (let [email-date (parse-date-iso "2026-01-03")
                result (commands/detect-directives :bug "Expiry: 3d\n" nil email-date)]
            (is (= 1 (count result)))
            (is (= :set-expiry (:action (first result))))
            (is (= (parse-date-iso "2026-01-06") (:date (first result)))))
          ;; Compound duration
          (let [email-date (parse-date-iso "2026-01-01")
                result (commands/detect-directives :bug "Expiry: 1m 2w\n" nil email-date)]
            (is (= 1 (count result)))
            (is (= (parse-date-iso "2026-02-14") (:date (first result)))))
          ;; Expiry not applicable to announcements
          (is (= [] (commands/detect-directives :announcement "Expiry: 2026-09-01\n"))))

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
        (testing "Bug 81 Acked-by directive"
          (let [r (get-report db "<81@test.org>")]
            (is (nil? (:report/acked r)))
            (is (nil? (:report/acked-address r)))
            (is (some? (:report/owned r)))
            (is (some? (:report/urgent r)))
            (is (= "fixer@test.org" (:report/owned-address r)))
            (is (= "fixer@test.org" (:report/urgent-address r)))))

        ;; --- Bug 81 user directive denied ---
        (testing "Bug 81 user directive denied"
          (is (nil? (:report/closed (get-report db "<81@test.org>")))))

        ;; --- Bug 86 last-one-wins ---
        (testing "Bug 86 last-one-wins"
          (is (nil? (:report/acked (get-report db "<86@test.org>")))))

        ;; --- Bug 88 Closed-by + Important-by ---
        (testing "Bug 88 Closed-by + Important-by"
          (let [r (get-report db "<88@test.org>")]
            (is (some? (:report/closed r)))
            (is (= :resolved (:report/close-reason r)))
            (is (some? (:report/important r)))
            (is (= "closer@test.org" (:report/closed-address r)))
            (is (= "closer@test.org" (:report/important-address r)))))

        ;; --- Bug 90 trigger + directive ---
        (testing "Bug 90 Confirmed trigger + Owned-by directive"
          (let [r (get-report db "<90@test.org>")]
            (is (some? (:report/acked r)))
            (is (some? (:report/owned r)))
            (is (= "fixer@test.org" (:report/owned-address r)))))

        ;; --- Bug 90 Fixed + Unclosed ---
        (testing "Bug 90 Fixed + Unclosed (directive wins)"
          (is (nil? (:report/closed (get-report db "<90@test.org>")))))

        ;; --- Bug 93 Confirmed + Urgent-by ---
        (testing "Bug 93 Confirmed + Urgent-by"
          (let [r (get-report db "<93@test.org>")]
            (is (some? (:report/acked r)))
            (is (some? (:report/urgent r)))
            (is (= "user@test.org" (:report/urgent-address r)))))

        ;; --- Bug 90 Topic directive ---
        (testing "Bug 90 Topic directive"
          (is (= "regression" (:report/topic-value (get-report db "<90@test.org>")))))

        ;; --- Bug 93 Undeadline ---
        (testing "Bug 93 Undeadline removes deadline"
          (is (nil? (:report/deadline-value (get-report db "<93@test.org>")))))

        ;; --- Bug 98 standalone deadline ---
        (testing "Bug 98 standalone deadline"
          (is (some? (:report/deadline-value (get-report db "<98@test.org>")))))

        ;; --- Email 100 [source-name] prefix (bark-source fallback removed) ---
        (testing "Email 100 — no bark-source fallback"
          (is (not (report-exists? db "<100@test.org>"))))

        ;; --- CHG 102+103 / REL 104 ---
        (testing "CHG 102+103 / REL 104 release closes multiple changes"
          (let [chg1 (get-report db "<102@test.org>")
                chg2 (get-report db "<103@test.org>")
                rel  (get-report db "<104@test.org>")
                rel-mids (set (map :report/message-id (:report/related rel)))]
            (is (some? (:report/closed chg1)))
            (is (some? (:report/closed chg2)))
            (is (= :resolved (:report/close-reason chg1)))
            (is (= :resolved (:report/close-reason chg2)))
            (is (contains? rel-mids "<102@test.org>"))
            (is (contains? rel-mids "<103@test.org>"))
            (is (some #(= "<104@test.org>" (:report/message-id %))
                      (:report/related chg1)))
            (is (some #(= "<104@test.org>" (:report/message-id %))
                      (:report/related chg2)))))

        ;; --- Emails 105-107 Superseded-by ---
        (testing "Bug 105 superseded by 106"
          (let [r105 (get-report db "<105@test.org>")
                r106 (get-report db "<106@test.org>")]
            (is (some? (:report/closed r105)) "superseded report is closed")
            (is (= :superseded (:report/close-reason r105)))
            (is (= "<106@test.org>"
                    (get-in r105 [:report/superseded-by-target :report/message-id])))
            ;; Bidirectional related link
            (is (some #(= "<106@test.org>" (:report/message-id %))
                      (:report/related r105)))
            (is (some #(= "<105@test.org>" (:report/message-id %))
                      (:report/related r106)))))

        ;; --- Emails 108-110 Supersede then unsupersede ---
        ;; 109 admin supersedes 108 by 106, 110 admin "Not superseded"
        ;; reopens it.  Email 111 is a neutral user reply (no command),
        ;; so the final state of 108 is cleanly "reopened".
        (testing "Bug 108 superseded then unsuperseded"
          (let [r108 (get-report db "<108@test.org>")
                r106 (get-report db "<106@test.org>")]
            (is (nil? (:report/closed r108)) "unsuperseded report is reopened")
            (is (nil? (:report/close-reason r108)))
            (is (nil? (:report/superseded-by-target r108)))
            ;; Related link from the supersede is removed
            (is (not (some #(= "<106@test.org>" (:report/message-id %))
                           (:report/related r108))))
            (is (not (some #(= "<108@test.org>" (:report/message-id %))
                           (:report/related r106))))))

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
                    (get-in r122 [:report/superseded-by-target :report/message-id])))
            ;; Bidirectional related link set by the supersede
            (is (some #(= "<123@test.org>" (:report/message-id %))
                      (:report/related r122)))
            (is (some #(= "<122@test.org>" (:report/message-id %))
                      (:report/related r123)))))

        ;; --- Emails 114-116 Not closed on superseded report ---
        (testing "Bug 114 superseded then reopened via Not closed"
          (let [r114 (get-report db "<114@test.org>")
                r106 (get-report db "<106@test.org>")]
            (is (nil? (:report/closed r114)) "report is reopened")
            (is (nil? (:report/close-reason r114)) "close-reason is cleared")
            (is (nil? (:report/superseded-by-target r114)) "superseded-by-target is cleared")
            (is (not (some #(= "<106@test.org>" (:report/message-id %))
                           (:report/related r114)))
                "related link to superseder is removed")
            (is (not (some #(= "<114@test.org>" (:report/message-id %))
                           (:report/related r106)))
                "reverse related link is removed")))

        ;; --- Directive unit tests for supersede ---
        (testing "detect-directives: Superseded-by with angle brackets"
          (is (= [{:action :set-superseded :target-message-id "<msg@example.com>" :scope :user :id :superseded-by}]
                 (commands/detect-directives :bug "Superseded-by: <msg@example.com>\n"))))

        (testing "detect-directives: Superseded-by without angle brackets"
          (is (= [{:action :set-superseded :target-message-id "<msg@example.com>" :scope :user :id :superseded-by}]
                 (commands/detect-directives :bug "Superseded-by: msg@example.com\n"))))

        (testing "detect-directives: Not superseded"
          (is (= [{:action :unset-superseded :scope :setter-or-maintainer :id :unsuperseded}]
                 (commands/detect-directives :bug "Not superseded\n"))))

        (testing "resolve-commands: superseded-by"
          (is (= {:set {} :unset #{} :superseded-by "<mid@host>"}
                 (commands/resolve-commands
                  [{:action :set-superseded :target-message-id "<mid@host>"}]))))

        (testing "resolve-commands: unsuperseded"
          (is (= {:set {} :unset #{} :unsuperseded? true}
                 (commands/resolve-commands [{:action :unset-superseded}]))))

        (testing "resolve-commands: supersede then unsupersede"
          (is (= {:set {} :unset #{} :unsuperseded? true}
                 (commands/resolve-commands
                  [{:action :set-superseded :target-message-id "<mid@host>"}
                   {:action :unset-superseded}]))))

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
                    (get-in r117 [:report/superseded-by-target :report/message-id])))))

        (testing "Patch 118 superseded by same-subject reply 119"
          (let [r118 (get-report db "<118@test.org>")]
            (is (= :patch (:report/type r118)))
            (is (some? (:report/closed r118)) "second patch should be closed")
            (is (= :superseded (:report/close-reason r118)))
            (is (= "<119@test.org>"
                    (get-in r118 [:report/superseded-by-target :report/message-id])))))

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
                    (get-in r120 [:report/superseded-by-target :report/message-id])))))

        (testing "Inline diff 121 is open (latest)"
          (let [r121 (get-report db "<121@test.org>")]
            (is (= :patch (:report/type r121)))
            (is (nil? (:report/closed r121)) "latest diff should remain open"))))
      (finally
        (teardown! ctx)))))
