(ns bark.digest-test
  "Integration tests for bark.digest.
  Creates a temporary Datalevin DB, inserts test emails, processes them
  via bark.digest/process-email!, and verifies reports, commands,
  threading, votes, roles, and permissions.

  Ported from test/bark-digest-test.clj (bb version)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [datalevin.core :as d]
            [bark.commands :as commands]
            [bark.common :as common]
            [bark.digest :as digest]
            [bark.roles :as roles])
  (:import [java.text SimpleDateFormat]
           [java.util TimeZone]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- parse-date-iso [s]
  (let [fmt (doto (SimpleDateFormat. "yyyy-MM-dd")
              (.setTimeZone (TimeZone/getTimeZone "UTC")))]
    (.parse fmt s)))

(defn- get-report [db message-id]
  (d/pull db
          '[:report/type :report/version :report/topic
            :report/patch-seq :report/patch-source :report/message-id
            :report/acked :report/owned :report/closed
            :report/close-reason
            :report/urgent :report/important
            :report/deadline
            {:report/acked-proxy [:email/from-address]}
            {:report/owned-proxy [:email/from-address]}
            {:report/closed-proxy [:email/from-address]}
            {:report/urgent-proxy [:email/from-address]}
            {:report/important-proxy [:email/from-address]}
            :report/votes-up :report/votes-down :report/votes-null :report/voters
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

;; ---------------------------------------------------------------------------
;; Fixture data
;; ---------------------------------------------------------------------------

(def source-map {"direct"      {:admin "admin@test.org"}
                 "public-list" {:admin     "admin@test.org"
                                :list-post "list@test.org"}})

(def sources [{:name "public-list"
               :match {:list-id "list.test.org"}
               :list-post "list@test.org"}
              {:name "direct"}])

;; ---------------------------------------------------------------------------
;; DB setup / teardown
;; ---------------------------------------------------------------------------

(defn- setup-db! []
  (let [db-path (str "/tmp/bark-test-" (System/currentTimeMillis))
        conn    (d/get-conn db-path common/bark-schema)]
    ;; Setup roles
    (d/transact! conn [{:roles/source      "direct"
                        :roles/admin       "admin@test.org"
                        :roles/maintainers "admin@test.org"}])
    (d/transact! conn [{:roles/source      "public-list"
                        :roles/admin       "admin@test.org"
                        :roles/maintainers "admin@test.org"}])
    ;; Insert test emails
    (let [emails (edn/read-string (slurp "resources/emails.edn"))]
      (doseq [email emails]
        (d/transact! conn [email])))
    {:conn conn :db-path db-path}))

(defn- teardown! [{:keys [conn db-path]}]
  (d/close conn)
  (let [dir (io/file db-path)]
    (when (.exists dir)
      (doseq [f (reverse (file-seq dir))]
        (.delete f)))))

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

(deftest digest-integration-test
  (let [{:keys [conn] :as ctx} (setup-db!)]
    (try
      (process-all-emails! conn)
      (let [db (d/db conn)]

        ;; --- Roles ---
        (testing "Roles (final state)"
          (let [r (d/pull db '[:roles/admin :roles/maintainers :roles/ignored]
                          [:roles/source "direct"])]
            (is (= "admin@test.org" (:roles/admin r)))
            (is (contains? (set (:roles/maintainers r)) "maint@test.org"))
            (is (not (contains? (set (:roles/ignored r)) "spam@test.org")))))

        ;; --- Bug 02 lifecycle ---
        (testing "Bug 02 lifecycle"
          (let [r (get-report db "<02@test.org>")]
            (is (= :bug (:report/type r)))
            (is (= "9.7" (:report/topic r)))
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
            (is (nil? (:report/topic r)))))

        ;; --- Patch 07 subject detection ---
        (testing "Patch 07 subject detection"
          (let [r (get-report db "<07@test.org>")]
            (is (= :patch (:report/type r)))
            (is (= "org-agenda" (:report/topic r)))
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
          (let [r (get-report db "<11@test.org>")]
            (is (= :request (:report/type r)))
            (is (= 1 (:report/votes-up r)))
            (is (= 1 (:report/votes-down r)))
            (is (= 2 (count (:report/voters r))))
            (is (= 3 (count (:report/descendants r))))))

        ;; --- TODO 15 request lifecycle ---
        (testing "TODO 15 request lifecycle"
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

        ;; --- Email 21 ignored ---
        (testing "Email 21 ignored user"
          (is (not (report-exists? db "<21@test.org>"))))

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
            (is (= "refactor" (:report/topic r)))
            (is (= "2/3" (:report/patch-seq r)))))

        ;; --- Email 29 role control via mailing list ---
        (testing "Email 29 role control via mailing list"
          (let [r (d/pull db '[:roles/maintainers] [:roles/source "direct"])]
            (is (not (contains? (set (:roles/maintainers r)) "evil@hacker.org")))))

        ;; --- Email 30 bug via list ---
        (testing "Email 30 bug via list"
          (is (= :bug (:report/type (get-report db "<30@test.org>")))))

        ;; --- Email 31 bypassing list ---
        (testing "Email 31 bypassing list"
          (is (not (report-exists? db "<31@test.org>"))))

        ;; --- Email 32 wrong list ---
        (testing "Email 32 wrong List-Post"
          (is (not (report-exists? db "<32@test.org>"))))

        ;; --- Email 33 admin bypass ---
        (testing "Email 33 admin bypass"
          (is (= :bug (:report/type (get-report db "<33@test.org>")))))

        ;; --- Series v1 ---
        (testing "Series v1 emails 34-37"
          (let [v1 (get-series-by-id db "parser|user@test.org|3")]
            (is (= "parser" (:series/topic v1)))
            (is (= 3 (:series/expected v1)))
            (is (= "<34@test.org>" (get-in v1 [:series/cover-letter :email/message-id])))
            (is (= 3 (series-patch-count db "parser|user@test.org|3")))
            (is (some? (:series/closed v1))))
          (let [r35 (get-report db "<35@test.org>")]
            (is (some? (:report/series r35)))
            (is (= "1/3" (:report/patch-seq r35)))))

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
          (let [r (d/pull db '[:roles/maintainers] [:roles/source "direct"])]
            (is (not (contains? (set (:roles/maintainers r)) "maint2@test.org")))))

        ;; --- Email 44 unignore ---
        (testing "Email 44 unignore"
          (let [r (d/pull db '[:roles/ignored] [:roles/source "direct"])]
            (is (not (contains? (set (:roles/ignored r)) "spam@test.org")))))

        ;; --- Email 45 maintainer adds peer ---
        (testing "Email 45 maintainer adds peer"
          (let [r (d/pull db '[:roles/maintainers] [:roles/source "direct"])]
            (is (contains? (set (:roles/maintainers r)) "maint3@test.org"))))

        ;; --- Email 47 user can't add maintainer ---
        (testing "Email 47 user can't add maintainer"
          (let [r (d/pull db '[:roles/maintainers] [:roles/source "direct"])]
            (is (not (contains? (set (:roles/maintainers r)) "user@test.org")))))

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

        ;; --- Emails 56-57 notify prefs ---
        (testing "Emails 56-57 notify prefs"
          (let [pref (d/pull db '[:notify/enabled :notify/interval-days :notify/min-priority]
                             [:notify/key "direct:maint@test.org"])]
            (is (:notify/enabled pref))
            (is (= 7 (:notify/interval-days pref)))
            (is (= 2 (:notify/min-priority pref)))))

        ;; --- Email 58 notify from regular user ---
        (testing "Email 58 notify from regular user"
          (is (nil? (d/q '[:find ?e . :in $ ?k :where [?e :notify/key ?k]]
                         db "direct:user@test.org"))))

        ;; --- Email 74 notify via mailing list ---
        (testing "Email 74 notify via mailing list"
          (let [pref (d/pull db '[:notify/interval-days]
                             [:notify/key "direct:maint@test.org"])]
            (is (= 7 (:notify/interval-days pref)))))

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

        ;; --- Email 67 maintainer ignores address ---
        (testing "Email 67 maintainer ignores address"
          (let [r (d/pull db '[:roles/ignored] [:roles/source "direct"])]
            (is (contains? (set (:roles/ignored r)) "nuisance@test.org"))))

        ;; --- Emails 68-69 series without cover letter ---
        (testing "Emails 68-69 series without cover letter"
          (let [r68 (get-report db "<68@test.org>")
                r69 (get-report db "<69@test.org>")]
            (is (= :patch (:report/type r68)))
            (is (= "1/2" (:report/patch-seq r68)))
            (is (= "2/2" (:report/patch-seq r69)))
            (is (some? (:report/series r68)))
            (is (some? (:report/series r69)))))

        ;; --- Email 70 different sender ---
        (testing "Email 70 different sender same topic"
          (let [r70 (get-report db "<70@test.org>")
                r68 (get-report db "<68@test.org>")]
            (is (= :patch (:report/type r70)))
            (is (not= (get-in r68 [:report/series :series/id])
                      (get-in r70 [:report/series :series/id])))))

        ;; --- POLL 75 vote variants ---
        (testing "POLL 75 vote format variants"
          (let [r (get-report db "<75@test.org>")]
            (is (= :request (:report/type r)))
            (is (= 2 (:report/votes-up r)))
            (is (= 1 (:report/votes-down r)))
            (is (= 1 (:report/votes-null r)))
            (is (= 4 (count (:report/voters r))))
            (is (not (contains? (set (:report/voters r)) "admin@test.org")))))

        ;; --- Directive unit tests ---
        (testing "detect-directives"
          (is (= [{:action :set :attr :report/acked :email-address "a@b.com" :scope :maintainer}]
                 (commands/detect-directives :bug "Acked-by: a@b.com\n")))
          (is (= [{:action :set :attr :report/owned :email-address "x@y.com" :scope :maintainer}
                  {:action :set :attr :report/urgent :email-address "x@y.com" :scope :maintainer}]
                 (commands/detect-directives :bug "Owned-by: x@y.com\nUrgent-by: x@y.com\n")))
          (is (= [{:action :unset :attr :report/acked :scope :maintainer}]
                 (commands/detect-directives :bug "Unacked\n")))
          (is (= [{:action :unset :attr :report/urgent :scope :maintainer}]
                 (commands/detect-directives :bug "Unurgent\n")))
          (is (= [{:action :unset :attr :report/important :scope :maintainer}]
                 (commands/detect-directives :bug "Unimportant\n")))
          (is (= [{:action :set-deadline :date (parse-date-iso "2026-06-15") :scope :maintainer}]
                 (commands/detect-directives :bug "Deadline: 2026-06-15\n")))
          (is (= [{:action :unset-deadline :scope :maintainer}]
                 (commands/detect-directives :bug "Undeadline\n")))
          (is (= [{:action :set-topic :topic "my-topic" :scope :maintainer}]
                 (commands/detect-directives :bug "Topic: my-topic\n")))
          (is (= [{:action :set :attr :report/acked :email-address "a@b.com" :scope :maintainer}
                  {:action :set-deadline :date (parse-date-iso "2026-07-01") :scope :maintainer}
                  {:action :set-topic :topic "urgent-fix" :scope :maintainer}]
                 (commands/detect-directives :bug "Acked-by: a@b.com\nDeadline: 2026-07-01\nTopic: urgent-fix\n")))
          (is (= [{:action :set :attr :report/owned :email-address "x@y.com" :scope :maintainer}]
                 (commands/detect-directives :bug "Thanks for the report.\nOwned-by: x@y.com\nWill look into it.\n")))
          (is (= [] (commands/detect-directives :bug "Just a normal reply.\n")))
          (is (nil? (commands/detect-directives :bug nil))))

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
                                             {:action :set-topic :topic "second"}]))))

        ;; --- Bug 81 directives ---
        (testing "Bug 81 Acked-by directive"
          (let [r (get-report db "<81@test.org>")]
            (is (nil? (:report/acked r)))
            (is (nil? (:report/acked-proxy r)))
            (is (some? (:report/owned r)))
            (is (some? (:report/urgent r)))
            (is (= "maint@test.org" (get-in r [:report/owned-proxy :email/from-address])))
            (is (= "maint@test.org" (get-in r [:report/urgent-proxy :email/from-address])))))

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
            (is (= "admin@test.org" (get-in r [:report/closed-proxy :email/from-address])))
            (is (= "admin@test.org" (get-in r [:report/important-proxy :email/from-address])))))

        ;; --- Bug 90 trigger + directive ---
        (testing "Bug 90 Confirmed trigger + Owned-by directive"
          (let [r (get-report db "<90@test.org>")]
            (is (some? (:report/acked r)))
            (is (some? (:report/owned r)))
            (is (= "maint@test.org" (get-in r [:report/owned-proxy :email/from-address])))))

        ;; --- Bug 90 Fixed + Unclosed ---
        (testing "Bug 90 Fixed + Unclosed (directive wins)"
          (is (nil? (:report/closed (get-report db "<90@test.org>")))))

        ;; --- Bug 93 Confirmed + Urgent-by ---
        (testing "Bug 93 Confirmed + Urgent-by"
          (let [r (get-report db "<93@test.org>")]
            (is (some? (:report/acked r)))
            (is (some? (:report/urgent r)))
            (is (= "admin@test.org" (get-in r [:report/urgent-proxy :email/from-address])))))

        ;; --- Bug 90 Topic directive ---
        (testing "Bug 90 Topic directive"
          (is (= "regression" (:report/topic (get-report db "<90@test.org>")))))

        ;; --- Bug 93 Undeadline ---
        (testing "Bug 93 Undeadline removes deadline"
          (is (nil? (:report/deadline (get-report db "<93@test.org>")))))

        ;; --- Bug 98 standalone deadline ---
        (testing "Bug 98 standalone deadline"
          (is (some? (:report/deadline (get-report db "<98@test.org>")))))

        ;; --- Email 100 [bark:list-id] prefix ---
        (testing "Email 100 [bark:list-id] subject prefix"
          (let [r   (get-report db "<100@test.org>")
                eid (d/q '[:find ?e . :in $ ?mid :where [?e :email/message-id ?mid]]
                         db "<100@test.org>")
                src (when eid (:email/source (d/pull db '[:email/source] eid)))]
            (is (some? r))
            (is (= :bug (:report/type r)))
            (is (= "public-list" src))))

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
                      (:report/related chg2))))))
      (finally
        (teardown! ctx)))))
