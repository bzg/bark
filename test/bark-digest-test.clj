#!/usr/bin/env bb

;; test/bark-digest-test.clj — Integration tests for bark-digest.
;;
;; Creates a temporary datalevin DB, inserts test emails, runs cmd-digest!,
;; and verifies reports, commands, threading, votes, roles, and permissions.
;;
;; Usage:
;;   bb test/bark-digest-test.clj
;;
;; Requires: datalevin pod (version from bark-common.clj), resources/bark-schema.edn.

(require '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io])

(load-file "scripts/bark-common.clj")

(load-datalevin-pod!)

;; ---------------------------------------------------------------------------
;; Test harness
;; ---------------------------------------------------------------------------

(def ^:dynamic *test-counts* (atom {:pass 0 :fail 0}))

(defn assert-test [label pred]
  (if pred
    (do (swap! *test-counts* update :pass inc)
        (println (str "  ✓ " label)))
    (do (swap! *test-counts* update :fail inc)
        (println (str "  ✗ FAIL: " label)))))

(defn assert= [label expected actual]
  (assert-test (str label " — expected: " (pr-str expected) " got: " (pr-str actual))
               (= expected actual)))

;; ---------------------------------------------------------------------------
;; Load bark-digest functions (everything except the main block)
;; ---------------------------------------------------------------------------

(def schema (edn/read-string (slurp "resources/bark-schema.edn")))

(load-file "scripts/bark-digest.clj")

;; ---------------------------------------------------------------------------
;; Unit tests: detect-vote patterns
;; ---------------------------------------------------------------------------

(println "\n--- detect-vote unit tests ---")

;; Up votes
(assert= "detect +1 (bare)"         :up (detect-vote "+1"))
(assert= "detect +1\\n"             :up (detect-vote "+1\n"))
(assert= "detect +1 with text"      :up (detect-vote "+1 thanks"))
(assert= "detect +1, punctuation"   :up (detect-vote "+1, great idea"))
(assert= "detect +1."               :up (detect-vote "+1."))
(assert= "detect +1!"               :up (detect-vote "+1!"))
(assert= "detect 1+ (reversed)"     :up (detect-vote "1+"))
(assert= "detect 1+ with space"     :up (detect-vote "1+ "))
(assert= "detect +1 mid-body"       :up (detect-vote "I agree\n+1\nthanks"))

;; Down votes
(assert= "detect -1"                :down (detect-vote "-1"))
(assert= "detect 1-"                :down (detect-vote "1-"))
(assert= "detect -1 with text"      :down (detect-vote "-1 nope"))
(assert= "detect 1-."               :down (detect-vote "1-."))

;; Null votes
(assert= "detect +0"                :null (detect-vote "+0"))
(assert= "detect -0"                :null (detect-vote "-0"))
(assert= "detect 0+"                :null (detect-vote "0+"))
(assert= "detect 0-"                :null (detect-vote "0-"))
(assert= "detect +0, meh"           :null (detect-vote "+0, meh"))

;; Non-votes (digit or alpha follows)
(assert= "+10 is NOT a vote"        nil (detect-vote "+10"))
(assert= "+1abc is NOT a vote"      nil (detect-vote "+1abc"))
(assert= "1+2 is NOT a vote"        nil (detect-vote "1+2"))
(assert= "-10 is NOT a vote"        nil (detect-vote "-10"))
(assert= "-1a is NOT a vote"        nil (detect-vote "-1a"))
(assert= "1-2 is NOT a vote"        nil (detect-vote "1-2"))
(assert= "+0x is NOT a vote"        nil (detect-vote "+0x"))
(assert= "plain text is NOT a vote" nil (detect-vote "nothing here"))
(assert= "empty is NOT a vote"      nil (detect-vote ""))
(assert= "nil is NOT a vote"        nil (detect-vote nil))

;; ---------------------------------------------------------------------------
;; Test setup
;; ---------------------------------------------------------------------------

(defn setup-db!
  "Create a temp datalevin DB, insert roles and test emails."
  []
  (let [db-path (str "/tmp/bark-test-" (System/currentTimeMillis))
        conn    (d/get-conn db-path schema)]
    ;; Setup roles for "direct" source
    (d/transact! conn [{:roles/source      "direct"
                        :roles/admin       "admin@test.org"
                        :roles/maintainers "admin@test.org"}])
    ;; Setup roles for "public-list" source (list-backed)
    (d/transact! conn [{:roles/source      "public-list"
                        :roles/admin       "admin@test.org"
                        :roles/maintainers "admin@test.org"}])
    ;; Insert test emails
    (let [emails (edn/read-string (slurp "test/emails.edn"))]
      (doseq [email emails]
        (d/transact! conn [email])))
    {:conn conn :db-path db-path}))

(defn teardown! [{:keys [conn db-path]}]
  (d/close conn)
  (let [dir (io/file db-path)]
    (when (.exists dir)
      (doseq [f (reverse (file-seq dir))]
        (.delete f)))))

(defn get-report [db message-id]
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

(defn get-series-by-id [db sid]
  (when-let [eid (d/q '[:find ?s . :in $ ?sid :where [?s :series/id ?sid]] db sid)]
    (d/pull db
            '[:series/id :series/topic :series/sender :series/expected
              :series/closed :series/patches
              {:series/cover-letter [:email/message-id]}]
            eid)))

(defn series-patch-count [db sid]
  (when-let [eid (d/q '[:find ?s . :in $ ?sid :where [?s :series/id ?sid]] db sid)]
    (count (d/q '[:find [?r ...]
                  :in $ ?s
                  :where [?s :series/patches ?r]]
                db eid))))

(defn report-exists? [db message-id]
  (some? (d/q '[:find ?r .
                :in $ ?mid
                :where [?r :report/message-id ?mid]]
              db message-id)))

;; ---------------------------------------------------------------------------
;; Run digest
;; ---------------------------------------------------------------------------

(def source-map {"direct"      {:admin "admin@test.org"}
                 "public-list" {:admin              "admin@test.org"
                                :list-post "list@test.org"}})

(def sources [{:name "public-list"
               :match {:list-id "list.test.org"}
               :list-post "list@test.org"}
              {:name "direct"}])

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(defn run-tests []
  (let [{:keys [conn] :as ctx} (setup-db!)]
    (try
      ;; Run digest over all emails
      (println "\n=== Running cmd-digest! ===\n")
      (cmd-digest! conn source-map sources true)

      (let [db (d/db conn)]

        ;; --- Roles (final state after all emails) ---
        (println "\n--- Roles (final state) ---")
        (let [roles (d/pull db '[:roles/admin :roles/maintainers :roles/ignored]
                            [:roles/source "direct"])]
          ;; Admin unchanged (Add admin is no longer a valid command)
          (assert= "Admin is admin@test.org (unchanged)"
                   "admin@test.org" (:roles/admin roles))
          (assert-test "maint@test.org is maintainer"
                       (contains? (set (:roles/maintainers roles)) "maint@test.org"))
          ;; spam@test.org was unignored by email 44
          (assert-test "spam@test.org NOT ignored (unignored by email 44)"
                       (not (contains? (set (:roles/ignored roles)) "spam@test.org"))))

        ;; --- Bug 02: full lifecycle ---
        (println "\n--- Bug 02: [BUG 9.7] lifecycle ---")
        (let [r (get-report db "<02@test.org>")]
          (assert= "Type is :bug" :bug (:report/type r))
          (assert= "Topic is 9.7" "9.7" (:report/topic r))
          (assert-test "Acked (Confirmed)" (some? (:report/acked r)))
          (assert-test "Owned (Handled)" (some? (:report/owned r)))
          (assert-test "Closed (Fixed)" (some? (:report/closed r)))
          (assert= "Close-reason is :resolved" :resolved (:report/close-reason r))
          (assert-test "Urgent still set (unsetting reserved to maintainer directives)" (some? (:report/urgent r)))
          (assert= "3 descendants" 3
                   (count (:report/descendants r))))

        ;; --- Bug 03: mailing list prefix ---
        (println "\n--- Bug 03: mailing list prefix ---")
        (let [r (get-report db "<03@test.org>")]
          (assert= "Type is :bug" :bug (:report/type r))
          (assert= "No topic" nil (:report/topic r)))

        ;; --- Patch 07: subject detection ---
        (println "\n--- Patch 07: [PATCH subject] ---")
        (let [r (get-report db "<07@test.org>")]
          (assert= "Type is :patch" :patch (:report/type r))
          (assert= "Topic" "org-agenda" (:report/topic r))
          (assert= "Seq" "1/2" (:report/patch-seq r))
          (assert-test ":subject in patch-source"
                       (contains? (set (:report/patch-source r)) :subject))
          (assert-test "Acked (Reviewed)" (some? (:report/acked r)))
          (assert-test "Closed (Applied)" (some? (:report/closed r)))
          (assert= "Close-reason is :resolved" :resolved (:report/close-reason r)))

        ;; --- Patch 08: attachment detection ---
        (println "\n--- Patch 08: attachment detection ---")
        (let [r (get-report db "<08@test.org>")]
          (assert= "Type is :patch" :patch (:report/type r))
          (assert-test ":attachment in patch-source"
                       (contains? (set (:report/patch-source r)) :attachment)))

        ;; --- Patch 09: inline diff detection ---
        (println "\n--- Patch 09: inline diff detection ---")
        (let [r (get-report db "<09@test.org>")]
          (assert= "Type is :patch" :patch (:report/type r))
          (assert-test ":inline in patch-source"
                       (contains? (set (:report/patch-source r)) :inline)))

        ;; --- POLL 11: votes ---
        (println "\n--- POLL 11: votes ---")
        (let [r (get-report db "<11@test.org>")]
          (assert= "Type is :request" :request (:report/type r))
          (assert= "1 vote up" 1 (:report/votes-up r))
          (assert= "1 vote down" 1 (:report/votes-down r))
          (assert= "2 voters (dedup)" 2 (count (:report/voters r)))
          (assert= "3 descendants (incl. dup vote)" 3
                   (count (:report/descendants r))))

        ;; --- TODO 15: closed ---
        (println "\n--- TODO 15: request lifecycle ---")
        (let [r (get-report db "<15@test.org>")]
          (assert= "Type is :request" :request (:report/type r))
          (assert-test "Closed (Done)" (some? (:report/closed r)))
          (assert= "Close-reason is :resolved" :resolved (:report/close-reason r)))

        ;; --- ANN 17: canceled ---
        (println "\n--- ANN 17: announcement canceled ---")
        (let [r (get-report db "<17@test.org>")]
          (assert= "Type is :announcement" :announcement (:report/type r))
          (assert-test "Closed (Canceled)" (some? (:report/closed r)))
          (assert= "Close-reason is :canceled" :canceled (:report/close-reason r)))

        ;; --- ANN 18: denied (user cannot create announcements) ---
        (println "\n--- ANN 18: permission denied ---")
        (assert-test "No report for unauthorized announcement"
                     (not (report-exists? db "<18@test.org>")))

        ;; --- CHG 19 auto-closed by REL 20 ---
        (println "\n--- CHG 19 / REL 20: release closes change ---")
        (let [chg (get-report db "<19@test.org>")
              rel (get-report db "<20@test.org>")]
          (assert= "CHG type" :change (:report/type chg))
          (assert= "CHG version" "9.8" (:report/version chg))
          (assert-test "CHG auto-closed" (some? (:report/closed chg)))
          (assert= "CHG close-reason is :resolved" :resolved (:report/close-reason chg))
          (assert= "REL type" :release (:report/type rel))
          (assert= "REL version" "9.8" (:report/version rel))
          ;; Cross-thread relation: [REL] and [CHG] are not in the same
          ;; email thread, but closing a CHG via REL links them as related.
          (assert-test "REL is related to CHG"
                       (some #(= "<19@test.org>" (:report/message-id %))
                             (:report/related rel)))
          (assert-test "CHG is related to REL"
                       (some #(= "<20@test.org>" (:report/message-id %))
                             (:report/related chg))))

        ;; --- Email 21: ignored ---
        (println "\n--- Email 21: ignored user ---")
        (assert-test "No report from ignored user"
                     (not (report-exists? db "<21@test.org>")))

        ;; --- Bug 23: important flag set (unsetting now requires maintainer directive) ---
        (println "\n--- Bug 23: important set (no user unset) ---")
        (let [r (get-report db "<23@test.org>")]
          (assert= "Type is :bug" :bug (:report/type r))
          (assert-test "Important still set (unsetting reserved to maintainer directives)" (some? (:report/important r)))
          (assert= "3 descendants" 3
                   (count (:report/descendants r))))

        ;; --- FR 26: request ---
        (println "\n--- FR 26: request ---")
        (let [r (get-report db "<26@test.org>")]
          (assert= "Type is :request" :request (:report/type r)))

        ;; --- ANN 27: mailing list prefix + Archived-At header ---
        (println "\n--- ANN 27: mailing list prefix + headers ---")
        (let [r (get-report db "<27@test.org>")]
          (assert= "Type is :announcement" :announcement (:report/type r))
          (let [edn-str (get-in r [:report/email :email/headers-edn])
                headers (when edn-str (edn/read-string edn-str))]
            (assert= "Archived-At header preserved"
                     "https://list.example.org/archive/27"
                     (get headers "Archived-At"))))

        ;; --- Patch 28: mailing list prefix + seq + topic ---
        (println "\n--- Patch 28: mailing list prefix with seq/topic ---")
        (let [r (get-report db "<28@test.org>")]
          (assert= "Type is :patch" :patch (:report/type r))
          (assert= "Topic" "refactor" (:report/topic r))
          (assert= "Seq" "2/3" (:report/patch-seq r)))

        ;; --- Email 29: role command via mailing list (List-Id blocks it) ---
        (println "\n--- Email 29: role command via mailing list ---")
        (let [roles (d/pull db '[:roles/maintainers]
                            [:roles/source "direct"])]
          (assert-test "evil@hacker.org NOT added as maintainer"
                       (not (contains? (set (:roles/maintainers roles))
                                       "evil@hacker.org"))))

        ;; --- Email 30: bug via list with correct List-Post (allowed) ---
        (println "\n--- Email 30: bug via list with correct List-Post ---")
        (let [r (get-report db "<30@test.org>")]
          (assert= "Type is :bug" :bug (:report/type r)))

        ;; --- Email 31: bug direct to list-backed source, no List-Post (denied) ---
        (println "\n--- Email 31: bug bypassing list (no List-Post) ---")
        (assert-test "No report for direct email to list-backed source"
                     (not (report-exists? db "<31@test.org>")))

        ;; --- Email 32: bug via wrong list (denied) ---
        (println "\n--- Email 32: bug via wrong List-Post ---")
        (assert-test "No report for wrong List-Post"
                     (not (report-exists? db "<32@test.org>")))

        ;; --- Email 33: admin direct to list-backed source (allowed, admin bypass) ---
        (println "\n--- Email 33: admin bypasses List-Post check ---")
        (let [r (get-report db "<33@test.org>")]
          (assert= "Type is :bug" :bug (:report/type r)))

        ;; --- Series v1: emails 34-37 ---
        (println "\n--- Series v1: [PATCH parser 0/3] through 3/3 ---")
        (let [v1 (get-series-by-id db "parser|user@test.org|3")]
          (assert= "v1 topic" "parser" (:series/topic v1))
          (assert= "v1 expected" 3 (:series/expected v1))
          (assert= "v1 cover letter" "<34@test.org>"
                   (get-in v1 [:series/cover-letter :email/message-id]))
          (assert= "v1 has 3 patches" 3
                   (series-patch-count db "parser|user@test.org|3"))
          (assert-test "v1 is closed (superseded by v2)"
                       (some? (:series/closed v1))))

        (let [r35 (get-report db "<35@test.org>")]
          (assert-test "Patch 1/3 has series ref"
                       (some? (:report/series r35)))
          (assert= "Patch 1/3 seq" "1/3" (:report/patch-seq r35)))

        ;; --- Series v2: emails 38-39 ---
        (println "\n--- Series v2: new series after restart ---")
        (let [v2 (get-series-by-id db "parser|user@test.org|3#2")]
          (assert-test "v2 series exists" (some? (:series/id v2)))
          (assert-test "v2 series is not closed"
                       (nil? (:series/closed v2)))
          (assert= "v2 expected 3" 3 (:series/expected v2))
          (assert= "v2 cover letter" "<38@test.org>"
                   (get-in v2 [:series/cover-letter :email/message-id]))
          (assert= "v2 has 1 patch" 1
                   (series-patch-count db "parser|user@test.org|3#2")))

        (let [r39 (get-report db "<39@test.org>")
              s   (:report/series r39)]
          (assert-test "v2 patch has series ref" (some? s))
          (assert= "v2 patch seq" "1/3" (:report/patch-seq r39)))

        ;; --- Email 40: patch related to bug 23 ---
        (println "\n--- Email 40: patch related to bug ---")
        (let [patch (get-report db "<40@test.org>")
              bug   (get-report db "<23@test.org>")]
          (assert= "Patch type" :patch (:report/type patch))
          (assert-test "Patch is related to bug"
                       (some #(= "<23@test.org>" (:report/message-id %))
                             (:report/related patch)))
          (assert-test "Bug is related to patch"
                       (some #(= "<40@test.org>" (:report/message-id %))
                             (:report/related bug))))

        ;; =================================================================
        ;; NEW TESTS (emails 41-74)
        ;; =================================================================

        ;; --- Remove maintainer (emails 41-42) ---
        (println "\n--- Emails 41-42: add then remove maintainer ---")
        (let [roles (d/pull db '[:roles/maintainers]
                            [:roles/source "direct"])]
          (assert-test "maint2@test.org NOT in maintainers (removed)"
                       (not (contains? (set (:roles/maintainers roles))
                                       "maint2@test.org"))))

        ;; --- Unignore (email 44) ---
        (println "\n--- Email 44: unignore spam@test.org ---")
        (let [roles (d/pull db '[:roles/ignored]
                            [:roles/source "direct"])]
          (assert-test "spam@test.org NOT in ignored (unignored)"
                       (not (contains? (set (:roles/ignored roles))
                                       "spam@test.org"))))

        ;; --- Maintainer adds maintainer (email 45) ---
        (println "\n--- Email 45: maintainer adds peer ---")
        (let [roles (d/pull db '[:roles/maintainers]
                            [:roles/source "direct"])]
          (assert-test "maint3@test.org IS maintainer"
                       (contains? (set (:roles/maintainers roles))
                                  "maint3@test.org")))

        ;; --- Regular user can't add maintainer (email 47) ---
        (println "\n--- Email 47: user can't add maintainer ---")
        (let [roles (d/pull db '[:roles/maintainers]
                            [:roles/source "direct"])]
          (assert-test "user@test.org NOT in maintainers"
                       (not (contains? (set (:roles/maintainers roles))
                                       "user@test.org"))))

        ;; --- Command with semicolon (emails 48-49) ---
        (println "\n--- Emails 48-49: command with semicolon ---")
        (let [r (get-report db "<48@test.org>")]
          (assert= "Type is :bug" :bug (:report/type r))
          (assert-test "Acked via Approved;" (some? (:report/acked r))))

        ;; --- Inline command ignored (emails 50-51) ---
        (println "\n--- Emails 50-51: inline command ignored ---")
        (let [r (get-report db "<50@test.org>")]
          (assert= "Type is :bug" :bug (:report/type r))
          (assert-test "NOT closed (inline 'Fixed.' ignored)"
                       (nil? (:report/closed r))))

        ;; --- Closed. on request (emails 52-53) ---
        (println "\n--- Emails 52-53: Closed. on request ---")
        (let [r (get-report db "<52@test.org>")]
          (assert= "Type is :request" :request (:report/type r))
          (assert-test "Closed via Closed." (some? (:report/closed r))))

        ;; --- Commands on announcement (emails 54-55) ---
        (println "\n--- Emails 54-55: commands on announcement ---")
        (let [r (get-report db "<54@test.org>")]
          (assert= "Type is :announcement" :announcement (:report/type r))
          (assert-test "NOT acked (announcements can't be acked)"
                       (nil? (:report/acked r)))
          (assert-test "NOT owned (announcements can't be owned)"
                       (nil? (:report/owned r)))
          (assert-test "Urgent (applies to all report types)"
                       (some? (:report/urgent r))))

        ;; --- Notify off then on with prefs (emails 56-57) ---
        (println "\n--- Emails 56-57: notify off then on with prefs ---")
        (let [k     "direct:maint@test.org"
              pref  (d/pull db '[:notify/enabled :notify/interval-days
                                 :notify/min-priority]
                            [:notify/key k])]
          (assert-test "Notify re-enabled" (:notify/enabled pref))
          (assert= "Interval set to 7" 7 (:notify/interval-days pref))
          (assert= "Min priority set to 2" 2 (:notify/min-priority pref)))

        ;; --- Notify from regular user (email 58) ---
        (println "\n--- Email 58: notify from regular user ---")
        (let [k    "direct:user@test.org"
              pref (d/q '[:find ?e .
                          :in $ ?k
                          :where [?e :notify/key ?k]]
                        db k)]
          (assert-test "No notify pref created for regular user"
                       (nil? pref)))

        ;; --- Notify via mailing list (email 74) ---
        (println "\n--- Email 74: notify via mailing list ---")
        (let [k    "direct:maint@test.org"
              pref (d/pull db '[:notify/interval-days]
                           [:notify/key k])]
          (assert= "Interval still 7 (list notify ignored)"
                   7 (:notify/interval-days pref)))

        ;; --- Case insensitive [bug] (email 59) ---
        (println "\n--- Email 59: case insensitive [bug] ---")
        (let [r (get-report db "<59@test.org>")]
          (assert= "Type is :bug" :bug (:report/type r)))

        ;; --- [ANNOUNCEMENT] long form (email 60) ---
        (println "\n--- Email 60: [ANNOUNCEMENT] long form ---")
        (let [r (get-report db "<60@test.org>")]
          (assert= "Type is :announcement" :announcement (:report/type r)))

        ;; --- Permission denials (emails 71-73) ---
        (println "\n--- Emails 71-73: permission denials ---")
        (assert-test "No report for user [ANN]"
                     (not (report-exists? db "<71@test.org>")))
        (assert-test "No report for user [REL]"
                     (not (report-exists? db "<72@test.org>")))
        (assert-test "No report for user [CHG]"
                     (not (report-exists? db "<73@test.org>")))

        ;; --- References-only threading (email 61) ---
        (println "\n--- Email 61: threading via References only ---")
        (let [r (get-report db "<59@test.org>")]
          (assert-test "Bug 59 acked via References-only reply"
                       (some? (:report/acked r))))

        ;; --- Deep thread (email 62) ---
        (println "\n--- Email 62: deep thread command ---")
        (let [r (get-report db "<59@test.org>")]
          (assert-test "Bug 59 closed via grandchild reply"
                       (some? (:report/closed r)))
          (assert-test "Bug 59 has >= 2 descendants"
                       (>= (count (:report/descendants r)) 2)))

        ;; --- Orphan email (email 63) ---
        (println "\n--- Email 63: orphan email ---")
        (assert-test "No report for orphan email"
                     (not (report-exists? db "<63@test.org>")))

        ;; --- Duplicate tag in same thread (email 64) ---
        (println "\n--- Email 64: [bug] reply in same thread ---")
        ;; Email 64 is a [bug] reply to bug 59. Since it has its own
        ;; [bug] tag, it creates a separate report (not just a descendant).
        ;; It also threads as a descendant of bug 59.
        (assert-test "Email 64 creates its own report (has [bug] tag)"
                     (report-exists? db "<64@test.org>"))

        ;; --- HTML-only body fallback (emails 65-66) ---
        (println "\n--- Emails 65-66: body-text-from-html fallback ---")
        (let [r (get-report db "<65@test.org>")]
          (assert= "Type is :bug" :bug (:report/type r))
          (assert-test "Acked via html body command"
                       (some? (:report/acked r))))

        ;; --- Maintainer ignores address (email 67) ---
        (println "\n--- Email 67: maintainer ignores address ---")
        (let [roles (d/pull db '[:roles/ignored]
                            [:roles/source "direct"])]
          (assert-test "nuisance@test.org IS ignored"
                       (contains? (set (:roles/ignored roles))
                                  "nuisance@test.org")))

        ;; --- Series without cover letter (emails 68-69) ---
        (println "\n--- Emails 68-69: series without cover letter ---")
        (let [r68 (get-report db "<68@test.org>")
              r69 (get-report db "<69@test.org>")]
          (assert= "Patch 68 type" :patch (:report/type r68))
          (assert= "Patch 68 seq" "1/2" (:report/patch-seq r68))
          (assert= "Patch 69 seq" "2/2" (:report/patch-seq r69))
          (assert-test "Patch 68 has series"
                       (some? (:report/series r68)))
          (assert-test "Patch 69 has series"
                       (some? (:report/series r69))))

        ;; --- Different sender same topic (email 70) ---
        (println "\n--- Email 70: different sender same topic ---")
        (let [r70 (get-report db "<70@test.org>")
              r68 (get-report db "<68@test.org>")]
          (assert= "Patch 70 type" :patch (:report/type r70))
          (assert-test "Different senders -> different series"
                       (not= (get-in r68 [:report/series :series/id])
                             (get-in r70 [:report/series :series/id]))))

        ;; --- POLL 75: vote format variants ---
        (println "\n--- POLL 75: vote format variants ---")
        (let [r (get-report db "<75@test.org>")]
          (assert= "Type is :request" :request (:report/type r))
          ;; voter1: "+1, great idea" -> up
          ;; voter2: "1+"             -> up
          (assert= "2 votes up (+1 with comma, 1+)"
                   2 (:report/votes-up r))
          ;; user: "1-"              -> down
          (assert= "1 vote down (1-)"
                   1 (:report/votes-down r))
          ;; newadmin: "+0"          -> null
          (assert= "1 null vote (+0)"
                   1 (:report/votes-null r))
          ;; admin: "+10 people agree" -> NOT a vote (digit follows)
          (assert= "4 voters (admin +10 not counted)"
                   4 (count (:report/voters r)))
          (assert-test "admin NOT in voters (+10 is not a vote)"
                       (not (contains? (set (:report/voters r))
                                       "admin@test.org"))))

        ;; =================================================================
        ;; DIRECTIVE TESTS (emails 81-89)
        ;; =================================================================

        ;; --- Unit tests: detect-directives ---
        (println "\n--- detect-directives unit tests ---")
        (assert= "Acked-by parsed"
                 [{:action :set :attr :report/acked :email-address "a@b.com"}]
                 (detect-directives :bug "Acked-by: a@b.com\n"))
        (assert= "Multiple directives parsed in order"
                 [{:action :set :attr :report/owned :email-address "x@y.com"}
                  {:action :set :attr :report/urgent :email-address "x@y.com"}]
                 (detect-directives :bug "Owned-by: x@y.com\nUrgent-by: x@y.com\n"))
        (assert= "Unacked parsed"
                 [{:action :unset :attr :report/acked}]
                 (detect-directives :bug "Unacked\n"))
        (assert= "Unurgent parsed"
                 [{:action :unset :attr :report/urgent}]
                 (detect-directives :bug "Unurgent\n"))
        (assert= "Unimportant parsed"
                 [{:action :unset :attr :report/important}]
                 (detect-directives :bug "Unimportant\n"))
        (assert= "Deadline parsed"
                 [{:action :set-deadline :date (parse-date-iso "2026-06-15")}]
                 (detect-directives :bug "Deadline: 2026-06-15\n"))
        (assert= "Undeadline parsed"
                 [{:action :unset-deadline}]
                 (detect-directives :bug "Undeadline\n"))
        (assert= "Topic parsed"
                 [{:action :set-topic :topic "my-topic"}]
                 (detect-directives :bug "Topic: my-topic\n"))
        (assert= "Mixed directives + deadline + topic in order"
                 [{:action :set :attr :report/acked :email-address "a@b.com"}
                  {:action :set-deadline :date (parse-date-iso "2026-07-01")}
                  {:action :set-topic :topic "urgent-fix"}]
                 (detect-directives :bug "Acked-by: a@b.com\nDeadline: 2026-07-01\nTopic: urgent-fix\n"))
        (assert= "Directive lines mixed with plain text"
                 [{:action :set :attr :report/owned :email-address "x@y.com"}]
                 (detect-directives :bug "Thanks for the report.\nOwned-by: x@y.com\nWill look into it.\n"))
        (assert= "No directives in plain text"
                 [] (detect-directives :bug "Just a normal reply.\n"))
        (assert= "nil body" nil (detect-directives :bug nil))

        ;; --- Unit tests: resolve-commands (last-one-wins) ---
        (println "\n--- resolve-commands unit tests ---")
        (assert= "Last-one-wins: set then unset"
                 {:set {} :unset #{:report/acked}}
                 (resolve-commands [{:action :set :attr :report/acked :email-address "a@b.com"}
                                      {:action :unset :attr :report/acked}]))
        (assert= "Last-one-wins: unset then set"
                 {:set {:report/acked "a@b.com"} :unset #{}}
                 (resolve-commands [{:action :unset :attr :report/acked}
                                      {:action :set :attr :report/acked :email-address "a@b.com"}]))
        (assert= "Deadline then undeadline"
                 {:set {} :unset #{} :undeadline? true}
                 (resolve-commands [{:action :set-deadline :date (parse-date-iso "2026-06-01")}
                                      {:action :unset-deadline}]))
        (assert= "Undeadline then deadline (deadline wins)"
                 {:set {} :unset #{} :deadline (parse-date-iso "2026-06-01")}
                 (resolve-commands [{:action :unset-deadline}
                                      {:action :set-deadline :date (parse-date-iso "2026-06-01")}]))
        (assert= "Topic last-one-wins"
                 {:set {} :unset #{} :topic "second"}
                 (resolve-commands [{:action :set-topic :topic "first"}
                                      {:action :set-topic :topic "second"}]))

        ;; --- Bug 81: Acked-by directive (email 82) ---
        (println "\n--- Bug 81: Acked-by directive ---")
        (let [r (get-report db "<81@test.org>")]
          ;; After email 85 (Unacked), acked should be nil
          (assert-test "Acked retracted by Unacked directive"
                       (nil? (:report/acked r)))
          (assert-test "Acked-proxy retracted too"
                       (nil? (:report/acked-proxy r)))
          ;; Owned-by and Urgent-by from email 83 should persist
          (assert-test "Owned is set" (some? (:report/owned r)))
          (assert-test "Urgent is set" (some? (:report/urgent r)))
          (assert= "Owned-proxy is maint@test.org"
                   "maint@test.org"
                   (get-in r [:report/owned-proxy :email/from-address]))
          (assert= "Urgent-proxy is maint@test.org"
                   "maint@test.org"
                   (get-in r [:report/urgent-proxy :email/from-address])))

        ;; --- Bug 81: regular user directive denied (email 84) ---
        (println "\n--- Bug 81: user directive denied ---")
        (let [r (get-report db "<81@test.org>")]
          (assert-test "NOT closed (user can't use Closed-by)"
                       (nil? (:report/closed r))))

        ;; --- Bug 86: last-one-wins in same email (email 87) ---
        (println "\n--- Bug 86: last-one-wins in same email ---")
        (let [r (get-report db "<86@test.org>")]
          (assert-test "NOT acked (Acked-by then Unacked -> unset wins)"
                       (nil? (:report/acked r))))

        ;; --- Bug 88: Closed-by + Important-by (email 89) ---
        (println "\n--- Bug 88: Closed-by + Important-by ---")
        (let [r (get-report db "<88@test.org>")]
          (assert-test "Closed is set" (some? (:report/closed r)))
          (assert= "Close-reason is :resolved (via Closed-by directive)" :resolved (:report/close-reason r))
          (assert-test "Important is set" (some? (:report/important r)))
          (assert= "Closed-proxy is admin@test.org"
                   "admin@test.org"
                   (get-in r [:report/closed-proxy :email/from-address]))
          (assert= "Important-proxy is admin@test.org"
                   "admin@test.org"
                   (get-in r [:report/important-proxy :email/from-address])))

        ;; =================================================================
        ;; MIXED TRIGGER + DIRECTIVE TESTS (emails 90-95)
        ;; =================================================================

        ;; --- Bug 90: trigger + directive in same email (email 91) ---
        (println "\n--- Bug 90: Confirmed trigger + Owned-by directive ---")
        (let [r (get-report db "<90@test.org>")]
          (assert-test "Acked via Confirmed trigger"
                       (some? (:report/acked r)))
          (assert-test "Owned via Owned-by directive"
                       (some? (:report/owned r)))
          (assert= "Owned-proxy is maint@test.org"
                   "maint@test.org"
                   (get-in r [:report/owned-proxy :email/from-address])))

        ;; --- Bug 90: Fixed trigger + Unclosed directive conflict (email 92) ---
        (println "\n--- Bug 90: Fixed trigger + Unclosed directive (directive wins) ---")
        (let [r (get-report db "<90@test.org>")]
          (assert-test "NOT closed (Unclosed directive overrides Fixed trigger)"
                       (nil? (:report/closed r))))

        ;; --- Bug 93: Confirmed trigger + Urgent-by (email 94) ---
        ;; Note: deadline from email 94 is overridden then removed by emails 96-97,
        ;; so we test deadline persistence separately on bug 98.
        (println "\n--- Bug 93: Confirmed + Urgent-by ---")
        (let [r (get-report db "<93@test.org>")]
          (assert-test "Acked via Confirmed trigger"
                       (some? (:report/acked r)))
          (assert-test "Urgent via Urgent-by directive"
                       (some? (:report/urgent r)))
          (assert= "Urgent-proxy is admin@test.org"
                   "admin@test.org"
                   (get-in r [:report/urgent-proxy :email/from-address])))

        ;; --- Bug 90: Confirmed trigger + Topic directive (email 95) ---
        (println "\n--- Bug 90: Confirmed trigger + Topic directive ---")
        (let [r (get-report db "<90@test.org>")]
          (assert= "Topic set to 'regression'"
                   "regression" (:report/topic r)))

        ;; --- Bug 93: Deadline override then Undeadline (emails 96-97) ---
        ;; Email 94 set deadline to 2026-06-01, email 96 overrides to 2026-09-01,
        ;; email 97 removes it with Undeadline.
        (println "\n--- Bug 93: Undeadline removes deadline ---")
        (let [r (get-report db "<93@test.org>")]
          (assert-test "Deadline removed by Undeadline"
                       (nil? (:report/deadline r))))

        ;; --- Bug 98: Standalone deadline (email 99, no undeadline) ---
        (println "\n--- Bug 98: Standalone deadline ---")
        (let [r (get-report db "<98@test.org>")]
          (assert-test "Deadline is set"
                       (some? (:report/deadline r))))

        ;; --- Email 100: [bark:<list-id>] subject prefix ---
        (println "\n--- Email 100: [bark:list.test.org] subject prefix ---")
        (let [r   (get-report db "<100@test.org>")
              eid (d/q '[:find ?e . :in $ ?mid
                         :where [?e :email/message-id ?mid]]
                       db "<100@test.org>")
              src (when eid (:email/source (d/pull db '[:email/source] eid)))]
          (assert-test "Report exists" (some? r))
          (assert= "Type is :bug (prefix stripped for detection)"
                   :bug (:report/type r))
          (assert= "Classified under public-list via bark prefix"
                   "public-list" src))

        ;; --- CHG 102, CHG 103, REL 104: release closes multiple changes ---
        (println "\n--- CHG 102+103 / REL 104: release closes and relates multiple changes ---")
        (let [chg1 (get-report db "<102@test.org>")
              chg2 (get-report db "<103@test.org>")
              rel  (get-report db "<104@test.org>")
              rel-related-mids (set (map :report/message-id (:report/related rel)))]
          (assert-test "CHG 102 auto-closed" (some? (:report/closed chg1)))
          (assert-test "CHG 103 auto-closed" (some? (:report/closed chg2)))
          (assert= "CHG 102 close-reason" :resolved (:report/close-reason chg1))
          (assert= "CHG 103 close-reason" :resolved (:report/close-reason chg2))
          ;; REL is related to both CHGs
          (assert-test "REL related to CHG 102"
                       (contains? rel-related-mids "<102@test.org>"))
          (assert-test "REL related to CHG 103"
                       (contains? rel-related-mids "<103@test.org>"))
          ;; Each CHG is related back to REL
          (assert-test "CHG 102 related to REL"
                       (some #(= "<104@test.org>" (:report/message-id %))
                             (:report/related chg1)))
          (assert-test "CHG 103 related to REL"
                       (some #(= "<104@test.org>" (:report/message-id %))
                             (:report/related chg2))))

        ;; --- Summary ---
        (println "\n=== Summary ===")
        (let [{:keys [pass fail]} @*test-counts*]
          (println (str pass " passed, " fail " failed"))
          (when (pos? fail) (System/exit 1))))

      (finally
        (teardown! ctx)))))

(run-tests)
