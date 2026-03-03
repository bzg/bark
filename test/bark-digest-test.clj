#!/usr/bin/env bb

;; test/bark-digest-test.clj — Integration tests for bark-digest.
;;
;; Creates a temporary datalevin DB, inserts test emails, runs cmd-digest!,
;; and verifies reports, triggers, threading, votes, roles, and permissions.
;;
;; Usage:
;;   bb test/bark-digest-test.clj
;;
;; Requires: datalevin pod 0.10.5, resources/bark-schema.edn.

(require '[babashka.pods :as pods]
         '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io])

(pods/load-pod 'huahaiy/datalevin "0.10.5")

(require '[pod.huahaiy.datalevin :as d])

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

;; We load the schema and use the same functions bark-digest uses.
;; To avoid running its main block, we load only the function defs.
(def schema (edn/read-string (slurp "resources/bark-schema.edn")))

;; Load bark-digest.clj — the main block is guarded by
;; (= (System/getProperty "babashka.file") *file*) so it won't run here.
(load-file "scripts/bark-digest.clj")

;; ---------------------------------------------------------------------------
;; Test setup
;; ---------------------------------------------------------------------------

(defn setup-db!
  "Create a temp datalevin DB, insert roles and test emails."
  []
  (let [db-path (str "/tmp/bark-test-" (System/currentTimeMillis))
        conn    (d/get-conn db-path schema)]
    ;; Setup roles for "direct" source
    (d/transact! conn [{:roles/source "direct"
                        :roles/admin  "admin@test.org"}])
    ;; Setup roles for "public-list" source (list-backed)
    (d/transact! conn [{:roles/source "public-list"
                        :roles/admin  "admin@test.org"}])
    ;; Insert test emails
    (let [emails (edn/read-string (slurp "test/emails.edn"))]
      (doseq [email emails]
        (d/transact! conn [email])))
    {:conn conn :db-path db-path}))

(defn teardown! [{:keys [conn db-path]}]
  (d/close conn)
  ;; Clean up temp dir
  (let [dir (io/file db-path)]
    (when (.exists dir)
      (doseq [f (reverse (file-seq dir))]
        (.delete f)))))

(defn get-report [db message-id]
  (d/pull db
          '[:report/type :report/version :report/topic
            :report/patch-seq :report/patch-source :report/message-id
            :report/acked :report/owned :report/closed
            :report/urgent :report/important
            :report/votes-up :report/votes-down :report/voters
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
                                :mailing-list-email "list@test.org"}})

(def sources [{:name "direct"}
              {:name "public-list"
               :mailing-list-email "list@test.org"}])

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

        ;; --- Role commands (email 01) ---
        (println "\n--- Roles ---")
        (let [roles (d/pull db '[:roles/admin :roles/maintainers :roles/ignored]
                            [:roles/source "direct"])]
          (assert= "Admin is admin@test.org"
                   "admin@test.org" (:roles/admin roles))
          (assert-test "maint@test.org is maintainer"
                       (contains? (set (:roles/maintainers roles)) "maint@test.org"))
          (assert-test "spam@test.org is ignored"
                       (contains? (set (:roles/ignored roles)) "spam@test.org")))

        ;; --- Bug 02: full lifecycle ---
        (println "\n--- Bug 02: [BUG 9.7] lifecycle ---")
        (let [r (get-report db "<02@test.org>")]
          (assert= "Type is :bug" :bug (:report/type r))
          (assert= "Version is 9.7" "9.7" (:report/version r))
          (assert-test "Acked (Confirmed)" (some? (:report/acked r)))
          (assert-test "Owned (Handled)" (some? (:report/owned r)))
          (assert-test "Closed (Fixed)" (some? (:report/closed r)))
          (assert-test "Urgent unset (Not urgent)" (nil? (:report/urgent r)))
          (assert= "3 descendants" 3
                   (count (:report/descendants r))))

        ;; --- Bug 03: mailing list prefix ---
        (println "\n--- Bug 03: mailing list prefix ---")
        (let [r (get-report db "<03@test.org>")]
          (assert= "Type is :bug" :bug (:report/type r))
          (assert= "No version" nil (:report/version r)))

        ;; --- Patch 07: subject detection ---
        (println "\n--- Patch 07: [PATCH subject] ---")
        (let [r (get-report db "<07@test.org>")]
          (assert= "Type is :patch" :patch (:report/type r))
          (assert= "Topic" "org-agenda" (:report/topic r))
          (assert= "Seq" "1/2" (:report/patch-seq r))
          (assert-test ":subject in patch-source"
                       (contains? (set (:report/patch-source r)) :subject))
          (assert-test "Acked (Reviewed)" (some? (:report/acked r)))
          (assert-test "Closed (Applied)" (some? (:report/closed r))))

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

        ;; --- RFC 15: closed ---
        (println "\n--- RFC 15: request lifecycle ---")
        (let [r (get-report db "<15@test.org>")]
          (assert= "Type is :request" :request (:report/type r))
          (assert-test "Closed (Done)" (some? (:report/closed r))))

        ;; --- ANN 17: canceled ---
        (println "\n--- ANN 17: announcement canceled ---")
        (let [r (get-report db "<17@test.org>")]
          (assert= "Type is :announcement" :announcement (:report/type r))
          (assert-test "Closed (Canceled)" (some? (:report/closed r))))

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
          (assert= "REL type" :release (:report/type rel))
          (assert= "REL version" "9.8" (:report/version rel)))

        ;; --- Email 21: ignored ---
        (println "\n--- Email 21: ignored user ---")
        (assert-test "No report from ignored user"
                     (not (report-exists? db "<21@test.org>")))

        ;; --- Bug 23: important flag set then unset ---
        (println "\n--- Bug 23: important set then unset ---")
        (let [r (get-report db "<23@test.org>")]
          (assert= "Type is :bug" :bug (:report/type r))
          (assert-test "Important unset" (nil? (:report/important r)))
          (assert= "3 descendants" 3
                   (count (:report/descendants r))))

        ;; --- TASK 26: synonym for request ---
        (println "\n--- TASK 26: request synonym ---")
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

        ;; Check individual patches link back to series
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

        ;; v2 patch 1/3 should point to v2 series
        (let [r39 (get-report db "<39@test.org>")
              s   (:report/series r39)]
          (assert-test "v2 patch has series ref" (some? s))
          (assert= "v2 patch seq" "1/3" (:report/patch-seq r39)))

        ;; --- Email 40: patch related to bug 23 ---
        (println "\n--- Email 40: patch related to bug ---")
        (let [patch (get-report db "<40@test.org>")
              bug   (get-report db "<23@test.org>")]
          (assert= "Patch type" :patch (:report/type patch))
          ;; Patch should be related to the bug
          (assert-test "Patch is related to bug"
                       (some #(= "<23@test.org>" (:report/message-id %))
                             (:report/related patch)))
          ;; Bug should be related to the patch (bi-directional)
          (assert-test "Bug is related to patch"
                       (some #(= "<40@test.org>" (:report/message-id %))
                             (:report/related bug))))

        ;; --- Summary ---
        (println "\n=== Summary ===")
        (let [{:keys [pass fail]} @*test-counts*]
          (println (str pass " passed, " fail " failed"))
          (when (pos? fail) (System/exit 1))))

      (finally
        (teardown! ctx)))))

(run-tests)
