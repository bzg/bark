(ns bone.commands-test
  "Unit tests for the loose/strict :command-syntax modes.

  The matrix being verified: every Bone command (bareword, negative
  bareword, -by line, Date/Topic/Expiry/Deadline line, Superseded-by,
  role command) must be:

    - accepted with and without `!` prefix in :loose mode
    - accepted only with `!` prefix in :strict mode"
  (:require [clojure.test :refer [deftest is testing]]
            [bone.commands :as commands]
            [bone.roles :as roles]))

(defn- src-cmds [strict?]
  (commands/build-source-commands
   {:command-syntax (if strict? :strict :loose)}))

;; ---------------------------------------------------------------------------
;; Barewords (simple + negative)
;; ---------------------------------------------------------------------------

(deftest word-loose-accepts-both-forms
  (let [sc (src-cmds false)]
    (testing "bare form"
      (is (some? (commands/detect-words :bug "Closed.\n" sc))))
    (testing "!-prefixed form"
      (is (some? (commands/detect-words :bug "!Closed.\n" sc))))))

(deftest word-strict-rejects-bare-form
  (let [sc (src-cmds true)]
    (testing "bare form rejected"
      (is (nil? (commands/detect-words :bug "Closed.\n" sc))))
    (testing "!-prefixed form accepted"
      (is (some? (commands/detect-words :bug "!Closed.\n" sc))))))

;; ---------------------------------------------------------------------------
;; Colon-line commands (-by, Deadline, Topic, Superseded-by) and negatives
;; ---------------------------------------------------------------------------

(defn- detect-with [strict? body]
  (let [sc (src-cmds strict?)]
    (commands/detect-lines :bug body nil nil (:line-patterns sc))))

(deftest line-loose-accepts-both-forms
  (testing "-by line"
    (is (seq (detect-with false "Acked-by: a@b.com\n")))
    (is (seq (detect-with false "!Acked-by: a@b.com\n"))))
  (testing "Deadline"
    (is (seq (detect-with false "Deadline: 2026-06-15\n")))
    (is (seq (detect-with false "!Deadline: 2026-06-15\n"))))
  (testing "Negative unset (Not X)"
    (is (seq (detect-with false "Not acked\n")))
    (is (seq (detect-with false "!Not acked\n"))))
  (testing "No X form"
    (is (seq (detect-with false "No deadline\n")))
    (is (seq (detect-with false "!No deadline\n"))))
  (testing "Superseded-by"
    (is (seq (detect-with false "Superseded-by: <msg@example.com>\n")))
    (is (seq (detect-with false "!Superseded-by: <msg@example.com>\n"))))
  (testing "Supersedes"
    (is (seq (detect-with false "Supersedes: <msg@example.com>\n")))
    (is (seq (detect-with false "!Supersedes: <msg@example.com>\n"))))
  (testing "Not supersedes"
    (is (seq (detect-with false "Not supersedes: <msg@example.com>\n")))
    (is (seq (detect-with false "!Not supersedes: <msg@example.com>\n")))))

(deftest line-strict-rejects-bare-form
  (testing "-by line"
    (is (empty? (detect-with true "Acked-by: a@b.com\n")))
    (is (seq    (detect-with true "!Acked-by: a@b.com\n"))))
  (testing "Deadline"
    (is (empty? (detect-with true "Deadline: 2026-06-15\n")))
    (is (seq    (detect-with true "!Deadline: 2026-06-15\n"))))
  (testing "Not acked"
    (is (empty? (detect-with true "Not acked\n")))
    (is (seq    (detect-with true "!Not acked\n"))))
  (testing "Superseded-by"
    (is (empty? (detect-with true "Superseded-by: <msg@example.com>\n")))
    (is (seq    (detect-with true "!Superseded-by: <msg@example.com>\n"))))
  (testing "Supersedes"
    (is (empty? (detect-with true "Supersedes: <msg@example.com>\n")))
    (is (seq    (detect-with true "!Supersedes: <msg@example.com>\n"))))
  (testing "Not supersedes"
    (is (empty? (detect-with true "Not supersedes: <msg@example.com>\n")))
    (is (seq    (detect-with true "!Not supersedes: <msg@example.com>\n")))))

;; ---------------------------------------------------------------------------
;; Role directives (Add/Remove maintainer) via pure parse-role-controls
;; ---------------------------------------------------------------------------

(deftest role-loose-accepts-both-forms
  (is (= 1 (count (roles/parse-role-controls "Add maintainer: a@b.com" false))))
  (is (= 1 (count (roles/parse-role-controls "!Add maintainer: a@b.com" false)))))

(deftest role-strict-rejects-bare-form
  (is (= 0 (count (roles/parse-role-controls "Add maintainer: a@b.com" true))))
  (is (= 1 (count (roles/parse-role-controls "!Add maintainer: a@b.com" true)))))

;; ---------------------------------------------------------------------------
;; Role control pattern -- strict mode requires the bang prefix
;; ---------------------------------------------------------------------------

(deftest role-control-pattern-mode-aware
  (let [loose  (roles/role-control-pattern false)
        strict (roles/role-control-pattern true)]
    (is (some? (re-find loose "Add maintainer: x@y.z")))
    (is (some? (re-find loose "!Add maintainer: x@y.z")))
    (is (nil?  (re-find strict "Add maintainer: x@y.z")))
    (is (some? (re-find strict "!Add maintainer: x@y.z")))))

;; ---------------------------------------------------------------------------
;; build-source-commands shape
;; ---------------------------------------------------------------------------

(deftest build-source-commands-shape
  (testing "scalar :strict produces strict-syntax? true"
    (let [sc (commands/build-source-commands {:command-syntax :strict})]
      (is (true? (:strict-syntax? sc)))
      (is (some? (:word-patterns sc)))
      (is (some? (:line-patterns sc)))))
  (testing "scalar :loose produces strict-syntax? false"
    (let [sc (commands/build-source-commands {:command-syntax :loose})]
      (is (false? (:strict-syntax? sc)))))
  (testing "no :command-syntax defaults to loose"
    (let [sc (commands/build-source-commands {})]
      (is (false? (:strict-syntax? sc))))))

;; ---------------------------------------------------------------------------
;; Case-insensitive address caches
;; ---------------------------------------------------------------------------

(deftest word-tx-lowercases-address-cache
  (testing "build-word-tx stores :report/*-address lowercased regardless of
            the sender's from-address casing -- so downstream :setter-or-maintainer
            comparisons are stable even if the user's MUA rewrites address case"
    (let [word-result {:report/acked 999}
          [tx _ _] (commands/build-word-tx 42 word-result 999 "Alice@Example.COM" {})]
      (is (some (fn [datom] (= datom [:db/add 42 :report/acked-address "alice@example.com"]))
                tx)))))
