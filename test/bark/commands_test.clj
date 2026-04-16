(ns bark.commands-test
  "Unit tests for the loose/strict :command-syntax modes.

  The matrix being verified: every Bark instruction (trigger, negative
  trigger, -by directive, Date/Topic/Expiry/Deadline directive,
  Superseded-by, role directive, Notify) must be:

    - accepted with and without `!` prefix in :loose mode
    - accepted only with `!` prefix in :strict mode"
  (:require [clojure.test :refer [deftest is testing]]
            [bark.commands :as commands]
            [bark.roles :as roles]))

(defn- src-cmds [strict?]
  (commands/build-source-commands
   {:command-syntax (if strict? :strict :loose)}))

;; ---------------------------------------------------------------------------
;; Triggers (simple + negative)
;; ---------------------------------------------------------------------------

(deftest trigger-loose-accepts-both-forms
  (let [sc (src-cmds false)]
    (testing "bare form"
      (is (some? (commands/detect-triggers :bug "Closed.\n" sc))))
    (testing "!-prefixed form"
      (is (some? (commands/detect-triggers :bug "!Closed.\n" sc))))))

(deftest trigger-strict-rejects-bare-form
  (let [sc (src-cmds true)]
    (testing "bare form rejected"
      (is (nil? (commands/detect-triggers :bug "Closed.\n" sc))))
    (testing "!-prefixed form accepted"
      (is (some? (commands/detect-triggers :bug "!Closed.\n" sc))))))

;; ---------------------------------------------------------------------------
;; Directives (-by, Deadline, Topic, Superseded-by) and negatives
;; ---------------------------------------------------------------------------

(defn- detect-with [strict? body]
  (let [sc (src-cmds strict?)]
    (commands/detect-directives :bug body nil nil nil (:directives sc))))

(deftest directive-loose-accepts-both-forms
  (testing "-by directive"
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
    (is (seq (detect-with false "!Superseded-by: <msg@example.com>\n")))))

(deftest directive-strict-rejects-bare-form
  (testing "-by directive"
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
    (is (seq    (detect-with true "!Superseded-by: <msg@example.com>\n")))))

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
;; Notify: pattern via the pattern generator
;; ---------------------------------------------------------------------------

(deftest notify-pattern-mode-aware
  (let [loose  (roles/role-control-pattern false)
        strict (roles/role-control-pattern true)]
    (is (some? (re-find loose "Add maintainer: x@y.z")))
    (is (some? (re-find loose "!Add maintainer: x@y.z")))
    (is (nil?  (re-find strict "Add maintainer: x@y.z")))
    (is (some? (re-find strict "!Add maintainer: x@y.z")))))
