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

;; ---------------------------------------------------------------------------
;; Time-windowed trigger words
;; ---------------------------------------------------------------------------

(defn- parse-date [s]
  (.parse (java.text.SimpleDateFormat. "yyyy-MM-dd") s))

(def ^:private windowed-src
  ;; :closed accepts "Done" only in the 2020–2026 window; "Fixed" always.
  (commands/build-source-commands
   {:commands {:closed {:words ["Fixed"
                                ["Done" {:since "2020-01-01"
                                         :until "2026-01-01"}]]}}}))

(deftest windowed-word-matches-inside-window
  (let [r (commands/detect-triggers :bug "Done.\n" windowed-src
                                    (parse-date "2023-06-15"))]
    (is (= true (:report/closed r)))
    (is (= :resolved (:report/close-reason r)))))

(deftest windowed-word-rejected-before-window
  (let [r (commands/detect-triggers :bug "Done.\n" windowed-src
                                    (parse-date "2018-06-15"))]
    (is (nil? (:report/closed r)))))

(deftest windowed-word-rejected-after-window
  (let [r (commands/detect-triggers :bug "Done.\n" windowed-src
                                    (parse-date "2027-06-15"))]
    (is (nil? (:report/closed r)))))

(deftest always-active-word-matches-outside-window
  (testing "Fixed stays active before, inside and after the Done window"
    (doseq [d ["2018-06-15" "2023-06-15" "2027-06-15"]]
      (is (some? (:report/closed (commands/detect-triggers
                                  :bug "Fixed.\n" windowed-src (parse-date d))))
          d))))

(deftest windowed-word-uses-first-period-when-email-date-nil
  ;; Email-date nil should not crash; falls back to the first period
  ;; (which excludes Done in this fixture).
  (let [r (commands/detect-triggers :bug "Done.\n" windowed-src nil)]
    (is (nil? (:report/closed r)))))

(deftest windowed-word-honors-strict-mode
  (let [sc (commands/build-source-commands
            {:command-syntax :strict
             :commands {:closed {:words ["Fixed"
                                         ["Done" {:since "2020-01-01"}]]}}})]
    (testing "strict requires ! even in active window"
      (is (nil? (:report/closed (commands/detect-triggers :bug "Done.\n" sc
                                                          (parse-date "2023-06-15"))))))
    (testing "!Done. matches in active window"
      (is (some? (:report/closed (commands/detect-triggers :bug "!Done.\n" sc
                                                           (parse-date "2023-06-15"))))))
    (testing "!Done. rejected outside window even with !"
      (is (nil? (:report/closed (commands/detect-triggers :bug "!Done.\n" sc
                                                          (parse-date "2019-06-15"))))))))
