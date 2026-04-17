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
  (let [sc     (src-cmds strict?)
        period (commands/select-period sc nil)]
    (commands/detect-directives :bug body nil nil nil (:directives period))))

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

;; ---------------------------------------------------------------------------
;; Time-windowed :command-syntax
;; ---------------------------------------------------------------------------

(def ^:private timelined-syntax-src
  ;; :loose until 2026-01-01, :strict from then on.
  (commands/build-source-commands
   {:command-syntax [[:loose  {:until "2026-01-01"}]
                     [:strict {:since "2026-01-01"}]]}))

(deftest syntax-timeline-loose-period-accepts-bare
  (let [r (commands/detect-triggers :bug "Closed.\n" timelined-syntax-src
                                    (parse-date "2025-06-15"))]
    (is (some? (:report/closed r)))))

(deftest syntax-timeline-strict-period-rejects-bare
  (let [r (commands/detect-triggers :bug "Closed.\n" timelined-syntax-src
                                    (parse-date "2026-06-15"))]
    (is (nil? (:report/closed r)))))

(deftest syntax-timeline-strict-period-accepts-bang
  (let [r (commands/detect-triggers :bug "!Closed.\n" timelined-syntax-src
                                    (parse-date "2026-06-15"))]
    (is (some? (:report/closed r)))))

(deftest syntax-timeline-directives-per-period
  (testing "directive bare form accepted in loose period"
    (let [period (commands/select-period timelined-syntax-src (parse-date "2025-06-15"))]
      (is (seq (commands/detect-directives :bug "Acked-by: a@b.com\n" nil nil nil
                                           (:directives period))))))
  (testing "directive bare form rejected in strict period"
    (let [period (commands/select-period timelined-syntax-src (parse-date "2026-06-15"))]
      (is (empty? (commands/detect-directives :bug "Acked-by: a@b.com\n" nil nil nil
                                              (:directives period)))))))

(deftest syntax-timeline-word-windows-compose
  ;; Combine a word window (Done active 2020→2026) with a syntax flip
  ;; (loose until 2026-01-01, strict after). Resulting periods:
  ;;   (-, 2020-01-01)    loose, no Done
  ;;   [2020-01-01, 2026) loose, Done active  → bare "Done." matches
  ;;   [2026, ∞)          strict, no Done
  (let [sc (commands/build-source-commands
            {:command-syntax [[:loose  {:until "2026-01-01"}]
                              [:strict {:since "2026-01-01"}]]
             :commands {:closed {:words ["Fixed"
                                         ["Done" {:since "2020-01-01"
                                                  :until "2026-01-01"}]]}}})]
    (testing "Done matches bare inside shared loose+window period"
      (is (some? (:report/closed (commands/detect-triggers :bug "Done.\n" sc
                                                           (parse-date "2023-06-15"))))))
    (testing "Done gone after strict flip"
      (is (nil? (:report/closed (commands/detect-triggers :bug "Done.\n" sc
                                                          (parse-date "2026-06-15")))))
      (is (nil? (:report/closed (commands/detect-triggers :bug "!Done.\n" sc
                                                          (parse-date "2026-06-15"))))))
    (testing "Fixed still matches bare in early loose period"
      (is (some? (:report/closed (commands/detect-triggers :bug "Fixed.\n" sc
                                                           (parse-date "2010-06-15"))))))
    (testing "Fixed requires bang in strict period"
      (is (nil? (:report/closed (commands/detect-triggers :bug "Fixed.\n" sc
                                                          (parse-date "2026-06-15")))))
      (is (some? (:report/closed (commands/detect-triggers :bug "!Fixed.\n" sc
                                                           (parse-date "2026-06-15"))))))))

(deftest syntax-scalar-form-still-works
  (testing "scalar :strict produces a single strict period"
    (let [sc (commands/build-source-commands {:command-syntax :strict})]
      (is (= 1 (count (:timeline sc))))
      (is (true? (:strict-syntax? (first (:timeline sc)))))))
  (testing "scalar :loose produces a single loose period"
    (let [sc (commands/build-source-commands {:command-syntax :loose})]
      (is (= 1 (count (:timeline sc))))
      (is (false? (:strict-syntax? (first (:timeline sc)))))))
  (testing "no :command-syntax defaults to loose"
    (let [sc (commands/build-source-commands {})]
      (is (false? (:strict-syntax? (first (:timeline sc))))))))

;; ---------------------------------------------------------------------------
;; Case-insensitive address caches
;; ---------------------------------------------------------------------------

(deftest trigger-tx-lowercases-address-cache
  (testing "build-trigger-tx stores :report/*-address lowercased regardless of
            the sender's from-address casing — so downstream :setter-or-maintainer
            comparisons are stable even if the user's MUA rewrites address case"
    (let [trig-result {:report/acked 999}
          [tx _ _] (commands/build-trigger-tx 42 trig-result 999 "Alice@Example.COM" {})]
      (is (some (fn [datom] (= datom [:db/add 42 :report/acked-address "alice@example.com"]))
                tx)))))
