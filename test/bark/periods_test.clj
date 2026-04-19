(ns bark.periods-test
  (:require [clojure.test :refer [deftest is testing]]
            [bark.common :as common]
            [bark.periods :as p]))

(defn- d [s] (common/parse-iso-date s))

;; ---------------------------------------------------------------------------
;; source-periods
;; ---------------------------------------------------------------------------

(deftest source-periods-no-period-returns-single-all-time
  (let [src {:name "s" :maintainers ["a@x.org"] :commands {:closed {:words ["Done"]}}}
        [only] (p/source-periods src)]
    (is (nil? (:from only)))
    (is (nil? (:to only)))
    (is (= ["a@x.org"] (:maintainers only)))
    (is (= {:closed {:words ["Done"]}} (:commands only)))))

(deftest source-periods-inherits-from-source-defaults
  (let [src {:maintainers ["default@x.org"]
             :commands    {:closed {:words ["Closed"]}}
             :periods     [{:end "2020-01-01"
                            :maintainers ["old@x.org"]}
                           {:start "2020-01-01"}]}
        [era1 era2] (p/source-periods src)]
    (testing "era1 overrides :maintainers, inherits :commands"
      (is (= ["old@x.org"] (:maintainers era1)))
      (is (= {:closed {:words ["Closed"]}} (:commands era1))))
    (testing "era2 inherits both (neither declared)"
      (is (= ["default@x.org"] (:maintainers era2)))
      (is (= {:closed {:words ["Closed"]}} (:commands era2))))))

(deftest source-periods-parses-dates
  (let [[era] (p/source-periods {:periods [{:start "2020-01-01" :end "2024-01-01"}]})]
    (is (= (d "2020-01-01") (:from era)))
    (is (= (d "2024-01-01") (:to era)))))

;; ---------------------------------------------------------------------------
;; period-at-date
;; ---------------------------------------------------------------------------

(deftest period-at-date-half-open-window
  (let [periods [{:from nil :to (d "2020-01-01") :tag :a}
                 {:from (d "2020-01-01") :to (d "2024-01-01") :tag :b}
                 {:from (d "2024-01-01") :to nil :tag :c}]]
    (testing "unbounded past catches very old dates"
      (is (= :a (:tag (p/period-at-date periods (d "1999-05-05"))))))
    (testing ":from is inclusive"
      (is (= :b (:tag (p/period-at-date periods (d "2020-01-01"))))))
    (testing ":to is exclusive"
      (is (= :b (:tag (p/period-at-date periods (d "2023-12-31"))))))
    (testing "boundary date goes to the next period"
      (is (= :c (:tag (p/period-at-date periods (d "2024-01-01"))))))
    (testing "unbounded future catches recent dates"
      (is (= :c (:tag (p/period-at-date periods (d "2030-06-01"))))))))

(deftest period-at-date-nil-date-returns-nil
  (is (nil? (p/period-at-date [{:from nil :to nil}] nil))))

(deftest period-at-date-returns-nil-when-gap
  (let [periods [{:from (d "2020-01-01") :to (d "2022-01-01")}
                 {:from (d "2023-01-01") :to nil}]]
    (is (nil? (p/period-at-date periods (d "2022-06-01"))))))

;; ---------------------------------------------------------------------------
;; source-cfg-at-date
;; ---------------------------------------------------------------------------

(deftest source-cfg-at-date-applies-period-override
  (let [src {:name        "s"
             :list        "x@list"
             :maintainers ["current@x.org"]
             :commands    {:closed {:words ["Closed"]}}
             :periods     [{:end "2020-01-01"
                            :maintainers ["old@x.org"]
                            :commands    {:closed {:words ["Done"]}}}
                           {:start "2020-01-01"}]}]
    (testing "pre-2020: era-1 overrides"
      (let [cfg (p/source-cfg-at-date src (d "2019-06-01"))]
        (is (= ["old@x.org"] (:maintainers cfg)))
        (is (= {:closed {:words ["Done"]}} (:commands cfg)))
        (testing "non-overridable fields preserved"
          (is (= "x@list" (:list cfg))))))
    (testing "post-2020: era-2 inherits from source"
      (let [cfg (p/source-cfg-at-date src (d "2025-01-01"))]
        (is (= ["current@x.org"] (:maintainers cfg)))
        (is (= {:closed {:words ["Closed"]}} (:commands cfg)))))))

(deftest source-cfg-at-date-no-period-is-identity-on-overridables
  (let [src {:name "s" :maintainers ["a@x.org"] :commands {}}]
    (is (= ["a@x.org"]
           (:maintainers (p/source-cfg-at-date src (d "2024-01-01")))))))

(deftest source-cfg-at-date-falls-back-to-last-period-on-gap
  (let [src {:periods [{:end "2020-01-01" :maintainers ["a@x"]}
                      {:start "2023-01-01" :maintainers ["b@x"]}]}]
    ;; date in the gap → defensive fallback to the last period
    (is (= ["b@x"]
           (:maintainers (p/source-cfg-at-date src (d "2022-01-01")))))))

;; ---------------------------------------------------------------------------
;; validate-periods
;; ---------------------------------------------------------------------------

(deftest validate-periods-no-period-is-ok
  (is (= [] (p/validate-periods {:name "s" :maintainers ["a@x"]}))))

(deftest validate-periods-well-formed-contiguous
  (is (= [] (p/validate-periods
             {:periods [{:end "2020-01-01" :maintainers ["a@x"]}
                       {:start "2020-01-01" :end "2024-01-01" :maintainers ["b@x"]}
                       {:start "2024-01-01" :maintainers ["c@x"]}]}))))

(deftest validate-periods-rejects-non-iso-date
  (let [errs (p/validate-periods
              {:periods [{:start "01/01/2020"}]})]
    (is (some #(re-find #":start not ISO" %) errs))))

(deftest validate-periods-rejects-start-not-before-end
  (let [errs (p/validate-periods
              {:periods [{:start "2022-01-01" :end "2020-01-01"}]})]
    (is (some #(re-find #":start must be strictly before :end" %) errs))))

(deftest validate-periods-detects-gap
  (let [errs (p/validate-periods
              {:periods [{:end "2020-01-01"}
                        {:start "2021-01-01"}]})]
    (is (some #(re-find #"gap/overlap" %) errs))))

(deftest validate-periods-detects-overlap
  (let [errs (p/validate-periods
              {:periods [{:end "2021-01-01"}
                        {:start "2020-06-01"}]})]
    (is (some #(re-find #"gap/overlap" %) errs))))

(deftest validate-periods-detects-missing-intermediate-bounds
  (testing "inner period missing :end"
    (let [errs (p/validate-periods
                {:periods [{} {:start "2020-01-01"}]})]
      (is (some #(re-find #"missing :end" %) errs))))
  (testing "inner period missing :start"
    (let [errs (p/validate-periods
                {:periods [{:end "2020-01-01"} {}]})]
      (is (some #(re-find #"missing :start" %) errs)))))

(deftest validate-periods-rejects-non-map-entry
  (let [errs (p/validate-periods {:periods ["oops"]})]
    (is (some #(re-find #"not a map" %) errs))))
