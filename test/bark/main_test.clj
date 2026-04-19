(ns bark.main-test
  (:require [clojure.test :refer [deftest is testing]]
            [bark.main :as main])
  (:import [java.util Date]))

(def parse-fetch (var-get #'main/parse-fetch))
(def cli-fetch->map (var-get #'main/cli-fetch->map))

(deftest parse-fetch-limit
  (testing "{:limit N} with positive integer"
    (is (= {:limit 50} (parse-fetch {:limit 50})))
    (is (= {:limit 1}  (parse-fetch {:limit 1}))))
  (testing ":limit rejects zero and negatives"
    (is (thrown? Exception (parse-fetch {:limit 0})))
    (is (thrown? Exception (parse-fetch {:limit -5}))))
  (testing ":limit rejects non-integer"
    (is (thrown? Exception (parse-fetch {:limit "50"}))))
  (testing ":limit cannot be combined with other keys"
    (is (thrown? Exception (parse-fetch {:limit 50 :since "30d"})))
    (is (thrown? Exception (parse-fetch {:limit 50 :start "2020-01-01"})))))

(deftest parse-fetch-since
  (testing "{:since \"Nd\"} returns {:since Date}"
    (let [{:keys [since]} (parse-fetch {:since "30d"})]
      (is (instance? Date since))))
  (testing ":since accepts all duration units"
    (doseq [u ["5d" "3w" "2m" "1y"]]
      (is (some? (:since (parse-fetch {:since u}))) u)))
  (testing ":since rejects ISO date (must be duration)"
    (is (thrown? Exception (parse-fetch {:since "2020-01-01"}))))
  (testing ":since rejects bare numbers"
    (is (thrown? Exception (parse-fetch {:since "50"})))
    (is (thrown? Exception (parse-fetch {:since 50}))))
  (testing ":since cannot combine with other keys"
    (is (thrown? Exception (parse-fetch {:since "30d" :end "2020-01-01"})))))

(deftest parse-fetch-start-end
  (testing "{:start ISO} translates to mailseq :since"
    (let [{:keys [since before]} (parse-fetch {:start "2020-01-01"})]
      (is (instance? Date since))
      (is (nil? before))))
  (testing "{:end ISO} translates to mailseq :before (alone allowed)"
    (let [{:keys [since before]} (parse-fetch {:end "2022-01-01"})]
      (is (nil? since))
      (is (instance? Date before))))
  (testing "{:start :end} translates to mailseq {:since :before}"
    (let [{:keys [since before]} (parse-fetch {:start "2020-01-01" :end "2022-01-01"})]
      (is (instance? Date since))
      (is (instance? Date before))
      (is (.before since before))))
  (testing ":start/:end reject duration strings"
    (is (thrown? Exception (parse-fetch {:start "30d"})))
    (is (thrown? Exception (parse-fetch {:end "30d"}))))
  (testing ":start must be strictly before :end"
    (is (thrown? Exception (parse-fetch {:start "2022-01-01" :end "2020-01-01"})))
    (is (thrown? Exception (parse-fetch {:start "2020-01-01" :end "2020-01-01"}))))
  (testing "extraneous keys rejected"
    (is (thrown? Exception (parse-fetch {:start "2020-01-01" :foo 1})))))

(deftest parse-fetch-rejects
  (testing "non-map values rejected"
    (is (thrown? Exception (parse-fetch 50)))
    (is (thrown? Exception (parse-fetch "30d")))
    (is (thrown? Exception (parse-fetch "2020-01-01")))
    (is (thrown? Exception (parse-fetch nil)))
    (is (thrown? Exception (parse-fetch [:limit 50]))))
  (testing "empty map rejected"
    (is (thrown? Exception (parse-fetch {})))))

(deftest cli-fetch->map-lifts
  (testing "bare integer → :limit"
    (is (= {:limit 50} (cli-fetch->map "50"))))
  (testing "duration → :since"
    (is (= {:since "30d"} (cli-fetch->map "30d")))
    (is (= {:since "6w"}  (cli-fetch->map "6w"))))
  (testing "ISO date → :start"
    (is (= {:start "2020-01-01"} (cli-fetch->map "2020-01-01"))))
  (testing "invalid rejected"
    (is (thrown? Exception (cli-fetch->map "foo")))
    (is (thrown? Exception (cli-fetch->map "30 days")))))
