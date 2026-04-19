(ns bark.history-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [bark.history :as h]))

(deftest iso-date?-recognises-yyyy-mm-dd
  (is (h/iso-date? "2020-01-01"))
  (is (h/iso-date? "1999-12-31"))
  (is (not (h/iso-date? "2020-1-1")))
  (is (not (h/iso-date? "2020-01-01T00:00:00")))
  (is (not (h/iso-date? "30d")))
  (is (not (h/iso-date? nil)))
  (is (not (h/iso-date? ""))))

(defn- with-tmp-config
  "Call `f` with the absolute path of a temp file that exists on disk,
  then delete it.  Used by validate-entry tests that assert the
  existence check fires."
  [f]
  (let [tmp (java.io.File/createTempFile "bark-test-" ".edn")]
    (try
      (spit tmp "{}")
      (f (.getAbsolutePath tmp))
      (finally (.delete tmp)))))

(defn- with-tmp-cfgs
  "Call `f` with a vector of temp-file paths, each containing the
  corresponding EDN content string from `contents`."
  [contents f]
  (let [files (mapv (fn [_] (java.io.File/createTempFile "bark-test-" ".edn")) contents)]
    (try
      (doseq [[file content] (map vector files contents)]
        (spit file content))
      (f (mapv #(.getAbsolutePath %) files))
      (finally (doseq [f files] (.delete f))))))

(deftest validate-entry-happy-path
  (with-tmp-config
    (fn [cfg]
      (testing "valid entry with both :start and :end"
        (is (empty? (h/validate-entry 0 {:config cfg :start "2020-01-01" :end "2022-01-01"}))))
      (testing "valid entry with :start only (living config)"
        (is (empty? (h/validate-entry 0 {:config cfg :start "2020-01-01"}))))
      (testing "valid entry with :end only (first era, unbounded past)"
        (is (empty? (h/validate-entry 0 {:config cfg :end "2020-01-01"}))))
      (testing "valid entry with neither (single-era plan)"
        (is (empty? (h/validate-entry 0 {:config cfg})))))))

(deftest validate-entry-rejects-non-map
  (is (seq (h/validate-entry 0 "not a map")))
  (is (seq (h/validate-entry 0 nil)))
  (is (seq (h/validate-entry 0 [:config "foo"]))))

(deftest validate-entry-missing-config
  (let [errs (h/validate-entry 2 {:start "2020-01-01"})]
    (is (= 1 (count errs)))
    (is (re-find #"missing :config" (first errs)))))

(deftest validate-entry-config-file-not-found
  (let [errs (h/validate-entry 0 {:config "definitely-does-not-exist.edn"})]
    (is (seq (filter #(re-find #"config file not found" %) errs)))))

(deftest validate-entry-bad-date-formats
  (with-tmp-config
    (fn [cfg]
      (testing ":start in wrong format"
        (let [errs (h/validate-entry 0 {:config cfg :start "01/01/2020"})]
          (is (seq (filter #(re-find #":start not ISO" %) errs)))))
      (testing ":end in wrong format"
        (let [errs (h/validate-entry 0 {:config cfg :end "yesterday"})]
          (is (seq (filter #(re-find #":end not ISO" %) errs))))))))

(deftest validate-entry-start-not-before-end
  (with-tmp-config
    (fn [cfg]
      (let [errs (h/validate-entry 0 {:config cfg :start "2022-01-01" :end "2020-01-01"})]
        (is (seq (filter #(re-find #":start must be strictly before :end" %) errs))))
      (let [errs (h/validate-entry 0 {:config cfg :start "2020-01-01" :end "2020-01-01"})]
        (is (seq (filter #(re-find #":start must be strictly before :end" %) errs)))))))

(deftest validate-contiguity-ok
  (let [errs (h/validate-contiguity
              [{:config "a" :end "2020-01-01"}
               {:config "b" :start "2020-01-01" :end "2021-01-01"}
               {:config "c" :start "2021-01-01"}])]
    (is (empty? errs))))

(deftest validate-contiguity-detects-gap
  (let [errs (h/validate-contiguity
              [{:config "a" :end "2020-01-01"}
               {:config "b" :start "2020-06-01"}])]
    (is (= 1 (count errs)))
    (is (re-find #"gap/overlap" (first errs)))))

(deftest validate-contiguity-detects-overlap
  (let [errs (h/validate-contiguity
              [{:config "a" :end "2021-01-01"}
               {:config "b" :start "2020-06-01"}])]
    (is (= 1 (count errs)))
    (is (re-find #"gap/overlap" (first errs)))))

(deftest validate-contiguity-missing-intermediate-bounds
  (testing "inner entry missing :end"
    (let [errs (h/validate-contiguity
                [{:config "a"}
                 {:config "b" :start "2020-01-01"}])]
      (is (seq (filter #(re-find #"missing :end" %) errs)))))
  (testing "inner entry missing :start"
    (let [errs (h/validate-contiguity
                [{:config "a" :end "2020-01-01"}
                 {:config "b"}])]
      (is (seq (filter #(re-find #"missing :start" %) errs))))))

(deftest validate-contiguity-3-entries-gap-in-the-middle
  (testing "3-entry plan with a gap between entries 1 and 2"
    (let [errs (h/validate-contiguity
                [{:config "a" :end "2020-01-01"}
                 {:config "b" :start "2020-01-01" :end "2021-01-01"}
                 {:config "c" :start "2022-01-01"}])]
      (is (seq errs))
      (is (some #(re-find #"entries 1 and 2" %) errs)))))

(deftest validate-db-paths-all-match
  (with-tmp-cfgs
    ["{:db {:path \"/tmp/shared\"}}"
     "{:db {:path \"/tmp/shared\"}}"]
    (fn [[a b]]
      (is (nil? (h/validate-db-paths [{:config a} {:config b}]))))))

(deftest validate-db-paths-detects-divergence
  (with-tmp-cfgs
    ["{:db {:path \"/tmp/alpha\"}}"
     "{:db {:path \"/tmp/beta\"}}"]
    (fn [[a b]]
      (let [errs (h/validate-db-paths [{:config a} {:config b}])]
        (is (seq errs))
        (is (re-find #"different :db :path" (first errs)))))))

(deftest validate-db-paths-default-path-matches-explicit
  (with-tmp-cfgs
    ["{}"                                   ;; no :db → default "data/bark-db"
     "{:db {:path \"data/bark-db\"}}"]
    (fn [[a b]]
      (is (nil? (h/validate-db-paths [{:config a} {:config b}]))))))

(deftest validate-db-paths-skips-unreadable
  ;; An entry whose config file can't be parsed is skipped — validate-entry
  ;; surfaces that kind of problem separately.
  (with-tmp-cfgs
    ["{:db {:path \"/tmp/x\"}}"]
    (fn [[a]]
      (is (nil? (h/validate-db-paths
                 [{:config a} {:config "definitely-missing.edn"}]))))))

(deftest validate-history-composes-entry-and-contiguity
  (with-tmp-config
    (fn [cfg]
      (testing "single-entry plan skips contiguity check"
        (is (nil? (h/validate-history [{:config cfg :start "2020-01-01"}]))))
      (testing "valid multi-era plan returns nil"
        (is (nil? (h/validate-history
                   [{:config cfg :end "2020-01-01"}
                    {:config cfg :start "2020-01-01"}]))))
      (testing "collects errors from both stages"
        (let [errs (h/validate-history
                    [{:config cfg :start "not-iso"}  ;; entry error
                     {:config cfg :start "2020-01-01"}])] ;; contiguity error (no :end on previous)
          (is (>= (count errs) 1)))))))

(deftest merge-fetch-window-overwrites-ingest-fetch
  (testing "replaces any existing :fetch with the era window"
    (is (= {:ingest {:fetch {:start "2020-01-01" :end "2022-01-01"}}}
           (h/merge-fetch-window {:ingest {:fetch {:limit 50}}}
                                 {:start "2020-01-01" :end "2022-01-01"}))))
  (testing ":start only → empty :end in fetch"
    (is (= {:ingest {:fetch {:start "2020-01-01"}}}
           (h/merge-fetch-window {} {:start "2020-01-01"}))))
  (testing ":end only → empty :start in fetch"
    (is (= {:ingest {:fetch {:end "2020-01-01"}}}
           (h/merge-fetch-window {} {:end "2020-01-01"}))))
  (testing "no bounds → :fetch dropped (era config's own default applies)"
    (is (= {} (h/merge-fetch-window {} {})))
    (is (= {:ingest {}}
           (h/merge-fetch-window {:ingest {:fetch {:limit 50}}} {})))
    (is (= {:ingest {:max-size 1048576}}
           (h/merge-fetch-window {:ingest {:fetch {:limit 50} :max-size 1048576}}
                                 {})))))
