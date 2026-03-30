(ns bark.common-test
  "Unit tests for bark.common pure functions:
  classify-delivery, classify-source, parse-duration-str,
  gate-related helpers, and build-source-map edge cases."
  (:require [clojure.test :refer [deftest is testing]]
            [bark.common :as common]))

;; ---------------------------------------------------------------------------
;; parse-duration-str
;; ---------------------------------------------------------------------------

(deftest parse-duration-str-test
  (testing "single units"
    (is (= 1   (common/parse-duration-str "1d")))
    (is (= 7   (common/parse-duration-str "1w")))
    (is (= 30  (common/parse-duration-str "1m")))
    (is (= 365 (common/parse-duration-str "1y"))))

  (testing "zero value"
    (is (= 0 (common/parse-duration-str "0d"))))

  (testing "combined units"
    (is (= 37  (common/parse-duration-str "1m 1w")))
    (is (= 395 (common/parse-duration-str "1y 1m")))
    (is (= 402 (common/parse-duration-str "1y 1m 1w")))
    (is (= 403 (common/parse-duration-str "1y 1m 1w 1d"))))

  (testing "whitespace variations"
    (is (= 37 (common/parse-duration-str "1m1w")))
    (is (= 37 (common/parse-duration-str "1m  1w"))))

  (testing "nil on empty/no-match input"
    (is (nil? (common/parse-duration-str "")))
    (is (nil? (common/parse-duration-str "hello"))))

  (testing "throws on unknown units"
    (is (thrown-with-msg? Exception #"Unknown duration unit"
                          (common/parse-duration-str "5h")))
    (is (thrown-with-msg? Exception #"Unknown duration unit"
                          (common/parse-duration-str "2x 1d")))))

;; ---------------------------------------------------------------------------
;; parse-delay
;; ---------------------------------------------------------------------------

(deftest parse-delay-test
  (testing "integer passthrough"
    (is (= 42 (common/parse-delay 42)))
    (is (= 0  (common/parse-delay 0))))

  (testing "string delegation"
    (is (= 30 (common/parse-delay "1m")))
    (is (= 0  (common/parse-delay "0d"))))

  (testing "nil on unsupported types"
    (is (nil? (common/parse-delay :foo)))
    (is (nil? (common/parse-delay nil)))))

;; ---------------------------------------------------------------------------
;; classify-delivery
;; ---------------------------------------------------------------------------

(defn- make-headers [& kvs]
  (pr-str (vec (partition 2 kvs))))

(deftest classify-delivery-test
  (testing "List-Id header → :list"
    (is (= :list (common/classify-delivery
                  (make-headers "List-Id" "<bugs.example.org>")))))

  (testing "X-BeenThere header → :list"
    (is (= :list (common/classify-delivery
                  (make-headers "X-BeenThere" "list@example.org")))))

  (testing "original recipient not in To/Cc → :alias"
    (is (= :alias (common/classify-delivery
                   (make-headers "X-Original-To" "alias@example.org"
                                "To" "someone@else.org")))))

  (testing "original recipient in To → :direct"
    (is (= :direct (common/classify-delivery
                    (make-headers "X-Original-To" "me@example.org"
                                 "To" "me@example.org")))))

  (testing "no special headers → :direct"
    (is (= :direct (common/classify-delivery
                    (make-headers "To" "inbox@example.org")))))

  (testing "nil headers → :direct"
    (is (= :direct (common/classify-delivery nil)))))

;; ---------------------------------------------------------------------------
;; classify-source
;; ---------------------------------------------------------------------------

(def test-sources
  [{:name "my-list"  :list "bugs.example.org"}
   {:name "my-alias" :alias "sec@example.com"}
   {:name "my-box"   :to "inbox@example.com"}])

(deftest classify-source-test
  (testing "mailing list match via List-Id"
    (is (= "my-list"
           (common/classify-source
            (make-headers "List-Id" "<bugs.example.org>")
            "[BUG] something" test-sources))))

  (testing "alias match via X-Original-To"
    (is (= "my-alias"
           (common/classify-source
            (make-headers "X-Original-To" "sec@example.com"
                          "To" "other@example.com")
            "test" test-sources))))

  (testing "mailbox match via Delivered-To"
    (is (= "my-box"
           (common/classify-source
            (make-headers "Delivered-To" "inbox@example.com")
            "hello" test-sources))))

  (testing "no match → nil"
    (is (nil? (common/classify-source
               (make-headers "To" "unknown@example.com")
               "hello" test-sources))))

  (testing "bark-source fallback in subject prefix"
    (is (= "my-list"
           (common/classify-source
            (make-headers "To" "unknown@example.com")
            "[my-list] some subject" test-sources))))

  (testing "bark-source fallback is case-insensitive"
    (is (= "my-list"
           (common/classify-source
            (make-headers "To" "unknown@example.com")
            "[MY-LIST] some subject" test-sources)))))

;; ---------------------------------------------------------------------------
;; source-type
;; ---------------------------------------------------------------------------

(deftest source-type-test
  (is (= :mailing-list (common/source-type {:list "x"})))
  (is (= :alias        (common/source-type {:alias "x"})))
  (is (= :mailbox      (common/source-type {:to "x"})))
  (is (nil?            (common/source-type {:name "broken"}))))

;; ---------------------------------------------------------------------------
;; build-source-map — nil source-type exclusion (fix 11)
;; ---------------------------------------------------------------------------

(deftest build-source-map-excludes-nil-type
  (let [config {:sources [{:name "good" :list "bugs.example.org"}
                           {:name "bad"}]}
        sm     (common/build-source-map config)]
    (is (contains? sm "good"))
    (is (not (contains? sm "bad")))))

;; ---------------------------------------------------------------------------
;; slugify
;; ---------------------------------------------------------------------------

(deftest slugify-test
  (is (= "hello-world"  (common/slugify "Hello World")))
  (is (= "cafe"         (common/slugify "Café")))
  (is (= "a-b-c"        (common/slugify "a--b--c")))
  (is (= "test"         (common/slugify "-test-"))))

;; ---------------------------------------------------------------------------
;; header utilities
;; ---------------------------------------------------------------------------

(deftest get-header-case-insensitive
  (let [hdrs (make-headers "List-Id" "<test.example.org>"
                           "X-Custom" "value")]
    (is (= "<test.example.org>" (common/get-header hdrs "list-id")))
    (is (= "value" (common/get-header hdrs "x-custom")))
    (is (nil? (common/get-header hdrs "absent")))))

(deftest extract-list-id-test
  (is (= "bugs.example.org"
         (common/extract-list-id "Some list <bugs.example.org>")))
  (is (= "raw-value" (common/extract-list-id "raw-value")))
  (is (nil? (common/extract-list-id nil))))

(deftest extract-in-reply-to-test
  (let [hdrs (make-headers "In-Reply-To" " <msg@test> ")]
    (is (= "<msg@test>" (common/extract-in-reply-to hdrs))))
  (let [hdrs (make-headers "In-Reply-To" "  ")]
    (is (nil? (common/extract-in-reply-to hdrs)))))

;; ---------------------------------------------------------------------------
;; Role checks (pure)
;; ---------------------------------------------------------------------------

(deftest maintainer-with-since-test
  (let [roles {:roles/admin "admin@test.org"
               :roles/maintainers #{"alice@test.org"}
               :roles/maintainer-since #{"alice@test.org:2025-06-01"}}]
    (testing "no date check → always true"
      (is (common/maintainer? roles "alice@test.org")))

    (testing "email after since date → true"
      (is (common/maintainer? roles "alice@test.org"
                              #inst "2025-07-01T00:00:00Z")))

    (testing "email before since date → false"
      (is (not (common/maintainer? roles "alice@test.org"
                                   #inst "2025-01-01T00:00:00Z"))))))

;; ---------------------------------------------------------------------------
;; resolve-commands-map with extended form
;; ---------------------------------------------------------------------------

(deftest resolve-commands-map-extended-form
  (let [cfg {:commands {:acked {:words ["LGTM" "Approved"]
                                :scope :maintainer}}}
        m   (common/resolve-commands-map cfg)]
    (is (= ["LGTM" "Approved"] (:acked m)))
    (testing "other defaults preserved"
      (is (seq (:closed m)))
      (is (seq (:owned m))))))

(deftest resolve-command-overrides-test
  (let [cfg {:commands {:acked {:words ["LGTM"]
                                :scope :maintainer
                                :report-types #{:bug}}}}
        ov  (common/resolve-command-overrides cfg)]
    (is (= :maintainer (:scope (:acked ov))))
    (is (= #{:bug} (:report-types (:acked ov))))))

;; ---------------------------------------------------------------------------
;; sent-via-source-channel?
;; ---------------------------------------------------------------------------

(deftest sent-via-source-channel-test
  (testing "mailing-list source: only :list delivery is via channel"
    (let [cfg {:source-type :mailing-list}]
      (is (true?  (common/sent-via-source-channel? :list cfg)))
      (is (false? (common/sent-via-source-channel? :direct cfg)))
      (is (false? (common/sent-via-source-channel? :alias cfg)))))

  (testing "alias source: only :alias delivery is via channel"
    (let [cfg {:source-type :alias}]
      (is (true?  (common/sent-via-source-channel? :alias cfg)))
      (is (false? (common/sent-via-source-channel? :direct cfg)))
      (is (false? (common/sent-via-source-channel? :list cfg)))))

  (testing "mailbox source: all deliveries are via channel"
    (let [cfg {:source-type :mailbox}]
      (is (true? (common/sent-via-source-channel? :direct cfg)))
      (is (true? (common/sent-via-source-channel? :list cfg)))
      (is (true? (common/sent-via-source-channel? :alias cfg)))))

  (testing "unknown source-type: always false"
    (is (false? (common/sent-via-source-channel? :direct {})))
    (is (false? (common/sent-via-source-channel? :list {})))))

;; ---------------------------------------------------------------------------
;; ics-file?
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; parse-cli-args — --topics-filter
;; ---------------------------------------------------------------------------

(deftest parse-cli-args-topics-filter
  (is (= "event,security"
         (:topics-filter (common/parse-cli-args ["--topics-filter" "event,security"]))))
  (is (nil? (:topics-filter (common/parse-cli-args ["--force"])))))

;; ---------------------------------------------------------------------------
;; ics-file?
;; ---------------------------------------------------------------------------

(deftest ics-file-test
  (is (true?  (common/ics-file? "event.ics")))
  (is (true?  (common/ics-file? "Meeting.ICS")))
  (is (true?  (common/ics-file? "path/to/file.ics")))
  (is (false? (common/ics-file? "readme.txt")))
  (is (false? (common/ics-file? "ics-like.doc")))
  (is (false? (common/ics-file? nil))))
