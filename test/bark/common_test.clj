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
            test-sources))))

  (testing "alias match via X-Original-To"
    (is (= "my-alias"
           (common/classify-source
            (make-headers "X-Original-To" "sec@example.com"
                          "To" "other@example.com")
            test-sources))))

  (testing "mailbox match via Delivered-To"
    (is (= "my-box"
           (common/classify-source
            (make-headers "Delivered-To" "inbox@example.com")
            test-sources))))

  (testing "no match → nil"
    (is (nil? (common/classify-source
               (make-headers "To" "unknown@example.com")
               test-sources)))))

;; ---------------------------------------------------------------------------
;; source-type
;; ---------------------------------------------------------------------------

(deftest source-type-test
  (is (= :mailing-list (common/source-type {:list "x"})))
  (is (= :alias        (common/source-type {:alias "x"})))
  (is (= :mailbox      (common/source-type {:to "x"})))
  (is (nil?            (common/source-type {:name "broken"}))))

;; ---------------------------------------------------------------------------
;; build-source-map -- nil source-type exclusion (fix 11)
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
    (is (nil? (common/extract-in-reply-to hdrs))))
  (testing "parenthetical comment after bracketed id is stripped"
    (let [hdrs (make-headers "In-Reply-To" "<msg@test> (in reply to Joe)")]
      (is (= "<msg@test>" (common/extract-in-reply-to hdrs)))))
  (testing "folded whitespace inside value is tolerated"
    (let [hdrs (make-headers "In-Reply-To" "\n\t<msg@test>")]
      (is (= "<msg@test>" (common/extract-in-reply-to hdrs))))))

(deftest extract-bracketed-id-test
  (testing "clean bracketed id returns unchanged"
    (is (= "<abc@x>" (common/extract-bracketed-id "<abc@x>"))))
  (testing "first bracketed token wins when value carries extra tokens"
    (is (= "<abc@x>" (common/extract-bracketed-id "<abc@x> (comment) <ignored@y>"))))
  (testing "padded input is trimmed to the bracketed token"
    (is (= "<abc@x>" (common/extract-bracketed-id "   <abc@x>   "))))
  (testing "vector values take the first element"
    (is (= "<abc@x>" (common/extract-bracketed-id ["<abc@x>" "<other@y>"]))))
  (testing "falls back to trimmed string when no bracketed token"
    (is (= "abc@x" (common/extract-bracketed-id "  abc@x  "))))
  (testing "blank and nil return nil"
    (is (nil? (common/extract-bracketed-id nil)))
    (is (nil? (common/extract-bracketed-id "")))
    (is (nil? (common/extract-bracketed-id "   ")))))

;; ---------------------------------------------------------------------------
;; Role checks (pure)
;; ---------------------------------------------------------------------------

(deftest maintainer-tenure-test
  (let [tenures [{:email "alice@test.org"
                  :from  #inst "2025-06-01T00:00:00Z"
                  :to    nil
                  :order 0}]]
    (testing "no date check → any active tenure matches"
      (is (common/maintainer? tenures "alice@test.org")))
    (testing "email after :from → true"
      (is (common/maintainer? tenures "alice@test.org"
                              #inst "2025-07-01T00:00:00Z")))
    (testing "email before :from → false"
      (is (not (common/maintainer? tenures "alice@test.org"
                                   #inst "2025-01-01T00:00:00Z")))))

  (testing "closed tenure does not match after :to"
    (let [tenures [{:email "bob@test.org"
                    :from  #inst "2025-01-01T00:00:00Z"
                    :to    #inst "2025-06-01T00:00:00Z"
                    :order 0}]]
      (is (common/maintainer? tenures "bob@test.org"
                              #inst "2025-03-01T00:00:00Z"))
      (is (not (common/maintainer? tenures "bob@test.org"
                                   #inst "2025-07-01T00:00:00Z"))))))

(deftest lead-maintainer-test
  (testing "earliest :from wins (nil sorts first)"
    (let [tenures [{:email "later@t.org" :from #inst "2025-06-01" :order 1}
                   {:email "first@t.org" :from nil                :order 0}
                   {:email "mid@t.org"   :from #inst "2025-03-01" :order 2}]]
      (is (= "first@t.org" (common/lead-maintainer tenures)))))
  (testing "tie-break by :order when :from is equal (both nil)"
    (let [tenures [{:email "b@t.org" :from nil :order 1}
                   {:email "a@t.org" :from nil :order 0}]]
      (is (= "a@t.org" (common/lead-maintainer tenures)))))
  (testing "closed tenures are ignored"
    (let [tenures [{:email "first@t.org" :from nil :to #inst "2025-01-01" :order 0}
                   {:email "now@t.org"   :from #inst "2025-02-01"          :order 1}]]
      (is (= "now@t.org" (common/lead-maintainer tenures)))))
  (testing "no active tenures → nil"
    (is (nil? (common/lead-maintainer []))))
  (testing "lead-maintainer? is case-insensitive"
    (let [tenures [{:email "lead@t.org" :from nil :order 0}]]
      (is (common/lead-maintainer? tenures "LEAD@t.org"))
      (is (not (common/lead-maintainer? tenures "other@t.org"))))))

;; ---------------------------------------------------------------------------
;; resolve-commands-map / resolve-command-overrides
;; ---------------------------------------------------------------------------

(deftest resolve-commands-map-test
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
;; resolve-command-syntax
;; ---------------------------------------------------------------------------

(deftest resolve-command-syntax-accepts-keywords
  (is (= :loose  (common/resolve-command-syntax {})))
  (is (= :loose  (common/resolve-command-syntax {:command-syntax :loose})))
  (is (= :strict (common/resolve-command-syntax {:command-syntax :strict}))))

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
;; parse-cli-args -- --topics-filter
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

;; ---------------------------------------------------------------------------
;; resolve-author -- Mailman/DMARC munging detection
;; ---------------------------------------------------------------------------

(deftest resolve-author-test
  (testing "no Reply-To: author = From"
    (is (= {:address "alice@example.org" :name "Alice"}
           (common/resolve-author
            {:from-address "alice@example.org"
             :from-name    "Alice"
             :reply-to     []}))))

  (testing "Reply-To present but From-name has no 'via' marker: keep From"
    ;; Standard Reply-To use (e.g. 'reply to my work address').  We must
    ;; NOT swap -- the actual author is in From.
    (is (= {:address "alice@example.org" :name "Alice"}
           (common/resolve-author
            {:from-address "alice@example.org"
             :from-name    "Alice"
             :reply-to     [{:address "alice-work@example.com"
                             :name    "Alice (work)"}]}))))

  (testing "Mailman/DMARC munging: From-name has 'via' + Reply-To different → use Reply-To"
    ;; Mirrors the BUG-org-habit fixture: Daniel Mendler's post via
    ;; gnu.org's mailman has From rewritten to the list address, and
    ;; the original sender address lives in Reply-To.
    (is (= {:address "mail@daniel-mendler.de" :name "Daniel Mendler"}
           (common/resolve-author
            {:from-address "emacs-orgmode@gnu.org"
             :from-name    "Daniel Mendler via \"General discussions about Org-mode.\""
             :reply-to     [{:address "mail@daniel-mendler.de"
                             :name    "Daniel Mendler"}]}))))

  (testing "munged From with no Reply-To: degrade to From rather than nil"
    (is (= {:address "list@example.org"
            :name    "Bob via \"Some List\""}
           (common/resolve-author
            {:from-address "list@example.org"
             :from-name    "Bob via \"Some List\""
             :reply-to     []}))))

  (testing "Reply-To name absent: fall back to From-name"
    (is (= {:address "real@author.io"
            :name    "Carol via \"L\""}
           (common/resolve-author
            {:from-address "list@example.org"
             :from-name    "Carol via \"L\""
             :reply-to     [{:address "real@author.io"}]}))))

  (testing "Reply-To equals From: no swap"
    (is (= {:address "alice@example.org" :name "Alice via \"L\""}
           (common/resolve-author
            {:from-address "alice@example.org"
             :from-name    "Alice via \"L\""
             :reply-to     [{:address "alice@example.org"}]})))))

