(ns bark.detect-test
  "Unit tests for ICS detection helpers (common + detect)."
  (:require [clojure.test :refer [deftest is testing]]
            [bark.common :as common]
            [bark.detect :as detect]))

;; ---------------------------------------------------------------------------
;; ICS attachment detection (bark.common)
;; ---------------------------------------------------------------------------

(deftest has-ics-attachment-test
  (testing "detects .ics attachment"
    (is (common/has-ics-attachment?
         [{:attachment/filename "meeting.ics"}])))

  (testing "case-insensitive"
    (is (common/has-ics-attachment?
         [{:attachment/filename "Event.ICS"}])))

  (testing "no .ics attachment"
    (is (not (common/has-ics-attachment?
              [{:attachment/filename "readme.txt"}]))))

  (testing "nil attachments"
    (is (not (common/has-ics-attachment? nil))))

  (testing "empty attachments"
    (is (not (common/has-ics-attachment? [])))))

;; ---------------------------------------------------------------------------
;; Inline ICS detection (bark.common)
;; ---------------------------------------------------------------------------

(deftest has-inline-ics-test
  (testing "detects inline VCALENDAR with VEVENT"
    (is (common/has-inline-ics?
         "Some text\nBEGIN:VCALENDAR\nVERSION:2.0\nBEGIN:VEVENT\nSUMMARY:Test\nEND:VEVENT\nEND:VCALENDAR")))

  (testing "nil body"
    (is (not (common/has-inline-ics? nil))))

  (testing "VCALENDAR without VEVENT"
    (is (not (common/has-inline-ics?
              "BEGIN:VCALENDAR\nVERSION:2.0\nEND:VCALENDAR"))))

  (testing "plain text without ICS"
    (is (not (common/has-inline-ics? "Hello world")))))

;; ---------------------------------------------------------------------------
;; Combined has-ics? check (bark.detect)
;; ---------------------------------------------------------------------------

(deftest has-ics-test
  (testing "true with ICS attachment"
    (is (detect/has-ics?
         {:email/attachments [{:attachment/filename "event.ics"}]})))

  (testing "true with inline ICS"
    (is (detect/has-ics?
         {:email/body-text "BEGIN:VCALENDAR\nBEGIN:VEVENT\nEND:VEVENT\nEND:VCALENDAR"})))

  (testing "false without ICS"
    (is (not (detect/has-ics?
              {:email/body-text "Hello"
               :email/attachments [{:attachment/filename "doc.pdf"}]})))))

;; ---------------------------------------------------------------------------
;; Strict label regex: case-sensitive tag, mandatory `\s` or EOL after `]`
;; ---------------------------------------------------------------------------

(defn- detect-type [subject]
  (:type (detect/detect-report {:email/subject subject})))

(deftest label-regex-strict-test
  (testing "accepted forms"
    (is (= :bug   (detect-type "[BUG] Mon bug")))
    (is (= :bug   (detect-type "[BUG foo] Mon bug")))
    (is (= :bug   (detect-type "[BUG]")))
    (is (= :bug   (detect-type "[mylist] [BUG] Mon bug")))
    (is (= :patch (detect-type "[PATCH foo v2 1/2] Body")))
    (is (= :request     (detect-type "[POLL] question?")))
    (is (= :request     (detect-type "[TODO] task")))
    (is (= :announcement (detect-type "[ANN] news")))
    (is (= :announcement (detect-type "[ANNOUNCEMENT] news")))
    (is (= :release  (detect-type "[REL 2.0] notes")))
    (is (= :change   (detect-type "[CHG 9.8] heads-up"))))

  (testing "rejected: wrong case"
    (is (nil? (detect-type "[Bug] Mon bug")))
    (is (nil? (detect-type "[bug] Mon bug")))
    (is (nil? (detect-type "[Patch] body")))
    (is (nil? (detect-type "[poll] question?"))))

  (testing "rejected: no whitespace after closing bracket"
    (is (nil? (detect-type "[BUG]Mon bug")))
    (is (nil? (detect-type "[BUG foo]bar")))
    (is (nil? (detect-type "[POLL]?"))))

  (testing "rejected: malformed bracket content"
    (is (nil? (detect-type "[BUG/RFC] Mon bug")))
    (is (nil? (detect-type "[ BUG ] Mon bug")))
    (is (nil? (detect-type "BUG: Mon bug")))
    (is (nil? (detect-type "Re: [BUG] Mon bug")))))
