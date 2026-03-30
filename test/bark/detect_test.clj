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
