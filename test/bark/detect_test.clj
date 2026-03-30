(ns bark.detect-test
  "Unit tests for bark.detect — ICS detection helpers."
  (:require [clojure.test :refer [deftest is testing]]
            [bark.detect :as detect]))

;; ---------------------------------------------------------------------------
;; ICS attachment detection
;; ---------------------------------------------------------------------------

(deftest has-ics-attachment-test
  (testing "detects .ics attachment"
    (is (detect/has-ics-attachment?
         [{:attachment/filename "meeting.ics"}])))

  (testing "case-insensitive"
    (is (detect/has-ics-attachment?
         [{:attachment/filename "Event.ICS"}])))

  (testing "no .ics attachment"
    (is (not (detect/has-ics-attachment?
              [{:attachment/filename "readme.txt"}]))))

  (testing "nil attachments"
    (is (not (detect/has-ics-attachment? nil))))

  (testing "empty attachments"
    (is (not (detect/has-ics-attachment? [])))))

;; ---------------------------------------------------------------------------
;; Inline ICS detection
;; ---------------------------------------------------------------------------

(deftest has-inline-ics-test
  (testing "detects inline VCALENDAR with VEVENT"
    (is (detect/has-inline-ics?
         "Some text\nBEGIN:VCALENDAR\nVERSION:2.0\nBEGIN:VEVENT\nSUMMARY:Test\nEND:VEVENT\nEND:VCALENDAR")))

  (testing "nil body"
    (is (not (detect/has-inline-ics? nil))))

  (testing "VCALENDAR without VEVENT"
    (is (not (detect/has-inline-ics?
              "BEGIN:VCALENDAR\nVERSION:2.0\nEND:VCALENDAR"))))

  (testing "plain text without ICS"
    (is (not (detect/has-inline-ics? "Hello world")))))

;; ---------------------------------------------------------------------------
;; Combined has-ics? check
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
