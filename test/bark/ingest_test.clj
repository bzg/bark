(ns bark.ingest-test
  "Unit tests for bark.ingest — ICS attachment data extraction."
  (:require [clojure.test :refer [deftest is testing]]
            [bark.ingest :as ingest]))

;; ---------------------------------------------------------------------------
;; ICS attachment data storage
;; ---------------------------------------------------------------------------

(deftest ics-attachment-data-stored
  (testing ".ics attachment text data is stored"
    (let [ics-data "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\nSUMMARY:Meeting\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"
          msg {:uid 100
               :message-id "<ics-test@test.org>"
               :subject "[ANN] Team meeting"
               :from [{:address "alice@test.org" :name "Alice"}]
               :date-sent #inst "2026-01-03T10:00:00Z"
               :body {:text "Please find the invite attached."
                      :attachments [{:filename "meeting.ics"
                                     :content-type "text/calendar"
                                     :data ics-data}]}}
          tx (ingest/email->txdata msg)]
      (is (= ics-data
             (-> tx :email/attachments first :attachment/data)))))

  (testing ".ics attachment bytes data is converted to string"
    (let [ics-data "BEGIN:VCALENDAR\r\nEND:VCALENDAR\r\n"
          msg {:uid 101
               :message-id "<ics-bytes@test.org>"
               :subject "[ANN] Event"
               :from [{:address "bob@test.org"}]
               :body {:text "Event invite."
                      :attachments [{:filename "event.ics"
                                     :content-type "text/calendar"
                                     :data (.getBytes ics-data "UTF-8")}]}}
          tx (ingest/email->txdata msg)]
      (is (= ics-data
             (-> tx :email/attachments first :attachment/data)))))

  (testing "non-ics non-patch attachment data is NOT stored"
    (let [msg {:uid 102
               :message-id "<pdf@test.org>"
               :subject "A document"
               :from [{:address "carol@test.org"}]
               :body {:text "See attached."
                      :attachments [{:filename "document.pdf"
                                     :content-type "application/pdf"
                                     :data "binary-data"}]}}
          tx (ingest/email->txdata msg)]
      (is (nil? (-> tx :email/attachments first :attachment/data)))))

  (testing ".patch attachment data is still stored"
    (let [patch-data "diff --git a/file b/file\n--- a/file\n+++ b/file\n"
          msg {:uid 103
               :message-id "<patch@test.org>"
               :subject "[PATCH] fix something"
               :from [{:address "dev@test.org"}]
               :body {:text "Patch attached."
                      :attachments [{:filename "fix.patch"
                                     :content-type "text/x-diff"
                                     :data patch-data}]}}
          tx (ingest/email->txdata msg)]
      (is (= patch-data
             (-> tx :email/attachments first :attachment/data)))))

  (testing "oversized attachment data is not stored"
    (let [big-data (apply str (repeat (inc ingest/default-max-attachment-size) "X"))
          msg {:uid 104
               :message-id "<big-ics@test.org>"
               :subject "[ANN] Huge event"
               :from [{:address "alice@test.org"}]
               :body {:text "Big invite."
                      :attachments [{:filename "huge.ics"
                                     :content-type "text/calendar"
                                     :data big-data}]}}
          tx (ingest/email->txdata msg)]
      (is (nil? (-> tx :email/attachments first :attachment/data)))
      (is (= "huge.ics" (-> tx :email/attachments first :attachment/filename)))))

  (testing "text/plain attachment data is stored"
    (let [txt-data "Exception in thread \"main\" java.lang.NullPointerException\n\tat Foo.bar(Foo.java:42)\n"
          msg {:uid 105
               :message-id "<txt@test.org>"
               :subject "[BUG] crash on startup"
               :from [{:address "user@test.org"}]
               :body {:text "See backtrace attached."
                      :attachments [{:filename "backtrace.txt"
                                     :content-type "text/plain"
                                     :data txt-data}]}}
          tx (ingest/email->txdata msg)]
      (is (= txt-data
             (-> tx :email/attachments first :attachment/data)))))

  (testing "text/x-log attachment data is stored"
    (let [log-data "2026-03-30 10:00:00 ERROR something broke\n"
          msg {:uid 106
               :message-id "<log@test.org>"
               :subject "[BUG] error in logs"
               :from [{:address "user@test.org"}]
               :body {:text "Log file attached."
                      :attachments [{:filename "error.log"
                                     :content-type "text/x-log"
                                     :data log-data}]}}
          tx (ingest/email->txdata msg)]
      (is (= log-data
             (-> tx :email/attachments first :attachment/data)))))

  (testing "text/plain with charset parameter is stored"
    (let [txt-data "some text content"
          msg {:uid 107
               :message-id "<txt-charset@test.org>"
               :subject "[BUG] encoding issue"
               :from [{:address "user@test.org"}]
               :body {:text "Attached."
                      :attachments [{:filename "output.txt"
                                     :content-type "text/plain; charset=utf-8"
                                     :data txt-data}]}}
          tx (ingest/email->txdata msg)]
      (is (= txt-data
             (-> tx :email/attachments first :attachment/data))))))
