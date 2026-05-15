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

;; ---------------------------------------------------------------------------
;; Label-decides priority: subject label wins over patch content
;; ---------------------------------------------------------------------------

(def ^:private patch-attachment
  [{:attachment/filename "fix.patch"
    :attachment/content-type "text/x-patch"
    :attachment/size 1234}])

(def ^:private inline-diff-body
  (str "diff --git a/foo.el b/foo.el\n"
       "index abc123..def456 100644\n"
       "--- a/foo.el\n+++ b/foo.el\n@@ -1,3 +1,4 @@\n+;; fix\n"))

(deftest label-wins-over-patch-content-test
  (testing "[BUG] + .patch attachment yields :bug, not :patch"
    (is (= :bug (:type (detect/detect-report
                        {:email/subject    "[BUG] something broken"
                         :email/attachments patch-attachment})))))

  (testing "[POLL] + .patch attachment yields :request"
    (is (= :request (:type (detect/detect-report
                            {:email/subject    "[POLL] new feature?"
                             :email/attachments patch-attachment})))))

  (testing "[PATCH] subject still yields :patch (even without attachment)"
    (is (= :patch (:type (detect/detect-report
                          {:email/subject "[PATCH] subject-only patch"})))))

  (testing "inline diff alone in the body is no longer a patch signal"
    (is (nil? (detect/detect-report
               {:email/subject   "Small cleanup"
                :email/body-text inline-diff-body})))))

(deftest re-prefix-patch-with-attachment-test
  (testing "Re: [PATCH] foo WITHOUT attachment yields nil (no spurious report from discussion replies)"
    (is (nil? (detect-type "Re: [PATCH] feature")))
    (is (nil? (detect-type "Re: Re: [PATCH] feature"))))

  (testing "Re: [PATCH] foo WITH attachment matches :patch (v2/v3 reply workflow)"
    (is (= :patch (:type (detect/detect-report
                          {:email/subject     "Re: [PATCH] feature"
                           :email/in-reply-to "<parent@test.org>"
                           :email/attachments patch-attachment})))))

  (testing "Re: [PATCH] foo with inline diff only (no attachment) yields nil"
    (is (nil? (detect/detect-report
               {:email/subject     "Re: [PATCH] feature"
                :email/in-reply-to "<parent@test.org>"
                :email/body-text   inline-diff-body}))))

  (testing "Re: [PATCH v2] with attachment keeps version handling"
    (is (= "v2" (:version (detect/detect-report
                           {:email/subject     "Re: [PATCH v2] feature"
                            :email/in-reply-to "<parent@test.org>"
                            :email/attachments patch-attachment})))))

  (testing "Re: [BUG] foo never matches :bug (strict for non-patch)"
    (is (nil? (:type (detect/detect-report
                      {:email/subject     "Re: [BUG] something"
                       :email/in-reply-to "<parent@test.org>"
                       :email/attachments patch-attachment}))))))

(def ^:private format-patch-attachment
  "A .patch attachment that looks like real `git format-patch` output."
  [{:attachment/filename "0001-fix-it.patch"
    :attachment/content-type "text/x-patch"
    :attachment/data (str "From 0123456789abcdef0123456789abcdef01234567 Mon Sep 17 00:00:00 2026\n"
                          "From: Alice <alice@test.org>\n"
                          "Date: Mon, 11 May 2026 09:00:00 +0000\n"
                          "Subject: [PATCH] Fix it\n\n"
                          "diff --git a/foo b/foo\n--- a/foo\n+++ b/foo\n@@ -1,1 +1,1 @@\n-old\n+new\n")}])

(deftest format-patch-attachment-escapes-thread-test
  (testing "Re: <no PATCH label> + real git-format-patch attachment -> :patch"
    (is (= :patch (:type (detect/detect-report
                          {:email/subject     "Re: Add font lock for structures"
                           :email/in-reply-to "<parent@test.org>"
                           :email/attachments format-patch-attachment})))))

  (testing "Re: <no label> + format-patch attachment without [PATCH] in inner Subject -> nil"
    (let [att-no-patch [{:attachment/filename "0001-demo.patch"
                         :attachment/content-type "text/x-patch"
                         :attachment/data (str "From 0123456789abcdef0123456789abcdef01234567 Mon Sep 17 00:00:00 2026\n"
                                               "From: Alice <alice@test.org>\n"
                                               "Subject: [DEMO] Just a demo\n\n"
                                               "diff --git a/foo b/foo\n")}]]
      (is (nil? (detect/detect-report
                 {:email/subject     "Re: Something"
                  :email/in-reply-to "<parent@test.org>"
                  :email/attachments att-no-patch})))))

  (testing "[BUG] subject + real format-patch attachment -> :bug (label still wins)"
    (is (= :bug (:type (detect/detect-report
                        {:email/subject     "[BUG] crash on save"
                         :email/attachments format-patch-attachment})))))

  (testing "Re: discussion + git-diff (no format-patch headers) attachment -> nil"
    (let [att-diff [{:attachment/filename "debug.patch"
                     :attachment/content-type "text/x-patch"
                     :attachment/data "diff --git a/foo b/foo\n--- a/foo\n+++ b/foo\n@@ -1 +1 @@\n-old\n+new\n"}]]
      (is (nil? (detect/detect-report
                 {:email/subject     "Re: Discussion"
                  :email/in-reply-to "<parent@test.org>"
                  :email/attachments att-diff}))))))
