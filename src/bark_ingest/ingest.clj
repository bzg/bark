;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark-ingest.ingest
  "Transform parsed email maps into Datalevin transactions and store them."
  (:require [bark-ingest.db :as db]
            [datalevin.core :as d]
            [clojure.string :as str]
            [clojure.tools.logging :as log])
  (:import [java.util Date]
           [org.jsoup Jsoup]))

(defn- format-address
  "Format {:name \"Alice\" :address \"alice@example.com\"} as a string."
  [{:keys [address] addr-name :name}]
  (if addr-name
    (str addr-name " <" address ">")
    address))

(defn strip-tags
  "Extract visible text from HTML using Jsoup.
  Returns plain text, or nil if input is nil."
  [^String html]
  (when html
    (.text (Jsoup/parse html))))

(defn- parse-message-ids
  "Parse a space-separated list of message-ids (as in References header).
  Returns a set of message-id strings, or nil if empty."
  [s]
  (when s
    (let [ids (set (re-seq #"<[^>]+>" s))]
      (when (seq ids) ids))))

(defn- extract-header
  "Extract a header value from the headers map.
  If multi-valued (vector), join with joiner or return first."
  ([headers header-name]
   (extract-header headers header-name nil))
  ([headers header-name joiner]
   (when-let [v (get headers header-name)]
     (let [s (str/trim (if (vector? v)
                         (if joiner (str/join joiner v) (first v))
                         v))]
       (when-not (str/blank? s) s)))))

(defn email->txdata
  "Convert a fetch-imap message map to Datalevin transaction data.
  Returns a map suitable for d/transact!."
  [msg]
  (let [body           (:body msg)
        text           (:text body)
        html-body      (:html body)
        text-from-html (strip-tags html-body)
        from           (first (:from msg))
        headers        (:headers msg)
        in-reply-to    (extract-header headers "In-Reply-To")
        references     (when-let [v (extract-header headers "References" " ")]
                         (parse-message-ids v))
        attachments    (mapv (fn [att]
                               {:attachment/filename     (or (:filename att) "unnamed")
                                :attachment/content-type (:content-type att)
                                :attachment/size         (or (:size att)
                                                            (when (:data att)
                                                              (count (:data att))))})
                             (remove nil? (:attachments body)))]
    (into {:email/uid          (:uid msg)
           :email/message-id   (:message-id msg)
           :email/subject      (or (:subject msg) "(no subject)")
           :email/content-type (:content-type msg)
           :email/ingested-at  (Date.)}
          (filter val)
          {:email/from-address      (:address from)
           :email/from-name         (:name from)
           :email/to                (when (seq (:to msg))
                                      (set (map format-address (:to msg))))
           :email/cc                (when (seq (:cc msg))
                                      (set (map format-address (:cc msg))))
           :email/date-sent         (:date-sent msg)
           :email/date-received     (:date-received msg)
           :email/body-text         text
           :email/body-html         html-body
           :email/body-text-from-html text-from-html
           :email/flags             (when (seq (:flags msg)) (:flags msg))
           :email/attachments       (when (seq attachments) attachments)
           :email/in-reply-to       in-reply-to
           :email/references        references})))

(defn store-email!
  "Store a single parsed email in Datalevin. Skips if UID already exists or is nil."
  [conn msg]
  (let [uid (:uid msg)]
    (cond
      (nil? uid)
      (log/warn "Skipping email with nil UID, subject:" (:subject msg))

      (db/email-exists? conn uid)
      (log/debug "Skipping already stored email UID:" uid)

      :else
      (let [txdata (email->txdata msg)]
        (d/transact! conn [txdata])
        (log/info "Stored email UID:" uid
                  "Subject:" (subs (or (:email/subject txdata) "") 0
                                   (min 60 (count (or (:email/subject txdata) "")))))))))

(defn store-emails!
  "Store a batch of parsed emails."
  [conn msgs]
  (let [stored (atom 0)]
    (doseq [msg msgs]
      (try
        (when (store-email! conn msg)
          (swap! stored inc))
        (catch Exception e
          (log/error "Failed to store email UID:" (:uid msg) (.getMessage e)))))
    (log/info "Batch complete." @stored "new email(s) stored.")))
