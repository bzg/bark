;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark-ingest.ingest
  "Transform parsed email maps into Datalevin transactions and store them."
  (:require [bark-ingest.db :as db]
            [datalevin.core :as d]
            [clojure.tools.logging :as log])
  (:import [java.util Date]
           [org.jsoup Jsoup]))

(defn- format-address
  "Format {:name \"Alice\" :address \"alice@example.com\"} as a string."
  [{:keys [name address]}]
  (if name
    (str name " <" address ">")
    address))

(defn strip-tags
  "Extract visible text from HTML using Jsoup.
  Returns plain text, or nil if input is nil."
  [^String html]
  (when html
    (.text (Jsoup/parse html))))

(defn email->txdata
  "Convert a fetch-imap message map to Datalevin transaction data.
  Returns a map suitable for d/transact!."
  [msg]
  (let [body      (:body msg)
        text      (:text body)
        html-body (:html body)
        text-from-html (strip-tags html-body)
        from      (first (:from msg))
        attachments (mapv (fn [att]
                            {:attachment/filename     (or (:filename att) "unnamed")
                             :attachment/content-type (:content-type att)
                             :attachment/size         (or (:size att)
                                                         (when (:data att)
                                                           (count (:data att))))})
                          (remove nil? (:attachments body)))]
    (cond-> {:email/uid           (:uid msg)
             :email/message-id    (:message-id msg)
             :email/subject       (or (:subject msg) "(no subject)")
             :email/content-type  (:content-type msg)
             :email/ingested-at   (Date.)}

      ;; From
      (:address from)
      (assoc :email/from-address (:address from))

      (:name from)
      (assoc :email/from-name (:name from))

      ;; To — store as set of formatted address strings
      (seq (:to msg))
      (assoc :email/to (set (map format-address (:to msg))))

      ;; Cc
      (seq (:cc msg))
      (assoc :email/cc (set (map format-address (:cc msg))))

      ;; Dates
      (:date-sent msg)
      (assoc :email/date-sent (:date-sent msg))

      (:date-received msg)
      (assoc :email/date-received (:date-received msg))

      ;; Body text
      text
      (assoc :email/body-text text)

      ;; Body HTML
      html-body
      (assoc :email/body-html html-body)

      ;; Plain text extracted from HTML
      text-from-html
      (assoc :email/body-text-from-html text-from-html)

      ;; Flags
      (seq (:flags msg))
      (assoc :email/flags (:flags msg))

      ;; Attachments (metadata only, no binary data)
      (seq attachments)
      (assoc :email/attachments attachments))))

(defn store-email!
  "Store a single parsed email in Datalevin. Skips if UID already exists."
  [conn msg]
  (let [uid (:uid msg)]
    (if (and uid (db/email-exists? conn uid))
      (log/debug "Skipping already stored email UID:" uid)
      (let [txdata (email->txdata msg)]
        (d/transact! conn [txdata])
        (log/info "Stored email UID:" uid
                  "Subject:" (subs (or (:email/subject txdata) "") 0
                                   (min 60 (count (or (:email/subject txdata) "")))))))))

(defn store-emails!
  "Store a batch of parsed emails. Returns the count of newly stored emails."
  [conn msgs]
  (let [before (count (d/datoms (d/db conn) :eav))]
    (doseq [msg msgs]
      (try
        (store-email! conn msg)
        (catch Exception e
          (log/error "Failed to store email UID:" (:uid msg) (.getMessage e)))))
    (let [after (count (d/datoms (d/db conn) :eav))]
      (log/info "Batch complete. Datoms before:" before "after:" after))))
