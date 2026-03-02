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
           [java.security MessageDigest]
           [org.jsoup Jsoup]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- format-address
  "Format {:name \"Alice\" :address \"alice@example.com\"} as a string."
  [{addr-name :name address :address}]
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
    (let [ids (->> (re-seq #"<[^>]+>" s)
                   (map str)
                   set)]
      (when (seq ids) ids))))

(defn sha256
  "Compute SHA-256 hex digest of a string."
  [^String s]
  (let [digest (MessageDigest/getInstance "SHA-256")
        bytes  (.digest digest (.getBytes s "UTF-8"))]
    (apply str (map #(format "%02x" %) bytes))))

(defn uid-hash
  "Compute a globally unique uid from mailbox-id and IMAP UID."
  [mailbox-id imap-uid]
  (sha256 (str mailbox-id ":" imap-uid)))

;; ---------------------------------------------------------------------------
;; Transform
;; ---------------------------------------------------------------------------

(defn email->txdata
  "Convert a fetch-imap message map to Datalevin transaction data.
  mailbox-id is a string like \"imap.example.com:user@example.com\"."
  [mailbox-id msg]
  (let [imap-uid    (:uid msg)
        body        (:body msg)
        text        (:text body)
        html-body   (:html body)
        text-from-html (strip-tags html-body)
        from        (first (:from msg))
        headers     (:headers msg)
        in-reply-to (when-let [v (get headers "In-Reply-To")]
                      (let [s (str/trim (if (vector? v) (first v) v))]
                        (when-not (str/blank? s) s)))
        references  (when-let [v (get headers "References")]
                      (parse-message-ids (if (vector? v) (str/join " " v) v)))
        attachments (mapv (fn [att]
                            {:attachment/filename     (or (:filename att) "unnamed")
                             :attachment/content-type (:content-type att)
                             :attachment/size         (or (:size att)
                                                         (when (:data att)
                                                           (count (:data att))))})
                          (remove nil? (:attachments body)))]
    (cond-> {:email/uid           (uid-hash mailbox-id imap-uid)
             :email/imap-uid      imap-uid
             :email/mailbox       mailbox-id
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
      (assoc :email/attachments attachments)

      ;; Threading headers
      in-reply-to
      (assoc :email/in-reply-to in-reply-to)

      references
      (assoc :email/references references))))

;; ---------------------------------------------------------------------------
;; Store
;; ---------------------------------------------------------------------------

(defn- truncate
  "Truncate string s to at most n characters."
  [s n]
  (when s (subs s 0 (min n (count s)))))

(defn store-email!
  "Store a single parsed email in Datalevin.
  Skips if uid hash or Message-ID already exists.
  Returns true if the email was stored, false/nil otherwise."
  [conn mailbox-id msg]
  (let [imap-uid (:uid msg)]
    (cond
      (nil? imap-uid)
      (do (log/warn "Skipping email with nil UID") false)

      (db/email-exists? conn (uid-hash mailbox-id imap-uid))
      (do (log/debug "Skipping already stored email UID:" imap-uid "(uid-hash)") false)

      (db/message-id-exists? conn (:message-id msg))
      (do (log/debug "Skipping already stored email UID:" imap-uid
                     "Message-ID:" (:message-id msg) "(from another mailbox)")
          false)

      :else
      (let [txdata (email->txdata mailbox-id msg)]
        (d/transact! conn [txdata])
        (log/info "Stored email UID:" imap-uid
                  "Subject:" (truncate (:email/subject txdata) 60))
        true))))

(defn store-emails!
  "Store a batch of parsed emails. Returns the count of newly stored emails."
  [conn mailbox-id msgs]
  (let [stored (reduce (fn [n msg]
                         (try
                           (if (store-email! conn mailbox-id msg)
                             (inc n)
                             n)
                           (catch Exception e
                             (log/error "Failed to store email UID:" (:uid msg) (.getMessage e))
                             n)))
                       0 msgs)]
    (log/info "Batch complete. Stored" stored "of" (count msgs) "emails.")
    stored))
