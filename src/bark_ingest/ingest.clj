;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark-ingest.ingest
  "Transform parsed email maps into Datalevin transactions and store them.
  Source classification is deferred to bark-digest."
  (:require [bark-ingest.db :as db]
            [datalevin.core :as d]
            [clojure.string :as str]
            [clojure.tools.logging :as log])
  (:import [java.util Date]
           [org.jsoup Jsoup]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- format-address
  "Format {:name \"Alice\" :address \"alice@example.com\"} as a string."
  [{addr-name :name address :address}]
  (if (str/blank? addr-name)
    address
    (str addr-name " <" address ">")))

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


;; ---------------------------------------------------------------------------
;; Transform
;; ---------------------------------------------------------------------------

(defn email->txdata
  "Convert a fetch-imap message map to Datalevin transaction data.
  No source is stamped here — that is resolved at digest time from headers."
  [msg]
  (let [imap-uid    (:uid msg)
        body        (:body msg)
        text        (:text body)
        html-body   (:html body)
        text-from-html (strip-tags html-body)
        from        (first (:from msg))
        headers     (:headers msg)
        headers-edn (when (seq headers) (pr-str headers))
        in-reply-to (when-let [v (get headers "In-Reply-To")]
                      (let [s (str/trim (if (vector? v) (first v) v))]
                        (when-not (str/blank? s) s)))
        references  (when-let [v (get headers "References")]
                      (parse-message-ids (if (vector? v) (str/join " " v) v)))
        attachments (mapv (fn [att]
                            (let [filename (or (:filename att) "unnamed")
                                  is-patch (boolean (re-find #"(?i)\.(patch|diff)$" filename))
                                  data     (:data att)
                                  text-data (when (and is-patch data)
                                              (cond
                                                (string? data) data
                                                (bytes? data)  (String. ^bytes data "UTF-8")
                                                :else          nil))]
                              (cond-> {:attachment/filename     filename
                                       :attachment/content-type (:content-type att)
                                       :attachment/size         (or (:size att)
                                                                    (when data (count data)))}
                                text-data (assoc :attachment/data text-data))))
                          (remove nil? (:attachments body)))]
    (cond-> {:email/imap-uid     imap-uid
             :email/message-id   (:message-id msg)
             :email/subject      (or (:subject msg) "(no subject)")
             :email/content-type (:content-type msg)
             :email/ingested-at  (Date.)}

      (:address from)      (assoc :email/from-address (:address from))
      (:name from)         (assoc :email/from-name (:name from))
      (seq (:to msg))      (assoc :email/to (set (map format-address (:to msg))))
      (seq (:cc msg))      (assoc :email/cc (set (map format-address (:cc msg))))
      (:date-sent msg)     (assoc :email/date-sent (:date-sent msg))
      (:date-received msg) (assoc :email/date-received (:date-received msg))
      text                 (assoc :email/body-text text)
      html-body            (assoc :email/body-html html-body)
      text-from-html       (assoc :email/body-text-from-html text-from-html)
      (seq (:flags msg))   (assoc :email/flags (:flags msg))
      (seq attachments)    (assoc :email/attachments attachments)
      in-reply-to          (assoc :email/in-reply-to in-reply-to)
      references           (assoc :email/references references)
      headers-edn          (assoc :email/headers-edn headers-edn))))

;; ---------------------------------------------------------------------------
;; Store
;; ---------------------------------------------------------------------------

(defn- truncate
  "Truncate string s to at most n characters."
  [s n]
  (when (string? s) (subs s 0 (min n (count s)))))

(defn store-email!
  "Store a single parsed email in Datalevin.
  Skips if Message-ID is nil or already exists.
  Returns true if the email was stored, false/nil otherwise."
  [conn msg]
  (let [message-id (:message-id msg)
        imap-uid   (:uid msg)]
    (cond
      (nil? message-id)
      (do (log/warn "Skipping email with nil Message-ID, UID:" imap-uid) false)

      (db/message-id-exists? conn message-id)
      (do (log/debug "Skipping already stored Message-ID:" message-id) false)

      :else
      (let [txdata (email->txdata msg)]
        (d/transact! conn [txdata])
        (log/info "Stored email UID:" imap-uid
                  "Subject:" (truncate (:email/subject txdata) 60))
        true))))

(defn store-emails!
  "Store a batch of parsed emails. Returns the count of newly stored emails."
  [conn msgs]
  (let [stored (reduce (fn [n msg]
                         (try
                           (if (store-email! conn msg) (inc n) n)
                           (catch Exception e
                             (log/warn "Failed to store email UID:" (:uid msg) (.getMessage e))
                             n)))
                       0 msgs)]
    (log/info "Batch complete. Stored" stored "of" (count msgs) "emails.")
    stored))
