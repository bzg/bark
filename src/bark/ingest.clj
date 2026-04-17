;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.ingest
  "Email ingestion: Datalevin connection, storage, and email→txdata transform."
  (:require [bark.common :as common]
            [datalevin.core :as d]
            [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.util Date]
           [org.jsoup Jsoup]))

;; ---------------------------------------------------------------------------
;; Database connection
;; ---------------------------------------------------------------------------

(defn connect [db-path]
  (log/info "Opening Datalevin database at" db-path)
  (d/get-conn db-path common/bark-schema {:wal? false}))

(defn close [conn] (d/close conn))

(defn- entity-exists? [conn attr v]
  (when v (some? (d/entid (d/db conn) [attr v]))))

(defn max-imap-uid [conn]
  (or (d/q '[:find ?uid . :where [?e :watermark/id "default"] [?e :watermark/imap-uid ?uid]]
           (d/db conn))
      0))

(defn save-imap-uid! [conn imap-uid]
  (d/transact! conn [{:watermark/id "default" :watermark/imap-uid imap-uid}]))

(defn known-email-ids
  "Return the set of all :email/id values stored in the DB."
  [conn]
  (into #{}
        (d/q '[:find [?id ...] :where [_ :email/id ?id]]
             (d/db conn))))

(defn seen-maildir-ids
  "Return the set of Maildir ids recorded as seen (first-run baseline)."
  [conn]
  (into #{}
        (d/q '[:find [?id ...]
               :where [?e :watermark/id "default"]
                      [?e :watermark/seen-ids ?id]]
             (d/db conn))))

(defn mark-ids-seen!
  "Record Maildir ids as seen on the watermark entity so they are
  excluded from future incremental diffs."
  [conn ids]
  (d/transact! conn [{:watermark/id "default"
                      :watermark/seen-ids (set ids)}]))

(defn maildir-init-done? [conn]
  (true? (d/q '[:find ?v .
                :where [?e :watermark/id "default"]
                       [?e :watermark/maildir-init ?v]]
              (d/db conn))))

(defn set-maildir-init-done! [conn]
  (d/transact! conn [{:watermark/id "default" :watermark/maildir-init true}]))

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(def default-max-attachment-size
  "Default maximum size (in characters) for extracted attachment text data
  (.patch, .diff, .ics, text/plain, text/x-log). Attachments exceeding
  this limit are stored without their text content. 1 MB."
  (* 1024 1024))

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
  Returns plain text, or nil if input is nil or parsing fails."
  [^String html]
  (when html
    (try (.text (Jsoup/parse html))
         (catch Exception e
           (log/warn "Failed to parse HTML:" (.getMessage e))
           nil))))

(defn- parse-message-ids
  "Parse a References header value into a single space-separated string
  of message-ids, preserving RFC 2822 order (root -> parent).
  Returns the string, or nil if empty."
  [s]
  (when s
    (let [ids (->> (re-seq #"<[^>]+>" s)
                   (map str)
                   distinct
                   vec)]
      (when (seq ids) (str/join " " ids)))))

;; ---------------------------------------------------------------------------
;; Transform
;; ---------------------------------------------------------------------------

(defn email->txdata
  "Convert a mailseq message map to Datalevin transaction data.
  No source is stamped here — that is resolved at digest time from headers.
  `opts` may contain :max-attachment-size to override the default (1 MB)."
  ([msg] (email->txdata msg {}))
  ([msg opts]
  (let [max-att-size (or (:max-attachment-size opts)
                         default-max-attachment-size)
        id          (:id msg)
        body        (:body msg)
        text        (:text body)
        html-body   (:html body)
        text-from-html (strip-tags html-body)
        from        (first (:from msg))
        headers     (:headers msg)
        headers-edn (when (seq headers) (pr-str headers))
        in-reply-to (common/extract-in-reply-to headers)
        references  (when-let [v (get headers "References")]
                      (parse-message-ids (if (vector? v)
                                           (str/join " " (keep identity v))
                                           v)))
        attachments (mapv (fn [att]
                            (let [filename (or (:filename att) "unnamed")
                                  is-patch (re-find #"(?i)\.(patch|diff)$" filename)
                                  is-ics   (re-find #"(?i)\.ics$" filename)
                                  is-text  (and (not (or is-patch is-ics))
                                                (common/text-attachment?
                                                  {:attachment/content-type
                                                   (:content-type att)}))
                                  data     (:data att)
                                  raw-text (when (and (or is-patch is-ics is-text) data)
                                              (cond
                                                (string? data) data
                                                (bytes? data)  (String. ^bytes data "UTF-8")
                                                :else          nil))
                                  too-large? (and raw-text
                                                  (> (count raw-text) max-att-size))
                                  text-data (when (and raw-text (not too-large?)) raw-text)]
                              (when too-large?
                                (log/warn "Attachment" filename "exceeds"
                                          (str (quot max-att-size 1024) "KB")
                                          "limit (" (count raw-text) "chars) — content not stored"))
                              (cond-> {:attachment/filename     filename
                                       :attachment/content-type (:content-type att)
                                       :attachment/size         (or (:size att)
                                                                    (when data (count data)))}
                                text-data (assoc :attachment/data text-data))))
                          (remove nil? (:attachments body)))]
    (cond-> {:email/message-id   (common/extract-bracketed-id (:message-id msg))
             :email/subject      (let [s (or (:subject msg) "")]
                                    (str/replace (if (str/blank? s) "(no subject)" s)
                                                 #"[\r\n]+" " "))
             :email/content-type (:content-type msg)
             :email/ingested-at  (Date.)}

      id                     (assoc :email/id id)

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
      headers-edn          (assoc :email/headers-edn headers-edn)))))

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
  Returns true if the email was stored, false/nil otherwise.
  `opts` may contain :max-attachment-size to override the default (1 MB)."
  ([conn msg] (store-email! conn msg {}))
  ([conn msg opts]
  (let [message-id (common/extract-bracketed-id (:message-id msg))
        id         (:id msg)]
    (cond
      (nil? message-id)
      (do (log/warn "Skipping email with nil Message-ID, id:" id) false)

      (entity-exists? conn :email/message-id message-id)
      (do (log/debug "Skipping already stored Message-ID:" message-id) false)

      (entity-exists? conn :email/id id)
      (do (log/warn "Skipping id collision:" id
                    "— different Message-ID but id already stored")
          false)

      :else
      (let [txdata (email->txdata msg opts)]
        (try
          (d/transact! conn [txdata])
          (log/info "Stored email id:" id
                    "Subject:" (truncate (:email/subject txdata) 60))
          true
          (catch Exception e
            ;; If the message-id now exists, another process (e.g. bb digest)
            ;; inserted it between our exists? check and the transact — harmless race.
            (let [now-exists? (try (entity-exists? conn :email/message-id message-id)
                                  (catch Exception _ false))]
              (if now-exists?
                (do (log/debug "Duplicate Message-ID (race):" message-id) false)
                (throw e))))))))))

