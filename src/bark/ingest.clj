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

(defn- id-collision?
  "True iff (:email/source, :email/id) is already stored.  Mailseq
  ids (IMAP UID, Maildir filename) are per-folder, so dedup is
  source-scoped."
  [conn source-name id]
  (when (and source-name id)
    (some? (d/q '[:find ?e . :in $ ?src ?id
                  :where [?e :email/source ?src] [?e :email/id ?id]]
                (d/db conn) source-name id))))

;; Watermarks are scoped per-mailbox: each `:mailboxes` entry's
;; `:name` keys its own `:watermark/id` entity, so IMAP UID / Maildir
;; baseline state never mix between mailboxes.

(defn- wm-id
  "Datalevin :watermark/id key for the given mailbox name.  Throws
  on any name `common/valid-config-name?` would reject -- never let
  two mailboxes collapse onto the same watermark id by accident, and
  never let an exotic character (slash, colon, control char) sneak
  into the entity id.  Uses an explicit `throw` rather than `:pre`
  so the guard survives a `(set! *assert* false)`."
  [mailbox-name]
  (when-not (common/valid-config-name? mailbox-name)
    (throw (ex-info "Invalid mailbox-name for watermark id"
                    {:mailbox-name mailbox-name})))
  (str "mailbox:" mailbox-name))

(defn max-imap-uid [conn mailbox-name]
  (or (d/q '[:find ?uid .
             :in $ ?wm
             :where [?e :watermark/id ?wm] [?e :watermark/imap-uid ?uid]]
           (d/db conn) (wm-id mailbox-name))
      0))

(defn save-imap-uid! [conn mailbox-name imap-uid]
  (when-not (and (integer? imap-uid) (not (neg? imap-uid)))
    (throw (ex-info "save-imap-uid! expects a non-negative integer UID"
                    {:mailbox-name mailbox-name :imap-uid imap-uid})))
  (d/transact! conn [{:watermark/id (wm-id mailbox-name)
                      :watermark/imap-uid imap-uid}]))

(defn stored-uid-validity
  "Recorded UIDVALIDITY for this mailbox's UID watermark (nil if never set)."
  [conn mailbox-name]
  (d/q '[:find ?uv .
         :in $ ?wm
         :where [?e :watermark/id ?wm] [?e :watermark/imap-uid-validity ?uv]]
       (d/db conn) (wm-id mailbox-name)))

(defn sync-uid-validity!
  "Align the stored UIDVALIDITY with the mailbox's live value.
  Returns :match, :stamped (first time), :reset (changed -- watermark
  cleared, caller falls back to first-run fetch), or :unsupported
  (backend cannot report UIDVALIDITY, e.g. Maildir)."
  [conn mailbox-name live-uv]
  (if (nil? live-uv)
    :unsupported
    (let [stored (stored-uid-validity conn mailbox-name)
          id     (wm-id mailbox-name)]
      (cond
        (nil? stored)
        (do (log/info "Stamping initial UIDVALIDITY:" live-uv
                      "for mailbox" (pr-str mailbox-name))
            (d/transact! conn [{:watermark/id id
                                :watermark/imap-uid-validity live-uv}])
            :stamped)

        (not= stored live-uv)
        (do (log/warn "UIDVALIDITY changed" stored "=>" live-uv
                      "for mailbox" (pr-str mailbox-name)
                      "-- clearing UID watermark, re-running initial fetch")
            (d/transact! conn [{:watermark/id id
                                :watermark/imap-uid-validity live-uv
                                :watermark/imap-uid 0}])
            :reset)

        :else :match))))

(defn known-email-ids
  "Return the set of all :email/id values stored in the DB."
  [conn]
  (into #{}
        (d/q '[:find [?id ...] :where [_ :email/id ?id]]
             (d/db conn))))

(defn seen-maildir-ids
  "Return the set of Maildir ids recorded as seen for this mailbox
  (first-run baseline)."
  [conn mailbox-name]
  (into #{}
        (d/q '[:find [?id ...]
               :in $ ?wm
               :where [?e :watermark/id ?wm]
                      [?e :watermark/seen-ids ?id]]
             (d/db conn) (wm-id mailbox-name))))

(defn mark-ids-seen!
  "Record Maildir ids as seen for this mailbox so future incremental
  diffs skip them.  `:watermark/seen-ids` has cardinality/many --
  successive calls accumulate, they do not replace the baseline."
  [conn mailbox-name ids]
  (d/transact! conn [{:watermark/id (wm-id mailbox-name)
                      :watermark/seen-ids (set ids)}]))

(defn maildir-init-done? [conn mailbox-name]
  (true? (d/q '[:find ?v .
                :in $ ?wm
                :where [?e :watermark/id ?wm]
                       [?e :watermark/maildir-init ?v]]
              (d/db conn) (wm-id mailbox-name))))

(defn set-maildir-init-done! [conn mailbox-name]
  (d/transact! conn [{:watermark/id (wm-id mailbox-name)
                      :watermark/maildir-init true}]))

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(def default-max-attachment-size
  "Cap (in chars) on extracted attachment text -- .patch/.diff/.ics
  /text/plain/text/x-log.  Larger payloads are stored without content."
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
  "Parse a References value into a space-separated mid string (root
  first), normalized and deduplicated.  Returns nil if none found."
  [s]
  (when s
    (let [ids (->> (re-seq #"<[^<>\s]+>" s)
                   (map common/normalize-mid)
                   distinct
                   vec)]
      (when (seq ids) (str/join " " ids)))))

;; ---------------------------------------------------------------------------
;; Transform
;; ---------------------------------------------------------------------------

(defn email->txdata
  "Mailseq message => Datalevin tx-data.  Source is NOT stamped here
  (resolved at digest time from headers).  Opts:
    :max-attachment-size -- override 1 MB cap
    :message-id          -- pre-normalized mid; recomputed if absent."
  ([msg] (email->txdata msg {}))
  ([msg opts]
  (let [max-att-size (or (:max-attachment-size opts)
                         default-max-attachment-size)
        message-id   (or (:message-id opts)
                         (common/extract-bracketed-id (:message-id msg)))
        id          (:id msg)
        body        (:body msg)
        text        (:text body)
        html-body   (:html body)
        text-from-html (strip-tags html-body)
        from        (first (:from msg))
        reply-to    (:reply-to msg)
        author      (common/resolve-author
                     {:from-address (:address from)
                      :from-name    (:name from)
                      :reply-to     reply-to})
        headers     (:headers msg)
        headers-edn (when (seq headers) (pr-str headers))
        in-reply-to (common/extract-in-reply-to headers)
        references  (when-let [v (get headers "References")]
                      (parse-message-ids (if (vector? v)
                                           (str/join " " (keep identity v))
                                           v)))
        ancestor-mids (common/ancestor-mids-from references in-reply-to)
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
                                          "limit (" (count raw-text) "chars) -- content not stored"))
                              (cond-> {:attachment/filename     filename
                                       :attachment/content-type (:content-type att)
                                       :attachment/size         (or (:size att)
                                                                    (when data (count data)))}
                                text-data (assoc :attachment/data text-data))))
                          (remove nil? (:attachments body)))]
    (cond-> {:email/message-id   message-id
             :email/subject      (let [s (or (:subject msg) "")]
                                    (if (str/blank? s)
                                      "(no subject)"
                                      (-> s (str/replace #"\s+" " ") str/trim)))
             :email/content-type (:content-type msg)
             :email/ingested-at  (Date.)}

      id                          (assoc :email/id id)

      (:address from)             (assoc :email/from-address (:address from))
      (:name from)                (assoc :email/from-name (:name from))
      (:address (first reply-to)) (assoc :email/reply-to-address
                                         (:address (first reply-to)))
      (:name (first reply-to))    (assoc :email/reply-to-name
                                         (:name (first reply-to)))
      (:address author)           (assoc :email/author-address (:address author))
      (:name author)              (assoc :email/author-name (:name author))
      (seq (:to msg))             (assoc :email/to (set (map format-address (:to msg))))
      (seq (:cc msg))             (assoc :email/cc (set (map format-address (:cc msg))))
      (:date-sent msg)            (assoc :email/date-sent (:date-sent msg))
      (:date-received msg)        (assoc :email/date-received (:date-received msg))
      text                        (assoc :email/body-text text)
      html-body                   (assoc :email/body-html html-body)
      text-from-html              (assoc :email/body-text-from-html text-from-html)
      (seq (:flags msg))          (assoc :email/flags (:flags msg))
      (seq attachments)           (assoc :email/attachments attachments)
      in-reply-to                 (assoc :email/in-reply-to in-reply-to)
      references                  (assoc :email/references references)
      (seq ancestor-mids)         (assoc :email/ancestor-mids ancestor-mids)
      headers-edn                 (assoc :email/headers-edn headers-edn)))))

;; ---------------------------------------------------------------------------
;; Store
;; ---------------------------------------------------------------------------

(defn- truncate
  "Truncate string s to at most n characters."
  [s n]
  (when (string? s) (subs s 0 (min n (count s)))))

(defn store-email!
  "Store a parsed email.  Skips nil/oversized/duplicate Message-IDs.
  Returns true if stored.
  Opts: :source (stamps the email + scopes (source,id) dedup; required
  for live ingestion), :max-attachment-size (override 1 MB default)."
  ([conn msg] (store-email! conn msg {}))
  ([conn msg opts]
  (let [message-id (or (:message-id opts)
                       (common/extract-bracketed-id (:message-id msg)))
        id         (:id msg)
        src-name   (:source opts)]
    (cond
      (nil? message-id)
      (do (log/warn "Skipping email with nil Message-ID, id:" id) false)

      (not (common/indexable-mid? message-id))
      (do (log/warn "Skipping email with oversized Message-ID (" (count message-id)
                    "chars), id:" id "-- exceeds LMDB key limit") false)

      (entity-exists? conn :email/message-id message-id)
      (do (log/debug "Skipping already stored Message-ID:" message-id) false)

      (id-collision? conn src-name id)
      (do (log/warn "Skipping id collision:" id "on source" src-name
                    "-- different Message-ID but (source,id) already stored")
          false)

      :else
      (let [txdata (cond-> (email->txdata msg (assoc opts :message-id message-id))
                     src-name (assoc :email/source src-name))]
        (try
          (d/transact! conn [txdata])
          (log/info "Stored email id:" id
                    "Subject:" (truncate (:email/subject txdata) 60))
          true
          (catch Exception e
            ;; If the message-id now exists, another process (e.g. bb digest)
            ;; inserted it between our exists? check and the transact -- harmless race.
            (let [now-exists? (try (entity-exists? conn :email/message-id message-id)
                                  (catch Exception _ false))]
              (if now-exists?
                (do (log/debug "Duplicate Message-ID (race):" message-id) false)
                (throw e))))))))))

