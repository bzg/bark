;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark-ingest.db
  "Datalevin database setup, schema, and operations."
  (:require [datalevin.core :as d]
            [clojure.tools.logging :as log]))

;; ---------------------------------------------------------------------------
;; Schema
;; ---------------------------------------------------------------------------

(def schema
  {;; SHA-256 hash of "mailbox-id:imap-uid" — globally unique across mailboxes
   :email/uid            {:db/valueType :db.type/string
                          :db/unique    :db.unique/identity}

   ;; Raw IMAP UID (not unique — same value can appear in different mailboxes)
   :email/imap-uid       {:db/valueType :db.type/long}

   ;; Which mailbox this email was ingested from (e.g. "imap.example.com:user@example.com")
   :email/mailbox        {:db/valueType :db.type/string}

   ;; Message-ID header (RFC 2822)
   :email/message-id     {:db/valueType :db.type/string
                          :db/unique    :db.unique/identity}

   ;; Envelope
   :email/subject        {:db/valueType :db.type/string}
   :email/from-name      {:db/valueType :db.type/string}
   :email/from-address   {:db/valueType :db.type/string}
   :email/to             {:db/valueType :db.type/string
                          :db/cardinality :db.cardinality/many}
   :email/cc             {:db/valueType :db.type/string
                          :db/cardinality :db.cardinality/many}
   :email/date-sent      {:db/valueType :db.type/instant}
   :email/date-received  {:db/valueType :db.type/instant}

   ;; Body content
   :email/body-text      {:db/valueType :db.type/string}
   :email/body-html      {:db/valueType :db.type/string}

   ;; Plain text extracted from HTML (stripped tags)
   :email/body-text-from-html {:db/valueType :db.type/string}

   ;; Content type
   :email/content-type   {:db/valueType :db.type/string}

   ;; Flags as a string set
   :email/flags          {:db/valueType :db.type/keyword
                          :db/cardinality :db.cardinality/many}

   ;; Attachments stored as child entities
   :email/attachments    {:db/valueType   :db.type/ref
                          :db/cardinality :db.cardinality/many
                          :db/isComponent true}

   ;; Attachment attributes
   :attachment/filename     {:db/valueType :db.type/string}
   :attachment/content-type {:db/valueType :db.type/string}
   :attachment/size         {:db/valueType :db.type/long}

   ;; Threading headers
   :email/in-reply-to    {:db/valueType :db.type/string}
   :email/references     {:db/valueType :db.type/string
                          :db/cardinality :db.cardinality/many}

   ;; Raw headers as EDN string (for debugging / advanced use)
   :email/headers-edn    {:db/valueType :db.type/string}

   ;; Ingestion metadata
   :email/ingested-at    {:db/valueType :db.type/instant}

   ;; --- Per-mailbox watermark ---
   :watermark/mailbox    {:db/valueType :db.type/string
                          :db/unique    :db.unique/identity}
   :watermark/imap-uid   {:db/valueType :db.type/long}})

;; ---------------------------------------------------------------------------
;; Connection
;; ---------------------------------------------------------------------------

(defn connect
  "Open a Datalevin connection at the given path."
  [db-path]
  (log/info "Opening Datalevin database at" db-path)
  (d/get-conn db-path schema))

(defn close [conn]
  (d/close conn))

;; ---------------------------------------------------------------------------
;; Mailbox identity
;; ---------------------------------------------------------------------------

(defn mailbox-id
  "Compute a stable mailbox identifier from an IMAP config map."
  [imap-cfg]
  (str (:host imap-cfg) ":" (:user imap-cfg)))

;; ---------------------------------------------------------------------------
;; Queries
;; ---------------------------------------------------------------------------

(defn email-exists?
  "Check if an email with the given uid hash already exists."
  [conn uid-hash]
  (some? (d/q '[:find ?e .
                :in $ ?uid
                :where [?e :email/uid ?uid]]
              (d/db conn) uid-hash)))

(defn message-id-exists?
  "Check if an email with the given Message-ID already exists."
  [conn message-id]
  (when message-id
    (some? (d/q '[:find ?e .
                  :in $ ?mid
                  :where [?e :email/message-id ?mid]]
                (d/db conn) message-id))))

(defn max-imap-uid
  "Return the highest IMAP UID stored for a given mailbox, or 0."
  [conn mailbox-id]
  (or (d/q '[:find ?uid .
             :in $ ?mb
             :where
             [?e :watermark/mailbox ?mb]
             [?e :watermark/imap-uid ?uid]]
           (d/db conn) mailbox-id)
      0))

(defn save-imap-uid!
  "Update the per-mailbox watermark to the given IMAP UID."
  [conn mailbox-id imap-uid]
  (d/transact! conn [{:watermark/mailbox  mailbox-id
                      :watermark/imap-uid imap-uid}]))

(defn get-email
  "Pull a full email entity by entity ID."
  [conn eid]
  (d/pull (d/db conn) '[*] eid))
