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
  {;; IMAP UID — unique per mailbox, used for deduplication
   :email/uid            {:db/valueType :db.type/long
                          :db/unique    :db.unique/identity}

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

   ;; Ingestion metadata
   :email/ingested-at    {:db/valueType :db.type/instant}})

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
;; Queries
;; ---------------------------------------------------------------------------

(defn email-exists?
  "Check if an email with the given UID already exists in the database."
  [conn uid]
  (some? (d/q '[:find ?e .
                :in $ ?uid
                :where [?e :email/uid ?uid]]
              (d/db conn) uid)))

(defn max-uid
  "Return the maximum UID stored, or 0 if the database is empty."
  [conn]
  (or (d/q '[:find (max ?uid) .
             :where [_ :email/uid ?uid]]
           (d/db conn))
      0))

(defn get-email
  "Pull a full email entity by entity ID."
  [conn eid]
  (d/pull (d/db conn) '[*] eid))
