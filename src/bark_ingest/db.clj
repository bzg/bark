;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark-ingest.db
  "Datalevin database setup, schema, and operations."
  (:require [datalevin.core :as d]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]))

;; ---------------------------------------------------------------------------
;; Schema
;; ---------------------------------------------------------------------------

(def schema
  (edn/read-string (slurp (io/resource "bark-schema.edn"))))

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
