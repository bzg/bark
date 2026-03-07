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
  (d/get-conn db-path schema {:wal? false}))

(defn close [conn]
  (d/close conn))

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
  "Return the highest stored IMAP UID, or 0."
  [conn]
  (or (d/q '[:find ?uid .
             :where
             [?e :watermark/id "default"]
             [?e :watermark/imap-uid ?uid]]
           (d/db conn))
      0))

(defn save-imap-uid!
  "Update the IMAP watermark to the given UID."
  [conn imap-uid]
  (d/transact! conn [{:watermark/id       "default"
                      :watermark/imap-uid imap-uid}]))

(defn get-email
  "Pull a full email entity by entity ID."
  [conn eid]
  (d/pull (d/db conn) '[*] eid))
