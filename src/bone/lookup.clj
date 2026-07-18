;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bone.lookup
  "Mid-keyed entity resolution, always through d/entid on the unique
  fixed-length hash attrs (:email/message-id-hash,
  :report/message-id-hash).

  The only sanctioned way to resolve a Message-Id to an entity: a d/q
  that seeks or scans a hash value can hit a Datalevin index bug
  (MDB_BAD_VALSIZE), so lookups stay on the entid/pull path and joins
  bind resolved eids.  Babashka scripts talk to Datalevin through the
  pod and cannot require this namespace; they repeat the same
  convention inline."
  (:require [datalevin.core :as d]
            [bone.common :as common]))

(defn email-eid-by-hash
  "Eid of the stored email whose mid-hash is `h`, or nil."
  [db h]
  (when h (d/entid db [:email/message-id-hash h])))

(defn report-eid-by-hash
  "Eid of the report whose root mid-hash is `h`, or nil."
  [db h]
  (when h (d/entid db [:report/message-id-hash h])))

(defn email-eid
  "Eid of the stored email with Message-Id `mid`, or nil."
  [db mid]
  (email-eid-by-hash db (common/mid-hash mid)))

(defn report-eid
  "Eid of the report whose root Message-Id is `mid`, or nil."
  [db mid]
  (report-eid-by-hash db (common/mid-hash mid)))
