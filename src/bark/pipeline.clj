;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.pipeline
  "Atomic store-and-process pipeline.
  Each email is stored then immediately digested.
  Duplicates are silently skipped at both layers."
  (:require [datalevin.core :as d]
            [taoensso.timbre :as log]
            [bark.ingest :as ingest]
            [bark.digest :as digest]))

(defn store-and-process!
  "Store an email and immediately process it.
  Skips silently if the email is already stored (by message-id or UID).
  Skips with a warning if the email exceeds max-size bytes.
  Returns true if the email was stored and processed, false otherwise."
  [db-conn source-map sources msg {:keys [max-size]}]
  (let [size (:size msg -1)]
    (if (and max-size (pos? size) (> size max-size))
      (do (log/warn "Skipping oversized email UID:" (:uid msg)
                    "size:" size "bytes (max:" max-size ")")
          false)
      (when (ingest/store-email! db-conn msg)
        (let [mid   (:message-id msg)
              eid   (d/q '[:find ?e . :in $ ?mid :where [?e :email/message-id ?mid]]
                         (d/db db-conn) mid)
              email (d/pull (d/db db-conn) digest/email-pull-pattern eid)]
          (try
            (digest/process-email! db-conn source-map sources email)
            true
            (catch Exception e
              (log/error e "Failed to digest email" mid
                         (or (.getMessage e) (str (class e))))
              false)))))))
