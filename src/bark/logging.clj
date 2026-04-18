;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.logging
  "Shared logging utilities: size parsing, log rotation, file appender."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; ---------------------------------------------------------------------------
;; Exception message helper
;; ---------------------------------------------------------------------------

(defn exception-msg
  "Return `(.getMessage e)` when present, else the exception's class name.
  Avoids the `nil` that some exception types (NPE, AssertionError with no
  message) would otherwise render in log output."
  [^Throwable e]
  (or (.getMessage e) (str (class e))))

;; ---------------------------------------------------------------------------
;; Size parsing
;; ---------------------------------------------------------------------------

(defn parse-size
  "Parse a size string like \"10MB\" into bytes. Supports KB, MB, GB.
  Returns nil if the numeric part is not a valid integer."
  [s]
  (let [s (str/upper-case (str/trim (str s)))]
    (cond
      (str/ends-with? s "GB") (some-> (parse-long (str/replace s #"GB$" "")) (* 1024 1024 1024))
      (str/ends-with? s "MB") (some-> (parse-long (str/replace s #"MB$" "")) (* 1024 1024))
      (str/ends-with? s "KB") (some-> (parse-long (str/replace s #"KB$" "")) (* 1024))
      :else                   (parse-long s))))

;; ---------------------------------------------------------------------------
;; Log rotation
;; ---------------------------------------------------------------------------

(defn rotate-log!
  "Rotate log-file if it exceeds max-bytes, keeping up to backlog files."
  [log-file max-bytes backlog]
  (let [f (io/file log-file)]
    (when (and (.exists f) (> (.length f) max-bytes))
      (doseq [i (range (dec backlog) 0 -1)]
        (let [src (io/file (str log-file "." i))
              dst (io/file (str log-file "." (inc i)))]
          (when (.exists src) (.renameTo src dst))))
      (.renameTo f (io/file (str log-file ".1"))))))

;; ---------------------------------------------------------------------------
;; File appender
;; ---------------------------------------------------------------------------

(def ^:private file-log-lock (Object.))

(defn configure-file-logging!
  "If logging-cfg contains :file, add a Timbre file appender
  that persists logs at or above the specified :level."
  [{:keys [file level max-size backlog]
    :or   {level :warn max-size "10MB" backlog 5}}]
  (when file
    (io/make-parents file)
    (let [max-bytes (parse-size max-size)]
      (log/merge-config!
       {:appenders
        {:file
         {:enabled?  true
          :min-level level
          :fn        (fn [data]
                       (locking file-log-lock
                         (rotate-log! file max-bytes backlog)
                         (spit file
                               (str (force (:timestamp_ data)) " "
                                    (str/upper-case (name (:level data))) " "
                                    (:?ns-str data) " - "
                                    (force (:msg_ data)) "\n")
                               :append true)))}}}))))
