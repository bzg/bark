;; Copyright (c) 2026 Bastien Guerry <bzg@gnu.org>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bark.detect
  "Report type detection and label compilation. All functions are pure."
  (:require [clojure.string :as str]
            [bark.common :as common]))

;; ---------------------------------------------------------------------------
;; Label compilation
;; ---------------------------------------------------------------------------

(def ^:private ml-prefix "(?:\\[[^\\]]*\\]\\s*)*")

(defn compile-labels [st]
  (update-vals st
               (fn [tags]
                 (let [alts (str/join "|" (map #(java.util.regex.Pattern/quote %) tags))]
                   (re-pattern (str "(?i)^" ml-prefix "\\[(" alts ")(?:\\s+([^\\]]*))?\\]"))))))

(def default-compiled-labels (compile-labels common/default-labels))

(defn resolve-labels
  "Compile labels for a source-map entry."
  [source-cfg]
  (let [merged (common/resolve-labels-map source-cfg)]
    (if (= merged common/default-labels)
      default-compiled-labels
      (compile-labels merged))))

;; ---------------------------------------------------------------------------
;; Colon-based topic extraction
;; ---------------------------------------------------------------------------

(def ^:private post-bracket-re #".*\]\s*(.*)")

(defn- extract-colon-topic [subject]
  (when-let [[_ rest] (re-find post-bracket-re subject)]
    (let [parts (str/split rest #":" -1)]
      (when (> (count parts) 1)
        (let [topic (str/trim (first parts))]
          (when-not (str/blank? topic) topic))))))

;; ---------------------------------------------------------------------------
;; Report detection (pure, table-driven)
;; ---------------------------------------------------------------------------

(def patch-seq-pattern #"(\d+/\d+)\s*$")
(def ^:private patch-version-pattern #"v\d+")

(defn- extract-inner [m]
  (when-let [s (nth m 2 nil)]
    (let [t (str/trim s)]
      (when-not (str/blank? t) t))))

;; Detection table: each entry describes how to parse a subject tag.
;; :key       — lookup in compiled patterns map
;; :type      — report type keyword
;; :versioned — extract last token as :version
;; :special   — :patch triggers special patch-subject parsing
(def ^:private detection-table
  [{:key :bug     :type :bug}
   {:key :patch   :type :patch   :special :patch}
   {:key :request :type :request}
   {:key :announcement :type :announcement}
   {:key :release :type :release :versioned true}
   {:key :change  :type :change  :versioned true}])

(defn- detect-simple-tag
  "Detect a report type from a subject tag. Handles topic extraction
  and optional version parsing for :versioned types."
  [rtype subject pattern versioned?]
  (when-let [m (re-find pattern subject)]
    (let [inner (extract-inner m)]
      (if versioned?
        (let [tokens (when inner (str/split inner #"\s+"))
              version (when (seq tokens) (last tokens))
              topic-tokens (when (> (count tokens) 1) (butlast tokens))
              topic (or (when (seq topic-tokens) (str/join " " topic-tokens))
                        (extract-colon-topic subject))]
          (cond-> {:type rtype}
            version (assoc :version version)
            topic   (assoc :topic topic)))
        (let [topic (or inner (extract-colon-topic subject))]
          (cond-> {:type rtype} topic (assoc :topic topic)))))))

(defn detect-patch-subject [subject patterns]
  (when subject
    (when-let [m (re-find (:patch patterns) subject)]
      (let [inner   (extract-inner m)
            seq-m   (when inner (re-find patch-seq-pattern inner))
            seq-str (when seq-m (first seq-m))
            rest    (when inner
                      (str/trim (if seq-str
                                  (subs inner 0 (- (count inner) (count seq-str)))
                                  inner)))
            tokens  (when (and rest (not (str/blank? rest))) (str/split rest #"\s+"))
            version (when (and tokens (re-matches patch-version-pattern (last tokens)))
                      (last tokens))
            topic-tokens (if version (butlast tokens) tokens)
            topic   (or (when (seq topic-tokens) (str/join " " topic-tokens))
                        (extract-colon-topic subject))]
        (cond-> {:type :patch :patch-source #{:subject}}
          seq-str (assoc :patch-seq seq-str)
          version (assoc :version version)
          topic   (assoc :topic topic))))))

;; Attachment & inline patch detection

(defn has-patch-attachment? [attachments]
  (some (fn [att] (common/patch-file? (:attachment/filename att))) attachments))

;; ICS / calendar detection — delegates to common for JVM+bb consistency.

(defn has-ics?
  "True if an email has ICS content (attached or inline)."
  [email]
  (or (common/has-ics-attachment? (:email/attachments email))
      (common/has-inline-ics? (common/email-body-text email))))

(def inline-patch-indicators
  [#"(?m)^diff --git " #"(?m)^--- a/" #"(?m)^\+\+\+ b/"
   #"(?m)^@@ [-+]\d+" #"(?m)^index [0-9a-f]+\.\.[0-9a-f]+"])

(defn has-inline-patch? [body-text]
  (when body-text (>= (count (filter #(re-find % body-text) inline-patch-indicators)) 2)))

(defn detect-patch [subject attachments body-text patterns]
  (let [from-subject    (detect-patch-subject subject patterns)
        from-attachment (when (has-patch-attachment? attachments) :attachment)
        from-inline     (when (has-inline-patch? body-text) :inline)
        sources         (cond-> #{}
                          from-subject    (into (:patch-source from-subject))
                          from-attachment (conj :attachment)
                          from-inline     (conj :inline))]
    (when (seq sources)
      (cond-> {:type :patch :patch-source sources}
        (:patch-seq from-subject) (assoc :patch-seq (:patch-seq from-subject))
        (:version from-subject)   (assoc :version (:version from-subject))
        (:topic from-subject)     (assoc :topic (:topic from-subject))))))

(defn detect-report
  "Detect report type from an email.

  Priority rules:
    1. Attachment or inline patch content ALWAYS wins, even if the subject
       carries another tag like [BUG].  A [BUG]-tagged email with a .patch
       attachment becomes a :patch report, not a :bug.
    2. Otherwise walks detection-table in order; first matching subject tag wins.

  Returns a report-info map or nil."
  ([email] (detect-report email default-compiled-labels nil))
  ([email patterns] (detect-report email patterns nil))
  ([email patterns allowed-types]
   (let [allowed?    (fn [result]
                       (when (and result
                                  (or (nil? allowed-types)
                                      (contains? allowed-types (:type result))))
                         result))
         subject     (:email/subject email)
         attachments (:email/attachments email)
         body-text   (common/email-body-text email)
         has-patch?  (or (has-patch-attachment? attachments)
                         (has-inline-patch? body-text))]
     (or
      ;; 1. Attachment or inline patch content → always a patch
      (when has-patch?
        (allowed? (detect-patch subject attachments body-text patterns)))
      ;; 2. Subject tag walk (no patch content present)
      (when subject
        (some (fn [{:keys [key type versioned special]}]
                (when-let [pattern (get patterns key)]
                  (allowed?
                   (if (= special :patch)
                     ;; [PATCH] in subject but no attachment/inline — subject-only patch
                     (detect-patch-subject subject patterns)
                     (detect-simple-tag type subject pattern versioned)))))
              detection-table))))))

;; ---------------------------------------------------------------------------
;; Patch content extraction (pure)
;; ---------------------------------------------------------------------------

(def ^:private format-patch-start #"(?m)^From [0-9a-f]{40} ")

(defn parse-format-patch-headers [text]
  (when (and text (re-find format-patch-start text))
    (let [lines (str/split-lines text)
          header-lines (rest lines)
          headers (loop [hs {} last-k nil [line & more] header-lines]
                    (cond
                      (nil? line)          hs
                      (str/blank? line)    hs
                      (re-matches #"^\s+.*" line)
                      (recur (if last-k (update hs last-k str " " (str/trim line)) hs)
                             last-k more)
                      :else
                      (let [[_ k v] (re-find #"^([^:]+):\s*(.*)" line)]
                        (if k
                          (let [lk (str/lower-case k)]
                            (recur (assoc hs lk (str/trim v)) lk more))
                          (recur hs last-k more)))))]
      (cond-> {}
        (get headers "from")    (assoc :author  (get headers "from"))
        (get headers "subject") (assoc :subject (get headers "subject"))
        (get headers "date")    (assoc :date    (get headers "date"))))))

(def ^:private inline-patch-start-patterns
  [#"^From [0-9a-f]{40} " #"^diff --git " #"^--- a/"])

(defn extract-inline-patch [body-text]
  (when body-text
    (let [lines (str/split-lines body-text)
          start (some (fn [[i line]]
                        (when (some #(re-find % line) inline-patch-start-patterns)
                          i))
                      (map-indexed vector lines))]
      (when start
        (str/join "\n" (subvec (vec lines) start))))))

(defn- patch-entity [filename source text]
  (let [fp-meta (parse-format-patch-headers text)]
    (cond-> {:patch/filename filename :patch/source source :patch/text text}
      (:author fp-meta)  (assoc :patch/author  (:author fp-meta))
      (:subject fp-meta) (assoc :patch/subject (:subject fp-meta))
      (:date fp-meta)    (assoc :patch/date    (:date fp-meta)))))

(defn build-patch-entities [email]
  (let [body-text   (common/email-body-text email)
        attachments (:email/attachments email)
        inline      (when-let [text (extract-inline-patch body-text)]
                      [(patch-entity "inline.patch" :inline text)])
        att-patches (->> attachments
                         (filter #(and (common/patch-file? (:attachment/filename %))
                                       (:attachment/data %)))
                         (mapv #(patch-entity (:attachment/filename %) :attachment (:attachment/data %))))]
    (into (or inline []) att-patches)))
