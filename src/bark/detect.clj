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

(def compile-labels
  "Compile a type=>tags map into type=>regex.  Strict anchor `^[TAG]`
  or `^[TAG <inner>]`, case-sensitive tag, mandatory whitespace or EOL
  after `]`.  Mailing-list bracket prefixes (e.g. \"[my-list] [BUG]\")
  are tolerated."
  (memoize
   (fn [st]
     (update-vals st
                  (fn [tags]
                    (let [alts (str/join "|" (map #(java.util.regex.Pattern/quote %) tags))]
                      (re-pattern (str "^" ml-prefix "\\[(" alts ")(?:\\s+([^\\]]*))?\\](?=\\s|$)"))))))))

(defn resolve-labels
  "Compile labels for a source-map entry."
  [source-cfg]
  (compile-labels (common/resolve-labels-map source-cfg)))

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

;; Detection precedence comes from `common/report-type-spec`; we only
;; need the per-row :type and :versioned? fields here (the lookup key
;; into the compiled patterns map equals :type).

(defn- detect-tag-with-topic
  "Non-versioned subject tag: the inner text (if any) becomes the topic,
  falling back to the colon-based topic after the closing bracket."
  [rtype subject inner]
  (let [topic (or inner (extract-colon-topic subject))]
    (cond-> {:type rtype} topic (assoc :topic topic))))

(defn- detect-versioned-tag
  "Versioned subject tag (\"[REL]\", \"[CHG]\"): the last space-separated token
  in the bracket is the version; preceding tokens form the topic,
  again falling back to the colon-based topic."
  [rtype subject inner]
  (let [tokens       (when inner (str/split inner #"\s+"))
        version      (when (seq tokens) (last tokens))
        topic-tokens (when (> (count tokens) 1) (butlast tokens))
        topic        (or (when (seq topic-tokens) (str/join " " topic-tokens))
                         (extract-colon-topic subject))]
    (cond-> {:type rtype}
      version (assoc :version version)
      topic   (assoc :topic topic))))

(defn- detect-simple-tag
  "Detect a report type from a subject tag.  Dispatches to the versioned
  or topic-only parser based on `versioned?`."
  [rtype subject pattern versioned?]
  (when-let [m (re-find pattern subject)]
    (let [inner (extract-inner m)]
      (if versioned?
        (detect-versioned-tag rtype subject inner)
        (detect-tag-with-topic rtype subject inner)))))

;; Re: only.  Fwd: stays untouched -- forwards are cross-postings,
;; not v2/v3 submissions.
(def ^:private reply-prefix-re #"(?i)^(?:Re:\s*)+")

(defn detect-patch-subject
  "Match [PATCH] in a subject (strict, no Re: stripping)."
  [subject patterns]
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

;; ICS / calendar detection -- delegates to common for JVM+bb consistency.

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

(defn has-patch-content?
  "True if an email carries patch content (attachment or inline diff)."
  [email]
  (or (has-patch-attachment? (:email/attachments email))
      (has-inline-patch? (common/email-body-text email))))

(def ^:private format-patch-start #"(?m)^From [0-9a-f]{40} ")

(defn parse-format-patch-headers
  "Parse `git format-patch` headers (`From`, `Subject`, `Date`) from
  patch `text`.  Returns nil when `text` lacks the `^From <sha40>`
  signature, else a map with the headers found (possibly empty)."
  [text]
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

(defn format-patch-submission?
  "True when the email ships an attachment whose contents look like a
  real `git format-patch` output (has the `From <sha40>` signature)
  AND whose internal `Subject:` header starts with `[PATCH]`.
  Distinguishes a serious submission from a quick `git diff` debug
  patch or quoted content -- the in-thread escape hatch for the
  label-first rule."
  [email]
  (boolean
   (some (fn [att]
           (when (and (common/patch-file? (:attachment/filename att))
                      (:attachment/data att))
             (when-let [subj (:subject (parse-format-patch-headers
                                        (:attachment/data att)))]
               (re-find #"^\s*\[PATCH(\s|\])" subj))))
         (:email/attachments email))))

(defn detect-report
  "Detect a report's type from an email.  Priority:
    1. Subject label (strict regex, anchored at start) -- authoritative.
    2. `Re: [PATCH]` reply WITH patch content (v2/v3 workflow).
    3. Real `git format-patch` attachment (inner Subject: [PATCH]) --
       creates a :patch even in a reply, escape hatch for the label-
       first rule when the patch is clearly meant to be applied.
    4. Patch content alone, only on fresh threads (no In-Reply-To)."
  ([email] (detect-report email (compile-labels common/default-labels) nil))
  ([email patterns] (detect-report email patterns nil))
  ([email patterns allowed-types]
   (let [allowed?       (fn [result]
                          (when (and result
                                     (or (nil? allowed-types)
                                         (contains? allowed-types (:type result))))
                            result))
         subject        (:email/subject email)
         attachments    (:email/attachments email)
         body-text      (common/email-body-text email)
         in-reply-to    (:email/in-reply-to email)
         attachment?    (has-patch-attachment? attachments)
         inline?        (has-inline-patch? body-text)
         has-patch?     (or attachment? inline?)]
     (or
      ;; 1. Strict subject tag walk -- explicit label is authoritative
      (when subject
        (some (fn [{:keys [type versioned?]}]
                (when-let [pattern (get patterns type)]
                  (allowed?
                   (if (= type :patch)
                     (detect-patch-subject subject patterns)
                     (detect-simple-tag type subject pattern versioned?)))))
              common/report-type-spec))
      ;; 2. Re: [PATCH] reply WITH content (a bare discussion does not qualify).
      (when (and subject has-patch?)
        (let [stripped (str/replace-first subject reply-prefix-re "")]
          (when (not= stripped subject)
            (allowed? (detect-patch-subject stripped patterns)))))
      ;; 3. Real git format-patch attachment -- escape hatch for replies
      ;; where the outer subject doesn't carry [PATCH].
      (when (format-patch-submission? email)
        (allowed? {:type :patch :patch-source #{:attachment}}))
      ;; 4. Fallback (no label, fresh thread).
      (when (and has-patch? (nil? in-reply-to))
        (allowed? {:type :patch
                   :patch-source (cond-> #{}
                                   attachment? (conj :attachment)
                                   inline?     (conj :inline))}))))))

;; ---------------------------------------------------------------------------
;; Patch content extraction (pure)
;; ---------------------------------------------------------------------------

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
    (into (vec inline) att-patches)))
