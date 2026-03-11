;; bark-detect.clj — Report type detection and label compilation.
;;
;; All functions are pure (no DB access, no side effects).
;;
;; Usage: (load-file "scripts/bark-detect.clj")

(require '[clojure.string :as str])

;; ---------------------------------------------------------------------------
;; Label defaults and compilation
;; ---------------------------------------------------------------------------

;; Mailing list managers may prepend "[listname] " or similar bracketed
;; prefixes to the subject.  The bark-specific tag is always the last
;; bracketed construct, so we skip zero or more leading "[...] " groups.
(def ^:private ml-prefix "(?:\\[[^\\]]*\\]\\s*)*")

(def default-labels
  "Default subject tags per report type.
  All types accept an optional inner text after the tag:
    [BUG topic], [PATCH topic v2 1/3], [REL topic version], etc."
  {:bug          ["BUG"]
   :patch        ["PATCH"]
   :request      ["POLL" "FR" "TODO"]
   :announcement ["ANN" "ANNOUNCEMENT"]
   :release      ["REL" "RELEASE"]
   :change       ["CHG" "CHANGE"]})

(defn compile-labels
  "Compile a labels map into a map of type -> regex pattern.
  All types allow an optional suffix inside the brackets."
  [st]
  (into {}
        (map (fn [[rtype tags]]
               (let [alts (str/join "|" (map #(java.util.regex.Pattern/quote %) tags))
                     pat  (re-pattern (str "(?i)^" ml-prefix "\\[(" alts ")(?:\\s+([^\\]]*))?\\]"))]
                 [rtype pat])))
        st))

(def default-compiled-labels
  (compile-labels default-labels))

(defn resolve-labels
  "Compile labels for a source-map entry.
  Merges global (:global-labels) and per-source (:labels)
  overrides on top of defaults. Returns compiled patterns."
  [source-cfg]
  (let [global  (:global-labels source-cfg)
        per-src (:labels source-cfg)
        merged  (cond
                  (and global per-src) (merge default-labels global per-src)
                  global              (merge default-labels global)
                  per-src             (merge default-labels per-src)
                  :else               nil)]
    (if merged
      (compile-labels merged)
      default-compiled-labels)))

;; ---------------------------------------------------------------------------
;; Colon-based topic extraction from subject (fallback)
;; ---------------------------------------------------------------------------

(def ^:private post-bracket-re
  "Captures text after the last ']' in the subject."
  #"(?i)\]\s*(.*)")

(defn- extract-colon-topic
  "Extract topic from the post-bracket portion of a subject.
  E.g. 'parser: crash on empty input' -> 'parser'.
  E.g. 'auth: oauth: fix refresh' -> 'auth: oauth'.
  Returns nil if no colon-delimited prefix is found."
  [subject]
  (when-let [[_ rest] (re-find post-bracket-re subject)]
    (let [parts (str/split rest #":" -1)]
      (when (> (count parts) 1)
        (let [topic (str/trim (str/join ":" (butlast parts)))]
          (when-not (str/blank? topic) topic))))))

;; ---------------------------------------------------------------------------
;; Report detection (pure)
;; ---------------------------------------------------------------------------

(def patch-seq-pattern #"(\d+/\d+)\s*$")
(def ^:private patch-version-pattern #"v\d+")

(defn- extract-inner
  "Extract the optional inner text from a regex match (group 2)."
  [m]
  (when-let [s (nth m 2 nil)]
    (let [t (str/trim s)]
      (when-not (str/blank? t) t))))

(defn detect-bug [subject patterns]
  (when-let [m (re-find (:bug patterns) subject)]
    (let [inner (extract-inner m)
          topic (or inner (extract-colon-topic subject))]
      (cond-> {:type :bug}
        topic (assoc :topic topic)))))

(defn detect-patch-subject [subject patterns]
  (when-let [m (re-find (:patch patterns) subject)]
    (let [inner   (extract-inner m)
          ;; Strip trailing N/M sequence marker
          seq-m   (when inner (re-find patch-seq-pattern inner))
          seq-str (when seq-m (first seq-m))
          rest    (when inner
                    (str/trim (if seq-str
                                (subs inner 0 (- (count inner) (count seq-str)))
                                inner)))
          ;; Split remaining tokens: last v\d+ is version, rest is topic
          tokens  (when (and rest (not (str/blank? rest)))
                    (str/split rest #"\s+"))
          version (when (and tokens (re-matches patch-version-pattern (last tokens)))
                    (last tokens))
          topic-tokens (if version (butlast tokens) tokens)
          topic   (when (seq topic-tokens)
                    (str/join " " topic-tokens))
          ;; Fallback to colon topic
          topic   (or topic (extract-colon-topic subject))]
      (cond-> {:type :patch :patch-source #{:subject}}
        seq-str (assoc :patch-seq seq-str)
        version (assoc :version version)
        topic   (assoc :topic topic)))))

(defn detect-request [subject patterns]
  (when-let [m (re-find (:request patterns) subject)]
    (let [inner (extract-inner m)
          topic (or inner (extract-colon-topic subject))]
      (cond-> {:type :request}
        topic (assoc :topic topic)))))

(defn detect-announcement [subject patterns]
  (when-let [m (re-find (:announcement patterns) subject)]
    (let [inner (extract-inner m)
          topic (or inner (extract-colon-topic subject))]
      (cond-> {:type :announcement}
        topic (assoc :topic topic)))))

(defn- detect-versioned-tag
  "Detect a report type where the bracket inner text is [topic...] version.
  Last token is always version; preceding tokens form topic."
  [rtype pattern subject]
  (when-let [m (re-find pattern subject)]
    (let [inner  (extract-inner m)
          tokens (when inner (str/split inner #"\s+"))
          version (when (seq tokens) (last tokens))
          topic-tokens (when (> (count tokens) 1) (butlast tokens))
          topic   (or (when (seq topic-tokens) (str/join " " topic-tokens))
                      (extract-colon-topic subject))]
      (cond-> {:type rtype}
        version (assoc :version version)
        topic   (assoc :topic topic)))))

(defn detect-release [subject patterns]
  (detect-versioned-tag :release (:release patterns) subject))

(defn detect-change [subject patterns]
  (detect-versioned-tag :change (:change patterns) subject))

;; Attachment & inline patch detection

(defn has-patch-attachment? [attachments]
  (some (fn [att] (patch-file? (:attachment/filename att)))
        attachments))

(def inline-patch-indicators
  [#"(?m)^diff --git " #"(?m)^--- a/" #"(?m)^\+\+\+ b/"
   #"(?m)^@@ [-+]\d+" #"(?m)^index [0-9a-f]+\.\.[0-9a-f]+"])

(defn has-inline-patch? [body-text]
  (when body-text (>= (count (filter #(re-find % body-text) inline-patch-indicators)) 2)))

(defn detect-patch [subject attachments body-text patterns]
  (let [from-subject    (detect-patch-subject subject patterns)
        from-attachment (when (has-patch-attachment? attachments) :attachment)
        from-inline     (when (has-inline-patch? body-text) :inline)
        ;; A subject tag alone is only sufficient for cover letters (0/M).
        ;; For all other cases, require actual patch content.
        cover-letter?   (when-let [s (:patch-seq from-subject)]
                          (str/starts-with? s "0/"))
        subject-only?   (and from-subject (not from-attachment) (not from-inline))
        sources         (cond-> #{}
                          (and from-subject (or (not subject-only?) cover-letter?))
                          (into (:patch-source from-subject))
                          from-attachment (conj :attachment)
                          from-inline     (conj :inline))]
    (when (seq sources)
      (cond-> {:type :patch :patch-source sources}
        (:patch-seq from-subject) (assoc :patch-seq (:patch-seq from-subject))
        (:version from-subject)   (assoc :version (:version from-subject))
        (:topic from-subject)     (assoc :topic (:topic from-subject))))))

(defn detect-report
  "Detect report type from an email. Returns a map with :type and optional
  :version, :topic, :patch-seq, :patch-source — or nil if no report detected."
  ([email] (detect-report email default-compiled-labels))
  ([email patterns]
   (when-let [subject (:email/subject email)]
     (let [attachments (:email/attachments email)
           body-text   (email-body-text email)]
       (or (detect-bug subject patterns)
           (detect-patch subject attachments body-text patterns)
           (detect-request subject patterns)
           (detect-announcement subject patterns)
           (detect-release subject patterns)
           (detect-change subject patterns))))))

;; ---------------------------------------------------------------------------
;; Patch content extraction (pure)
;; ---------------------------------------------------------------------------

(def ^:private format-patch-start #"(?m)^From [0-9a-f]{40} ")

(defn parse-format-patch-headers
  "Extract author, subject, date from a git format-patch output.
  Returns a map with :author, :subject, :date (all optional)."
  [text]
  (when (and text (re-find format-patch-start text))
    (let [lines  (str/split-lines text)
          ;; Skip the 'From <hash> ...' line, parse RFC 822 headers until blank line
          header-lines (rest lines)
          headers (loop [hs {} [line & more] header-lines]
                    (cond
                      (nil? line)          hs
                      (str/blank? line)    hs
                      ;; Continuation line (starts with whitespace)
                      (re-matches #"^\s+.*" line)
                      (let [last-k (:_last-key hs)]
                        (recur (if last-k
                                 (update hs last-k str " " (str/trim line))
                                 hs)
                               more))
                      ;; Header line
                      :else
                      (let [[_ k v] (re-find #"^([^:]+):\s*(.*)" line)]
                        (if k
                          (let [lk (str/lower-case k)]
                            (recur (-> hs
                                       (assoc lk (str/trim v))
                                       (assoc :_last-key lk))
                                   more))
                          (recur hs more)))))]
      (cond-> {}
        (get headers "from")    (assoc :author  (get headers "from"))
        (get headers "subject") (assoc :subject (get headers "subject"))
        (get headers "date")    (assoc :date    (get headers "date"))))))

(defn extract-inline-patch
  "Extract inline diff/patch text from an email body.
  Returns the text from the first 'From <hash>' or 'diff --git' line
  to the end, or nil if no inline patch found."
  [body-text]
  (when body-text
    (let [lines (str/split-lines body-text)
          start (some (fn [[i line]]
                        (when (or (re-find #"^From [0-9a-f]{40} " line)
                                  (re-find #"^diff --git " line)
                                  (re-find #"^--- a/" line))
                          i))
                      (map-indexed vector lines))]
      (when start
        (str/join "\n" (subvec (vec lines) start))))))

(defn build-patch-entities
  "Build patch entity maps from an email's inline content and attachments.
  Returns a vector of maps suitable for :report/patches."
  [email]
  (let [body-text   (email-body-text email)
        attachments (:email/attachments email)
        ;; Inline patch
        inline      (when-let [text (extract-inline-patch body-text)]
                      (let [fp-meta (parse-format-patch-headers text)]
                        [(cond-> {:patch/filename "inline.patch"
                                  :patch/source   :inline
                                  :patch/text     text}
                           (:author fp-meta)  (assoc :patch/author  (:author fp-meta))
                           (:subject fp-meta) (assoc :patch/subject (:subject fp-meta))
                           (:date fp-meta)    (assoc :patch/date    (:date fp-meta)))]))
        ;; Attachment patches
        att-patches (when (seq attachments)
                      (->> attachments
                           (filter (fn [att]
                                     (and (patch-file? (:attachment/filename att))
                                          (:attachment/data att))))
                           (mapv (fn [att]
                                   (let [text    (:attachment/data att)
                                         fp-meta (parse-format-patch-headers text)]
                                     (cond-> {:patch/filename (:attachment/filename att)
                                              :patch/source   :attachment
                                              :patch/text     text}
                                       (:author fp-meta)  (assoc :patch/author  (:author fp-meta))
                                       (:subject fp-meta) (assoc :patch/subject (:subject fp-meta))
                                       (:date fp-meta)    (assoc :patch/date    (:date fp-meta))))))))]
    (vec (concat inline att-patches))))
