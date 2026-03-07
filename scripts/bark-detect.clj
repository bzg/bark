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
  :bug/:patch/:release/:change accept a version suffix after the tag.
  :request/:announcement match any of the listed tags exactly."
  {:bug          ["BUG"]
   :patch        ["PATCH"]
   :request      ["POLL" "FR" "TODO"]
   :announcement ["ANN" "ANNOUNCEMENT"]
   :release      ["REL" "RELEASE"]
   :change       ["CHG" "CHANGE"]})

(defn compile-labels
  "Compile a labels map into a map of type → regex pattern.
  Tags for :bug/:patch/:release/:change allow an optional version suffix.
  Tags for :request/:announcement match the tag exactly (no suffix)."
  [st]
  (let [versioned? #{:bug :patch :release :change}]
    (into {}
          (map (fn [[rtype tags]]
                 (let [alts (str/join "|" (map #(java.util.regex.Pattern/quote %) tags))
                       pat  (if (versioned? rtype)
                              (re-pattern (str "(?i)^" ml-prefix "\\[(" alts ")(?:\\s+([^\\]]*))?\\]"))
                              (re-pattern (str "(?i)^" ml-prefix "\\[(" alts ")\\]")))]
                   [rtype pat])))
          st)))

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
;; Report detection (pure)
;; ---------------------------------------------------------------------------

(def patch-seq-pattern #"(\d+/\d+)\s*$")

(defn detect-bug [subject patterns]
  (when-let [m (re-find (:bug patterns) subject)]
    {:type :bug :version (when (nth m 2 nil) (str/trim (nth m 2)))}))

(defn detect-patch-subject [subject patterns]
  (when-let [m (re-find (:patch patterns) subject)]
    (let [inner   (when (nth m 2 nil) (str/trim (nth m 2)))
          seq-m   (when inner (re-find patch-seq-pattern inner))
          seq-str (when seq-m (first seq-m))
          topic   (when inner
                    (let [t (if seq-str
                              (str/trim (subs inner 0 (- (count inner) (count seq-str))))
                              inner)]
                      (when-not (str/blank? t) t)))]
      (cond-> {:type :patch :patch-source #{:subject}}
        seq-str (assoc :patch-seq seq-str)
        topic   (assoc :topic topic)))))

(defn detect-request [subject patterns] (when (re-find (:request patterns) subject) {:type :request}))
(defn detect-announcement [subject patterns] (when (re-find (:announcement patterns) subject) {:type :announcement}))

(defn- detect-versioned-tag [pattern type subject]
  (when-let [m (re-find pattern subject)]
    (let [ver (nth m 2 nil)]
      (cond-> {:type type}
        (and ver (not (str/blank? ver))) (assoc :version (str/trim ver))))))

(defn detect-release [subject patterns] (detect-versioned-tag (:release patterns) :release subject))
(defn detect-change [subject patterns] (detect-versioned-tag (:change patterns) :change subject))

;; Attachment & inline patch detection

(def patch-filename-pattern #"(?i)\.(patch|diff)$")

(defn has-patch-attachment? [attachments]
  (some (fn [att] (when-let [f (:attachment/filename att)] (re-find patch-filename-pattern f)))
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
        (:topic from-subject)     (assoc :topic (:topic from-subject))))))

(defn detect-report
  "Detect report type from an email. Returns a map with :type and optional
  :version, :topic, :patch-seq, :patch-source — or nil if no report detected."
  ([email] (detect-report email default-compiled-labels))
  ([email patterns]
   (when-let [subject (:email/subject email)]
     (let [attachments (:email/attachments email)
           body-text   (or (:email/body-text email) (:email/body-text-from-html email))]
       (or (detect-bug subject patterns)
           (detect-patch subject attachments body-text patterns)
           (detect-request subject patterns)
           (detect-announcement subject patterns)
           (detect-release subject patterns)
           (detect-change subject patterns))))))
