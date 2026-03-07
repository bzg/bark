;; bark-common.clj — Shared utilities for bark bb scripts.
;;
;; Usage: (load-file "scripts/bark-common.clj")

(require '[clojure.string :as str]
         '[clojure.edn :as edn])

;; ---------------------------------------------------------------------------
;; Canonical report pull pattern (shared by export, notify, stats)
;; ---------------------------------------------------------------------------

(def report-pull-pattern
  '[:db/id :report/type :report/version :report/topic
    :report/patch-seq :report/patch-source :report/message-id
    {:report/acked [:email/from-address]}
    {:report/owned [:email/from-address]}
    {:report/closed [:email/from-address :email/date-sent]}
    :report/urgent :report/important
    :report/votes-up :report/votes-down
    :report/descendants :report/digested-at
    {:report/related [:report/type :report/message-id
                      {:report/email [:email/headers-edn]}]}
    {:report/series [:series/id :series/expected :series/closed
                     {:series/patches [:db/id]}
                     {:series/cover-letter [:email/message-id]}]}
    {:report/email [:email/subject :email/from-address :email/from-name
                    :email/date-sent :email/source :email/imap-uid
                    :email/headers-edn]}])

(defn load-config
  "Load config.edn if it exists, or nil."
  []
  (let [f (clojure.java.io/file "config.edn")]
    (when (.exists f) (edn/read-string (slurp f)))))

(defn get-header
  "Case-insensitive header lookup. headers-edn can be an EDN string or
  an already-parsed map. Returns nil on parse failure."
  [headers-edn header-name]
  (when headers-edn
    (try
      (let [headers (if (string? headers-edn) (edn/read-string headers-edn) headers-edn)
            lname   (str/lower-case header-name)]
        (some (fn [[k v]] (when (= (str/lower-case k) lname) v)) headers))
      (catch Exception _ nil))))

(defn extract-list-id
  "Extract the identifier from a List-Id header value.
  RFC 2919: \"Description <list-id>\" → \"list-id\".
  Returns the content inside angle brackets, or the raw value if none found."
  [raw]
  (when raw
    (if-let [[_ id] (re-find #"<([^>]+)>" (str raw))]
      id
      (str raw))))

(defn- match-source?
  "Check if headers match a source's :match spec (substring, case-insensitive).
  For :list-id, extracts the identifier from angle brackets before comparing."
  [headers-edn match-spec]
  (every? (fn [[k v]]
            (let [header-name (case k
                                :list-id      "List-Id"
                                :delivered-to "Delivered-To"
                                :to           "To"
                                (name k))
                  header-val  (get-header headers-edn header-name)
                  header-val  (if (= k :list-id)
                                (extract-list-id header-val)
                                header-val)]
              (and header-val
                   (str/includes? (str/lower-case (str header-val))
                                  (str/lower-case v)))))
          match-spec))

(defn classify-source
  "Return the :name of the first matching source, or nil.
  A source with no :match acts as a catch-all."
  [headers-edn sources]
  (some (fn [{:keys [name match]}]
          (when (or (empty? match)
                    (match-source? headers-edn match))
            name))
        sources))

(defn build-source-map
  "Build source-name → {:admin :list-post :triggers :labels} from config."
  [config]
  (let [default-admin    (:admin config)
        global-st        (:labels config)]
    (into {}
          (map (fn [src]
                 [(:name src)
                  (cond-> {:admin (or (:admin src) default-admin)}
                    (:list-post src)
                    (assoc :list-post (:list-post src))
                    (:triggers src)
                    (assoc :triggers (:triggers src))
                    (:labels src)
                    (assoc :labels (:labels src))
                    (:archive-format-string src)
                    (assoc :archive-format-string (:archive-format-string src))
                    (:list-archive src)
                    (assoc :list-archive (:list-archive src))
                    global-st
                    (assoc :global-labels global-st))]))
          (:sources config))))

;; ---------------------------------------------------------------------------
;; CLI arg parsing (shared by bark-export and bark-html)
;; ---------------------------------------------------------------------------

(defn parse-cli-args
  "Parse common CLI flags into a map.
  Recognises: -o/--output, -n/--source, -p/--min-priority, -s/--min-status.
  Any leading non-flag token is captured as :format."
  [args]
  (loop [opts {} [a & [v & r :as more]] args]
    (cond
      (nil? a)                        opts
      (#{"-o" "--output"} a)          (if v (recur (assoc opts :out-file v) r) opts)
      (#{"-n" "--source"} a)          (if v (recur (assoc opts :source-name v) r) opts)
      (#{"-p" "--min-priority"} a)    (if v (recur (assoc opts :min-priority (parse-long v)) r) opts)
      (#{"-s" "--min-status"} a)      (if v (recur (assoc opts :min-status (parse-long v)) r) opts)
      (not (:format opts))            (recur (assoc opts :format a) more)
      :else                           (recur opts more))))

;; ---------------------------------------------------------------------------
;; Report scoring (shared by bark-export and bark-notify)
;; ---------------------------------------------------------------------------

(defn report-priority [report]
  (+ (if (:report/urgent report) 2 0)
     (if (:report/important report) 1 0)))

(defn report-status
  "Compute a numeric status score for filtering.
  Higher = more active: open (4) > closed (0), +2 if owned, +1 if acked.
  E.g. --min-status 4 filters to open reports only."
  [report]
  (+ (if-not (:report/closed report) 4 0)
     (if (:report/owned report) 2 0)
     (if (:report/acked report) 1 0)))

(defn report-descendant-count [report]
  (let [d (:report/descendants report)]
    (cond (sequential? d) (count d)
          (map? d)        1
          :else           0)))
