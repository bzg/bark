;; bark-common.clj — Shared utilities for bark bb scripts.
;;
;; Usage: (load-file "scripts/bark-common.clj")

(require '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[taoensso.timbre :as log])

;; ---------------------------------------------------------------------------
;; Logging config
;; ---------------------------------------------------------------------------

(log/merge-config! {:min-level :info})

(defn- parse-size
  "Parse a size string like \"10MB\" into bytes.
  Returns nil if the numeric part is not a valid integer."
  [s]
  (let [s (str/upper-case (str/trim (str s)))]
    (cond
      (str/ends-with? s "GB") (some-> (parse-long (str/replace s #"GB$" "")) (* 1024 1024 1024))
      (str/ends-with? s "MB") (some-> (parse-long (str/replace s #"MB$" "")) (* 1024 1024))
      (str/ends-with? s "KB") (some-> (parse-long (str/replace s #"KB$" "")) (* 1024))
      :else                   (parse-long s))))

(defn- rotate-log!
  "Rotate log-file if it exceeds max-bytes, keeping up to backlog files."
  [log-file max-bytes backlog]
  (let [f (clojure.java.io/file log-file)]
    (when (and (.exists f) (> (.length f) max-bytes))
      (doseq [i (range (dec backlog) 0 -1)]
        (let [src (clojure.java.io/file (str log-file "." i))
              dst (clojure.java.io/file (str log-file "." (inc i)))]
          (when (.exists src) (.renameTo src dst))))
      (.renameTo f (clojure.java.io/file (str log-file ".1"))))))

(defn configure-file-logging!
  "If logging-cfg contains :file, add a Timbre file appender
  that persists logs at or above the specified :level."
  [{:keys [file level max-size backlog]
    :or   {level :warn max-size "10MB" backlog 5}}]
  (when file
    (clojure.java.io/make-parents file)
    (let [max-bytes (parse-size max-size)]
      (log/merge-config!
       {:appenders
        {:file
         {:enabled?  true
          :min-level level
          :fn        (fn [data]
                       (rotate-log! file max-bytes backlog)
                       (spit file
                             (str (force (:timestamp_ data)) " "
                                  (str/upper-case (name (:level data))) " "
                                  (:?ns-str data) " - "
                                  (force (:msg_ data)) "\n")
                             :append true))}}}))))

(defn- ensure-mail-pod!
  "Load the tzzh/mail pod once (idempotent)."
  []
  (when-not (try (requiring-resolve 'pod.tzzh.mail/send-mail) (catch Exception _ nil))
    (require '[babashka.pods :as pods])
    ((resolve 'pods/load-pod) 'tzzh/mail "0.0.3")
    (require '[pod.tzzh.mail :as mail])))

(defn configure-email-logging!
  "Add a Timbre email appender using the SMTP config from :notifications.
  Loads the tzzh/mail pod on first use."
  [smtp-cfg {:keys [to level] :or {level :error}}]
  (when (and smtp-cfg to)
    (ensure-mail-pod!)
    (let [{:keys [host port tls user password from]} smtp-cfg
          send! (resolve 'pod.tzzh.mail/send-mail)]
      (log/merge-config!
       {:appenders
        {:email
         {:enabled?   true
          :min-level  level
          :rate-limit [[5 (* 5 60 1000)]]
          :fn         (fn [data]
                        (try
                          (let [level-str (str/upper-case (name (:level data)))
                                msg       (force (:msg_ data))]
                            (send! {:host     host
                                    :port     (or port 587)
                                    :tls      (boolean tls)
                                    :username user
                                    :password password
                                    :from     from
                                    :to       [to]
                                    :subject  (str "[BARK] " level-str " — " (:?ns-str data))
                                    :text     (str (force (:timestamp_ data)) " "
                                                   level-str " " (:?ns-str data)
                                                   " — " msg)}))
                          (catch Exception e
                            (binding [*out* *err*]
                              (println "Failed to send log email:" (.getMessage e))))))}}}))))

;; ---------------------------------------------------------------------------
;; Utilities
;; ---------------------------------------------------------------------------

(defn slugify
  "Normalize a source name for use as a directory name: strip accents,
  downcase, replace non-alphanumeric runs with hyphens, trim hyphens."
  [s]
  (-> (java.text.Normalizer/normalize (str s) java.text.Normalizer$Form/NFD)
      (str/replace #"\p{InCombiningDiacriticalMarks}+" "")
      str/lower-case
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"^-|-$" "")))

(defn sha256
  "Compute SHA-256 hex digest of a string."
  [^String s]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")
        bytes  (.digest digest (.getBytes s "UTF-8"))]
    (str/join (map #(format "%02x" (Byte/toUnsignedInt %)) bytes))))

(defn mid-hash
  "Compute a stable directory-safe hash from a message-id."
  [message-id]
  (sha256 (str "bark:" message-id)))

(def patch-filename-re #"(?i)\.(patch|diff)$")

(defn patch-file?
  "True if filename looks like a patch/diff file."
  [filename]
  (boolean (and filename (re-find patch-filename-re filename))))

(defn email-body-text
  "Return the plain-text body of an email, preferring :email/body-text
  over :email/body-text-from-html. Returns nil if neither is present."
  [email]
  (or (:email/body-text email) (:email/body-text-from-html email)))

;; ---------------------------------------------------------------------------
;; Datalevin pod — single version definition
;; ---------------------------------------------------------------------------

(def datalevin-version "0.10.7")

(defn load-datalevin-pod!
  "Load the datalevin pod and require its namespace as `d`."
  []
  (require '[babashka.pods :as pods])
  ((resolve 'pods/load-pod) 'huahaiy/datalevin datalevin-version)
  (require '[pod.huahaiy.datalevin :as d]))

;; ---------------------------------------------------------------------------
;; Canonical report pull pattern (shared by export, notify, stats)
;; ---------------------------------------------------------------------------

(def report-pull-pattern
  '[:db/id :report/type :report/version :report/topic
    :report/patch-seq :report/patch-source :report/message-id
    {:report/acked [:email/from-address]}
    {:report/owned [:email/from-address]}
    {:report/closed [:email/from-address :email/date-sent]}
    {:report/acked-proxy [:email/from-address]}
    {:report/owned-proxy [:email/from-address]}
    {:report/closed-proxy [:email/from-address]}
    {:report/urgent [:email/from-address]}
    {:report/important [:email/from-address]}
    {:report/urgent-proxy [:email/from-address]}
    {:report/important-proxy [:email/from-address]}
    :report/close-reason
    :report/votes-up :report/votes-down :report/votes-null
    :report/deadline :report/descendants :report/digested-at :report/updated-at
    {:report/related [:report/type :report/message-id
                      {:report/email [:email/headers-edn]}]}
    {:report/series [:series/id :series/expected :series/closed
                     {:series/patches [:db/id]}
                     {:series/cover-letter [:email/message-id]}]}
    {:report/patches [:patch/filename :patch/source :patch/text
                      :patch/author :patch/subject :patch/date]}
    {:report/email [:email/subject :email/from-address :email/from-name
                    :email/date-sent :email/source :email/imap-uid
                    :email/headers-edn]}])

(defn all-reports
  "Fetch all reports from the database. Returns unsorted.
  Must be called after load-datalevin-pod!."
  [db]
  (let [dq (resolve 'pod.huahaiy.datalevin/q)]
    (->> (dq (list :find (list 'pull '?r report-pull-pattern)
                   :where ['?r :report/type '_])
             db)
         (map first))))

(defn load-config
  "Load config.edn if it exists, or nil.
  Configures file and email logging when :logging is present."
  []
  (let [f (clojure.java.io/file "config.edn")]
    (when (.exists f)
      (let [cfg (edn/read-string (slurp f))]
        (when-let [logging (:logging cfg)]
          (configure-file-logging! logging)
          (when-let [email-cfg (:email logging)]
            (if-let [smtp (get-in cfg [:notifications :smtp])]
              (configure-email-logging! smtp email-cfg)
              (log/warn "Logging :email configured but no :notifications :smtp found."))))
        cfg))))

(defn get-header
  "Case-insensitive header lookup. headers-edn can be an EDN string or
  an already-parsed map. Returns nil on parse failure (with warning)."
  [headers-edn header-name]
  (when headers-edn
    (try
      (let [headers (if (string? headers-edn) (edn/read-string headers-edn) headers-edn)
            lname   (str/lower-case header-name)]
        (some (fn [[k v]] (when (= (str/lower-case k) lname) v)) headers))
      (catch Exception e
        (log/warn "Failed to parse headers-edn:" (.getMessage e))
        nil))))

(defn extract-list-id
  "Extract the identifier from a List-Id header value.
  RFC 2919: \"Description <list-id>\" -> \"list-id\".
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

(def ^:private bark-prefix-pattern
  "Matches [bark:<list-id>] at the start of a subject line (case-insensitive)."
  #"(?i)^\[bark:([^\]]+)\]")

(defn- extract-bark-list-id
  "Extract list-id from a [bark:<list-id>] subject prefix, or nil."
  [subject]
  (when subject
    (second (re-find bark-prefix-pattern subject))))

(defn classify-source
  "Return the :name of the first matching source, or nil.
  Matches by header (List-Id, Delivered-To, etc.) first, then falls back
  to a [bark:<list-id>] subject prefix for maintainers who cannot set
  mail headers.  A source with no :match acts as a catch-all."
  [headers-edn subject sources]
  (let [bark-lid (extract-bark-list-id subject)]
    (some (fn [{:keys [name match]}]
            (when (or (empty? match)
                      (match-source? headers-edn match)
                      ;; Fallback: [bark:<list-id>] in subject matches :list-id
                      (and bark-lid
                           (:list-id match)
                           (str/includes? (str/lower-case bark-lid)
                                          (str/lower-case (:list-id match)))))
              name))
          sources)))

;; ---------------------------------------------------------------------------
;; Maintainer-since parsing (shared by bark-roles, bark-docs)
;; ---------------------------------------------------------------------------

(defn parse-maintainer-since-entries
  "Parse :roles/maintainer-since entries (\"email:yyyy-MM-dd\") into a map
  of lower-cased email -> date-string.
  The separator is the *last* colon, which is safe because email addresses
  cannot contain colons (RFC 5321)."
  [roles]
  (let [entries (let [v (:roles/maintainer-since roles)]
                  (cond (nil? v) #{} (string? v) #{v} :else (set v)))]
    (into {}
          (keep (fn [entry]
                  (let [idx (str/last-index-of entry ":")]
                    (when (and idx (pos? idx))
                      [(subs entry 0 idx) (subs entry (inc idx))]))))
          entries)))

(defn build-source-map
  "Build source-name -> {:admin :list-post :list-id :list-archive :bark-path ...} from config."
  [config]
  (let [default-admin    (:admin config)
        global-st        (:labels config)
        global-cmd       (:commands config)
        global-ef        (:export-formats config)
        global-er        (:export-reports config)
        global-expiry    (:expiry config)
        global-rt        (:report-types config)]
    (into {}
          (map (fn [src]
                 [(:name src)
                  (merge {:admin (or (:admin src) default-admin)}
                         (select-keys src [:list-post :commands :labels :notifications
                                           :archive-format-string :list-archive :bark-path
                                           :maintainers])
                         (when-let [lid (get-in src [:match :list-id])] {:list-id lid})
                         (when global-st {:global-labels global-st})
                         (when global-cmd {:global-commands global-cmd})
                         {:export-formats (set (or (:export-formats src)
                                                   global-ef
                                                   ["json" "org" "rss"]))
                          :export-reports (when-let [er (or (:export-reports src) global-er)]
                                            (set (map keyword er)))
                          :report-types (when-let [rt (or (:report-types src) global-rt)]
                                          (set (map keyword rt)))
                          :expiry (or (:expiry src) global-expiry)})]))
          (:sources config))))

;; ---------------------------------------------------------------------------
;; CLI arg parsing (shared by bark-export and bark-html)
;; ---------------------------------------------------------------------------

(defn parse-cli-args
  "Parse common CLI flags into a map.
  Recognises: -o/--output, -n/--source, -p/--min-priority, -s/--min-status,
  --json (path to all.json), --dir (output directory), --force (force full
  export), --only-open (also export -open files with only open reports).
  Any leading non-flag token is captured as :format."
  [args]
  (loop [opts {} [a & [v & r :as more]] args]
    (cond
      (nil? a)                        opts
      (#{"--force"} a)                (recur (assoc opts :force-all? true) more)
      (#{"--only-open"} a)            (recur (assoc opts :only-open? true) more)
      (#{"-o" "--output"} a)          (if v (recur (assoc opts :out-file v) r) opts)
      (#{"--json"} a)                 (if v (recur (assoc opts :json-file v) r) opts)
      (#{"--dir"} a)                  (if v (recur (assoc opts :out-dir v) r) opts)
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

;; ---------------------------------------------------------------------------
;; Date formatting (shared by bark-export, bark-notify, bark-stats)
;; ---------------------------------------------------------------------------

(defn format-date [date]
  (let [s (str (or date ""))]
    (subs s 0 (min 16 (count s)))))

(defn format-date-iso
  "Format a java.util.Date as yyyy-MM-dd (ISO 8601 date only)."
  [date]
  (when date
    (let [fmt (java.text.SimpleDateFormat. "yyyy-MM-dd")]
      (.setTimeZone fmt (java.util.TimeZone/getTimeZone "UTC"))
      (.format fmt date))))

;; ---------------------------------------------------------------------------
;; Shared label/command defaults and merge logic
;; (canonical definitions used by bark-detect, bark-commands, bark-docs)
;; ---------------------------------------------------------------------------

(def default-labels
  "Default subject tags per report type."
  {:bug          ["BUG"]
   :patch        ["PATCH"]
   :request      ["POLL" "FR" "TODO"]
   :announcement ["ANN" "ANNOUNCEMENT"]
   :release      ["REL" "RELEASE"]
   :change       ["CHG" "CHANGE"]})

(def default-commands
  "Default command words per action. Flat — applies to all report types.
  Configurable via :commands in config.edn."
  {:acked     ["Acked" "Confirmed" "Reviewed" "Approved"]
   :owned     ["Owned" "Handled" "Assigned"]
   :closed    ["Canceled" "Cancelled" "Resolved" "Applied"
               "Done" "Fixed" "Closed" "Expired"]
   :urgent    ["Urgent"]
   :important ["Important"]})

(def close-reasons
  "Map closed command words to close reasons.
  Words not listed here default to :resolved."
  {"Canceled"  :canceled
   "Cancelled" :canceled
   "Expired"   :expired})

(defn resolve-labels-map
  "Resolve labels for a source-map entry: defaults -> global -> per-source.
  Returns a (non-compiled) labels map."
  [source-cfg]
  (cond-> default-labels
    (:global-labels source-cfg) (merge (:global-labels source-cfg))
    (:labels source-cfg)        (merge (:labels source-cfg))))

(defn resolve-commands-map
  "Resolve command words for a source-map entry: defaults -> global -> per-source.
  Returns a (non-compiled) word-list map."
  [source-cfg]
  (let [global (:global-commands source-cfg)
        local  (:commands source-cfg)]
    (cond-> default-commands
      global (merge global)
      local  (merge local))))

;; ---------------------------------------------------------------------------
;; Shared schema (used by bark-export, bark-notify, bark-stats, bark-digest)
;; ---------------------------------------------------------------------------

(def bark-schema
  (let [f (clojure.java.io/file "resources/bark-schema.edn")]
    (if (.exists f)
      (edn/read-string (slurp f))
      (throw (ex-info "resources/bark-schema.edn not found" {:path (.getAbsolutePath f)})))))

;; ---------------------------------------------------------------------------
;; Export change tracking
;; ---------------------------------------------------------------------------

(defn bump-report-updated!
  "Set :report/updated-at and :meta/last-modified to now.
  `report-eid` can be a single eid or a collection of eids."
  [conn report-eid]
  (let [dt!  (resolve 'pod.huahaiy.datalevin/transact!)
        now  (java.util.Date.)
        eids (if (coll? report-eid) report-eid [report-eid])
        tx   (into [{:meta/ident "global" :meta/last-modified now}]
                   (map (fn [eid] {:db/id eid :report/updated-at now}))
                   eids)]
    (dt! conn tx)))

(defn bump-global-modified!
  "Bump :meta/last-modified without targeting a specific report.
  Use when a mutation (e.g. expiry) already transacted report changes
  and you just need the global flag."
  [conn]
  (let [dt! (resolve 'pod.huahaiy.datalevin/transact!)]
    (dt! conn [{:meta/ident "global" :meta/last-modified (java.util.Date.)}])))

(defn get-last-modified
  "Return the global :meta/last-modified instant, or nil."
  [db]
  (let [dq (resolve 'pod.huahaiy.datalevin/q)]
    (dq '[:find ?t .
           :where [?e :meta/ident "global"] [?e :meta/last-modified ?t]]
        db)))

(defn get-last-export
  "Return the :meta/last-export instant, or nil."
  [db]
  (let [dq (resolve 'pod.huahaiy.datalevin/q)]
    (dq '[:find ?t .
           :where [?e :meta/ident "global"] [?e :meta/last-export ?t]]
        db)))

(defn save-last-export!
  "Record the export timestamp."
  [conn ts]
  (let [dt! (resolve 'pod.huahaiy.datalevin/transact!)]
    (dt! conn [{:meta/ident "global" :meta/last-export ts}])))

(defn changed-report-types-since
  "Return the set of :report/type keywords that have been updated since `since-ts`."
  [db since-ts]
  (let [dq (resolve 'pod.huahaiy.datalevin/q)]
    (set (dq '[:find [?t ...]
               :in $ ?since
               :where
               [?r :report/updated-at ?u]
               [(> ?u ?since)]
               [?r :report/type ?t]]
             db since-ts))))

;; ---------------------------------------------------------------------------
;; Shared date arithmetic (used by bark-digest, bark-stats)
;; ---------------------------------------------------------------------------

(defn days-between
  "Number of whole days between two java.util.Date instances (absolute)."
  [^java.util.Date a ^java.util.Date b]
  (when (and a b)
    (quot (Math/abs (- (.getTime b) (.getTime a)))
          86400000)))
