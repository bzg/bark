#!/usr/bin/env bb

;; bark-export.clj — Export BARK reports per source.
;;
;; Each source gets its own directory tree under public/:
;;   public/<source-name>/index.html
;;   public/<source-name>/data.html
;;   public/<source-name>/docs.html
;;   public/<source-name>/reports/all.json
;;   public/<source-name>/reports/all.xml
;;   public/<source-name>/reports/all.org
;;   public/<source-name>/reports/bugs.json  (etc.)
;;   public/<source-name>/reports/stats.json
;;   public/<source-name>/events/<mid-hash>/<file>.ics
;;   public/<source-name>/events/announcements.ics
;;   public/<source-name>/events/announcements-open.ics
;;   public/<source-name>/events/announcements-closed.ics
;;   public/<source-name>/reports/events.json
;;   public/<source-name>/reports/events.org
;;   public/<source-name>/reports/events-closed.json
;;   public/<source-name>/reports/events-closed.org
;;   public/<source-name>/text/<mid-hash>/<file>
;;   public/<source-name>/patches/<mid-hash>/<file>
;;
;; Usage:
;;   bb export               — incremental export (skip if nothing changed)
;;   bb export json          — export all.json for each source
;;   bb export rss           — export all.xml for each source
;;   bb export org           — export all.org for each source
;;   bb export html          — generate index.html for each source
;;   bb export stats         — generate stats.json for each source
;;   bb export patches       — export patch files for each source
;;   bb export events        — export ICS event files and events.ics for each source
;;   bb export text          — export text/plain and text/x-log attachments
;;   bb export root          — regenerate public/index.html (source listing)
;;   bb export all           — all formats (still incremental)
;;   bb export --force       — force full export, ignore timestamps
;;   bb export --only-open   — also export -open files (all-open.json, etc.)
;;   bb export json -n src   — export only source "src"
;;   bb export json -p 2     — only priority >= 2
;;   bb export json -s 3     — only status >= 3
;;
;; Environment / defaults:
;;   BARK_DB — path to db (default: ./data/bark-db)

(require '[babashka.process :as process]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io])

;; Forward-declared for clj-kondo (provided at runtime by load-file below).
(declare load-datalevin-pod! get-header slugify mid-hash email-body-text
         ensure-set format-date format-date-iso report-priority report-status
         report-descendant-count all-reports report-pull-pattern attachment-pull-pattern
         parse-cli-args load-config build-source-map bark-schema bark-format
         fetch-attachment-data get-tenures tenures-snapshot
         get-last-modified changed-source-types-since
         set-theme! resolve-css-theme votes-by-report vote-counts
         html-head footer-css bark-footer wrap-js spit-html theme-toggle-js bark-repo-url
         ics-file? text-attachment?)

(load-file "scripts/bark-common.clj")
(load-file "scripts/bark-html.clj")

(load-datalevin-pod!)

;; ---------------------------------------------------------------------------
;; File-based export timestamp (replaces DB-based save-last-export!)
;; The export is fully read-only w.r.t. the database.
;; ---------------------------------------------------------------------------

(def ^:private last-export-file "public/.last-export")

(defn- get-last-export
  "Read the last export timestamp from public/.last-export, or nil."
  []
  (let [f (io/file last-export-file)]
    (when (.exists f)
      (try (java.util.Date. ^long (parse-long (str/trim (slurp f))))
           (catch Exception _ nil)))))

(defn- save-last-export!
  "Write the export timestamp to public/.last-export."
  [^java.util.Date ts]
  (io/make-parents last-export-file)
  (spit last-export-file (str (.getTime ts))))

;; ---------------------------------------------------------------------------
;; --closed-retention: resolve a date or duration to a cutoff java.util.Date.
;; Reports closed before that date are excluded from export.
;; ---------------------------------------------------------------------------

(defn- resolve-closed-retention-date
  "Turn a --closed-retention value (ISO date or duration like \"1y\", \"6m\")
  into a java.util.Date cutoff.  Returns nil on invalid input."
  [v]
  (when v
    (if (re-matches #"\d{4}-\d{2}-\d{2}" v)
      ;; ISO date
      (try (.parse (java.text.SimpleDateFormat. "yyyy-MM-dd") v)
           (catch Exception _ nil))
      ;; Duration string → days before today
      (when-let [days (parse-delay v)]
        (java.util.Date. (- (System/currentTimeMillis)
                            (* days 24 60 60 1000)))))))

(defn- drop-old-closed
  "Remove reports closed before `cutoff-date`."
  [reports ^java.util.Date cutoff-date]
  (remove (fn [r]
            (when-let [closed-email (:report/closed r)]
              (when-let [^java.util.Date closed-at (:email/date-sent closed-email)]
                (.before closed-at cutoff-date))))
          reports))

(defn- resolve-topics-filter
  "Parse a topics filter value into a set of lower-cased topic strings.
  Accepts a comma-separated string (CLI) or a vector (config.edn).
  Returns nil when no filter is active."
  [v]
  (when v
    (let [topics (if (string? v)
                   (map str/trim (str/split v #","))
                   (map str v))]
      (when (seq topics)
        (set (map str/lower-case topics))))))

(defn- filter-by-topics
  "Keep only reports whose :report/topic matches one of the given topics (case-insensitive).
  Returns all reports when `topics` is nil."
  [reports topics]
  (if topics
    (filter (fn [r]
              (when-let [t (:report/topic r)]
                (topics (str/lower-case t))))
            reports)
    reports))

;; ---------------------------------------------------------------------------
;; DB queries (all-reports and report-pull-pattern loaded from bark-common.clj)
;; ---------------------------------------------------------------------------

(defn all-reports-by-date [db]
  (let [epoch (java.util.Date. 0)]
    (sort-by #(or (get-in % [:report/email :email/date-sent]) epoch)
             #(compare %2 %1)
             (all-reports db))))

;; ---------------------------------------------------------------------------
;; Formatting helpers
;; ---------------------------------------------------------------------------

(defn- close-flag [report]
  (if (:report/closed report)
    (case (:report/close-reason report)
      :canceled   "C"
      :expired    "E"
      :superseded "S"
      "R")
    "-"))

(defn- flags-str [report]
  (str (if (:report/acked report) "A" "-")
       (if (:report/owned report) "O" "-")
       (close-flag report)))

;; format-date and format-date-iso are defined in bark-common.clj

(defn- votes-str
  "Format vote counts as \"score/total\" from a seq of vote maps, or nil."
  [votes]
  (when (seq votes)
    (let [{:keys [up down null]} (vote-counts votes)
          total (+ up down null)]
      (when (pos? total)
        (str (- up down) "/" total)))))

;; ---------------------------------------------------------------------------
;; Config & source map — loaded from bark-common.clj
;; ---------------------------------------------------------------------------

(defn- build-maintainers
  "Gather per-source currently-active maintainer sets from DB tenures.
   Returns source-name -> #{maintainer-emails}."
  [db source-map]
  (into {}
        (map (fn [[source-name _]]
               [source-name
                (->> (get-tenures db source-name)
                     (remove :to)
                     (keep :email)
                     (map str/lower-case)
                     set)]))
        source-map))

;; ---------------------------------------------------------------------------
;; Report -> map
;; ---------------------------------------------------------------------------

;; get-header loaded from bark-common.clj

(defn- archived-at [email]
  (get-header (:email/headers-edn email) "Archived-At"))

(defn- sender-role
  "Determine role of sender for a given source context."
  [from source-name _source-map maintainers-map]
  (when (and (seq from) source-name)
    (let [from-lc (str/lower-case from)]
      (when (contains? (get maintainers-map source-name #{}) from-lc)
        "maintainer"))))

(def ^:private address-fields
  "Report address attrs to extract into the output map."
  [[:report/acked-address :acked] [:report/owned-address :owned]
   [:report/closed-address :closed] [:report/urgent-address :urgent]
   [:report/important-address :important]])

(def ^:private proxy-address-pairs
  "Triplets [ref-attr address-key proxy-key] for extracting maintainer from-address.
  The proxy key is only emitted when it differs from the address key (i.e. a -by directive)."
  [[:report/acked :acked :acked-proxy] [:report/owned :owned :owned-proxy]
   [:report/closed :closed :closed-proxy] [:report/urgent :urgent :urgent-proxy]
   [:report/important :important :important-proxy]])

(defn- assoc-from-addresses
  "Extract addresses from report: direct string attrs and from-address of ref attrs.
  Omits -proxy keys when their value equals the corresponding address key."
  [m report]
  (as-> m m
    (reduce (fn [m [rk mk]]
              (if-let [v (get report rk)]
                (assoc m mk v)
                m))
            m address-fields)
    (reduce (fn [m [rk mk proxy-mk]]
              (if-let [v (get report rk)]
                (let [addr (:email/from-address v)]
                  (if (= addr (get m mk))
                    m
                    (assoc m proxy-mk addr)))
                m))
            m proxy-address-pairs)))

(defn- archive-url
  "Compute the archive URL for a report, or nil."
  [report email source-map]
  (let [source-name (:email/source email)
        src-type    (get-in source-map [source-name :source-type])]
    (when-not (#{:alias :mailbox} src-type)
      (let [raw  (archived-at email)
            mid  (some-> (:report/message-id report) (str/replace #"^<|>$" ""))
            fmt  (get-in source-map [source-name :archive-format-string])]
        (if (and fmt mid) (str/replace fmt "%s" mid) raw)))))

(defn- report-vote-fields
  "Build vote-related fields from vote data."
  [report-votes]
  (when (seq report-votes)
    (let [votes  (votes-str report-votes)
          counts (vote-counts report-votes)]
      (cond-> {}
        votes                    (assoc :votes votes)
        (pos? (:up counts 0))   (assoc :votes-up (:up counts))
        (pos? (:down counts 0)) (assoc :votes-down (:down counts))
        (pos? (:null counts 0)) (assoc :votes-null (:null counts))))))

(defn- report-series-fields [series]
  (when series
    (let [patches (:series/patches series)]
      {:id       (:series/id series)
       :received (count patches)
       :expected (:series/expected series)
       :complete (= (count patches) (:series/expected series))
       :closed   (some? (:series/closed series))})))

(defn- report-patch-fields [report]
  (when (seq (:report/patches report))
    (let [h (mid-hash (:report/message-id report))]
      (mapv (fn [p]
              (cond-> {:file   (str h "/" (:patch/filename p))
                       :source (name (:patch/source p))}
                (:patch/author p)  (assoc :author  (:patch/author p))
                (:patch/subject p) (assoc :subject (:patch/subject p))
                (:patch/date p)    (assoc :date    (:patch/date p))))
            (:report/patches report)))))

(defn- report-attachment-files
  "Build :events and :texts fields from lazy-fetched attachment data."
  [report att-email]
  (let [h (mid-hash (:report/message-id report))]
    (cond-> {}
      (and (= :announcement (:report/type report))
           (:report/has-ics report))
      (assoc :events
             (mapv (fn [att]
                     {:file (str h "/" (.getName (clojure.java.io/file (:attachment/filename att))))})
                   (filter #(ics-file? (:attachment/filename %))
                           (:email/attachments @att-email))))
      (:report/has-text-attachments report)
      (assoc :texts
             (mapv (fn [att]
                     {:file (str h "/" (.getName (clojure.java.io/file (:attachment/filename att))))})
                   (filter #(text-attachment? %)
                           (:email/attachments @att-email)))))))

;; ---------------------------------------------------------------------------
;; Export context — set once at the start of each export run via
;; init-export-context!.  Used by map-reports, dump-events!, dump-text!,
;; and collect-vevents to access the DB and votes without threading
;; parameters through every dump-* function.
;; ---------------------------------------------------------------------------

(def ^:private export-ctx
  "Export context: {:db <datalevin-db> :votes {eid -> [vote-maps]} :config <config>}"
  (atom {:db nil :votes {} :config nil}))

(defn- init-export-context! [db votes config]
  (reset! export-ctx {:db db :votes votes :config config}))

(defn- ctx-db [] (:db @export-ctx))
(defn- ctx-votes [] (:votes @export-ctx))
(defn- ctx-config [] (:config @export-ctx))

(def ^:private default-awaiting-delay-days 14)

(defn- awaiting-reply?
  "True when a report is open, last activity was by an admin/maintainer,
  and the configured delay has elapsed."
  [report source-name source-map maintainers-map]
  (when (and (not (:report/closed report))
             (:report/last-activity report)
             (:report/last-activity-address report))
    (let [addr    (str/lower-case (:report/last-activity-address report))
          role    (sender-role addr source-name source-map maintainers-map)]
      (when role
        (let [config     (ctx-config)
              src-cfg    (get source-map source-name)
              delay-str  (or (:awaiting-delay src-cfg)
                             (:awaiting-delay config))
              delay-days (if delay-str (parse-delay delay-str) default-awaiting-delay-days)
              ^java.util.Date last-act (:report/last-activity report)
              elapsed-ms  (- (System/currentTimeMillis) (.getTime last-act))
              elapsed-days (/ elapsed-ms (* 24 60 60 1000))]
          (>= elapsed-days delay-days))))))

(defn report->map [report source-map maintainers-map report-votes db]
  (let [email       (:report/email report)
        source-name (:email/source email)
        att-data    (delay (when db
                     (fetch-attachment-data db (:report/message-id report))))
        att-email   (delay (:report/email @att-data))
        from        (or (:email/from-address email) "")
        arch        (archive-url report email source-map)
        src-type    (get-in source-map [source-name :source-type])
        related     (:report/related report)
        role        (sender-role from source-name source-map maintainers-map)
        awaiting?   (awaiting-reply? report source-name source-map maintainers-map)]
    (-> {:type     (name (:report/type report))
         :subject  (or (:email/subject email) "")
         :from     from
         :date     (format-date (:email/date-sent email))
         :date-raw (str (:email/date-sent email))
         :flags    (flags-str report)
         :status   (report-status report)
         :priority (report-priority report)
         :replies  (report-descendant-count report)}
        (assoc-from-addresses report)
        (cond->
         (:email/from-name email)        (assoc :from-name (:email/from-name email))
          role                            (assoc :role role)
          (:report/message-id report)     (assoc :message-id (:report/message-id report))
          (:report/version report)        (assoc :version (:report/version report))
          (:report/topic report)          (assoc :topic (:report/topic report))
          (:report/patch-seq report)      (assoc :patch-seq (:report/patch-seq report))
          (:report/patch-source report)   (assoc :patch-source (mapv name (sort (:report/patch-source report))))
          arch                            (assoc :archived-at arch)
          (:report/deadline report)       (assoc :deadline (format-date-iso (:report/deadline report)))
          (:report/last-activity report)  (assoc :last-activity (format-date-iso (:report/last-activity report)))
          awaiting?                      (assoc :awaiting true)
          (:report/expiry report)         (assoc :expiry (format-date-iso (:report/expiry report)))
          (:report/close-reason report)   (assoc :close-reason (name (:report/close-reason report)))
          (:report/superseded-by report)  (assoc :superseded-by
                                                 {:message-id (:report/message-id (:report/superseded-by report))
                                                  :subject (get-in report [:report/superseded-by :report/email :email/subject])})
          (and (= :expired (:report/close-reason report))
               (:email/date-sent (:report/closed report)))
          (assoc :expired-date (format-date-iso (:email/date-sent (:report/closed report)))))
        (merge (report-vote-fields report-votes))
        (cond->
          (:report/series report) (assoc :series (report-series-fields (:report/series report)))
          (seq related)
          (assoc :related
                 (mapv (fn [r]
                         (let [a (when-not (#{:alias :mailbox} src-type)
                                   (archived-at (:report/email r)))]
                           (cond-> {:type (name (:report/type r))
                                    :message-id (:report/message-id r)}
                             a (assoc :archived-at a))))
                       related)))
        (cond->
          (seq (:report/patches report)) (assoc :patches (report-patch-fields report)))
        (merge (report-attachment-files report att-email))
        ;; Ensure closed is truthy when close-reason is set (auto-superseded
        ;; reports may lack :report/closed-address, leaving :closed unset).
        (as-> m (if (and (:close-reason m) (not (:closed m)))
                  (assoc m :closed "auto")
                  m)))))

(defn- map-reports
  "Map reports through report->map, looking up votes from export context."
  [reports source-map maintainers-map]
  (let [av (ctx-votes)
        db (ctx-db)]
    (mapv #(report->map % source-map maintainers-map (get av (:db/id %)) db)
          reports)))

;; ---------------------------------------------------------------------------
;; Source metadata for JSON envelope
;; ---------------------------------------------------------------------------

(defn- source-metadata
  "Build metadata map for a single source."
  [source-name source-map]
  (let [cfg (get source-map source-name)]
    (cond-> {:source-type (or (:source-type cfg) :mailbox)}
      (:list cfg)          (assoc :list          (:list cfg))
      (:alias cfg)         (assoc :alias         (:alias cfg))
      (:to cfg)            (assoc :to            (:to cfg))
      (:list-archive cfg)  (assoc :list-archive  (:list-archive cfg))
      (:base-url cfg)      (assoc :base-url      (:base-url cfg)))))

;; ---------------------------------------------------------------------------
;; XML helpers
;; ---------------------------------------------------------------------------

(defn- xml-escape [s]
  (when s
    (-> s
        (str/replace "&" "&amp;")
        (str/replace "<" "&lt;")
        (str/replace ">" "&gt;")
        (str/replace "\"" "&quot;")
        (str/replace "'" "&apos;"))))

;; ---------------------------------------------------------------------------
;; Filtering
;; ---------------------------------------------------------------------------

(defn- filter-reports
  "Filter reports by optional criteria: source name, type, min priority/status."
  [reports {:keys [source type min-priority min-status]}]
  (cond->> reports
    source       (filter #(= source (get-in % [:report/email :email/source])))
    type         (filter #(= type (:report/type %)))
    min-priority (filter #(>= (report-priority %) min-priority))
    min-status   (filter #(>= (report-status %) min-status))))

(defn- open-reports
  "Keep only reports that are not closed."
  [reports]
  (remove :report/closed reports))

;; ---------------------------------------------------------------------------
;; Per-type helpers
;; ---------------------------------------------------------------------------

(def report-types [:bug :patch :request :announcement :release :change])

(def type->plural
  {:bug "bugs" :patch "patches" :request "requests"
   :announcement "announcements" :release "releases" :change "changes"})

(def rss-limit 50)

;; ---------------------------------------------------------------------------
;; Per-source export functions
;; ---------------------------------------------------------------------------

(defn dump-json!
  "Dump reports as JSON for a single source."
  ([reports out-dir source-name source-map maintainers-map]
   (dump-json! reports out-dir source-name source-map maintainers-map "all.json" nil))
  ([reports out-dir source-name source-map maintainers-map basename]
   (dump-json! reports out-dir source-name source-map maintainers-map basename nil))
  ([reports out-dir source-name source-map maintainers-map basename extra-meta]
   (let [data     (map-reports reports source-map maintainers-map)
         meta     (source-metadata source-name source-map)
         envelope (cond-> {:bark-format bark-format
                           :source      source-name
                           :reports     data}
                    (seq meta)       (merge meta)
                    (seq extra-meta) (merge extra-meta))
         filename (str out-dir "/" basename)]
     (spit filename (json/generate-string envelope {:pretty true}))
     (when (seq data)
       (log/info "Wrote" (count data) "reports to" filename)))))

(defn- parse-date-tostring
  "Parse a java.util.Date#toString value into a java.util.Date.
  Returns nil on invalid input."
  [date-str]
  (when (and date-str (not (str/blank? (str date-str))))
    (try
      (let [fmt (doto (java.text.SimpleDateFormat. "EEE MMM dd HH:mm:ss zzz yyyy"
                                                    java.util.Locale/ENGLISH)
                  (.setLenient true))]
        (.parse fmt (str date-str)))
      (catch Exception _ nil))))

(defn- rfc822-date
  "RFC 822 date from a java.util.Date#toString string."
  [date-str]
  (when-let [d (parse-date-tostring date-str)]
    (try
      (let [out-fmt (doto (java.text.SimpleDateFormat. "EEE, dd MMM yyyy HH:mm:ss Z"
                                                       java.util.Locale/ENGLISH)
                      (.setTimeZone (java.util.TimeZone/getTimeZone "UTC")))]
        (.format out-fmt d))
      (catch Exception _
        (log/debug "Could not parse date for RSS:" date-str)
        nil))))

(defn- rss-author [m]
  (let [email (:from m)
        name  (:from-name m)]
    (xml-escape (if (and name (not= name email))
                  (str email " (" name ")")
                  email))))

(defn- rss-guid [m]
  (let [arch (:archived-at m)
        mid  (:message-id m)]
    (cond
      (seq arch) {:value (xml-escape arch) :permalink true}
      mid        {:value (xml-escape (str "urn:message-id:" mid)) :permalink false}
      :else      nil)))

(defn- report->rss-item [m]
  (let [title   (xml-escape (:subject m))
        link    (or (:archived-at m) "")
        date    (rfc822-date (:date-raw m))
        author  (rss-author m)
        guid    (rss-guid m)
        flags   (:flags m)
        replies (:replies m)
        desc    (xml-escape
                 (str "[" (:type m) "] flags:" flags " replies:" replies
                      (when-let [v (:version m)]  (str " version:" v))
                      (when-let [t (:topic m)]    (str " topic:" t))
                      (when-let [d (:deadline m)] (str " deadline:" d))
                      (when-let [d (:expiry m)]   (str " expiry:" d))
                      (when-let [s (:series-summary m)] (str " series:" s))))]
    (str "    <item>\n"
         "      <title>" title "</title>\n"
         (when (seq link)
           (str "      <link>" (xml-escape link) "</link>\n"))
         (when guid
           (if (:permalink guid)
             (str "      <guid>" (:value guid) "</guid>\n")
             (str "      <guid isPermaLink=\"false\">" (:value guid) "</guid>\n")))
         (when date
           (str "      <pubDate>" date "</pubDate>\n"))
         "      <author>" author "</author>\n"
         "      <description>" desc "</description>\n"
         "    </item>")))

(defn- series-status-summary
  "Build a short status summary for a folded series, e.g. \"3/5 acked, 1 open\".
  Excludes cover letters (patch-seq starting with \"0/\") from the tally."
  [members]
  (let [patches (remove #(str/starts-with? (or (:patch-seq %) "") "0/") members)
        counts (reduce (fn [acc m]
                         (let [flags (or (:flags m) "---")]
                           (cond
                             (not= \- (nth flags 2 \-)) (update acc :closed (fnil inc 0))
                             (not= \- (nth flags 0 \-)) (update acc :acked (fnil inc 0))
                             :else                       (update acc :open (fnil inc 0)))))
                       {} patches)
        parts (cond-> []
                (:acked counts) (conj (str (:acked counts) " acked"))
                (:closed counts) (conj (str (:closed counts) " closed"))
                (:open counts) (conj (str (:open counts) " open")))]
    (str/join ", " parts)))

(defn- fold-series
  "Fold report maps by series: replace each series group with a single
  representative (cover letter preferred, else first patch by patch-seq).
  Non-series reports pass through unchanged.  Preserves original order
  (the representative appears at the position of the first group member)."
  [report-maps]
  (let [groups (group-by #(get-in % [:series :id]) report-maps)
        seen   (volatile! #{})]
    (reduce (fn [acc m]
              (let [sid (get-in m [:series :id])]
                (if-not sid
                  (conj acc m)
                  (if (contains? @seen sid)
                    acc ;; already emitted representative
                    (let [_ (vswap! seen conj sid)
                          members (get groups sid)
                          sorted  (sort-by #(parse-long (re-find #"^\d+" (or (:patch-seq %) "0"))) members)
                          cover   (first (filter #(str/starts-with? (or (:patch-seq %) "") "0/") sorted))
                          rep     (or cover (first sorted))
                          summary (series-status-summary members)]
                      (conj acc (assoc rep :series-summary summary)))))))
            [] report-maps)))

(defn dump-rss!
  "Dump reports as RSS 2.0 for a single source.
  Only includes open reports, capped at rss-limit (50).
  Patch series are folded: one item per series."
  ([reports out-dir source-name source-map maintainers-map]
   (dump-rss! reports out-dir source-name source-map maintainers-map "all.xml" "reports"))
  ([reports out-dir source-name source-map maintainers-map basename feed-label]
   (let [open     (->> reports open-reports (take (* rss-limit 20)))
         data     (map-reports open source-map maintainers-map)
         data     (->> data fold-series (take rss-limit))
         items    (str/join "\n" (map report->rss-item data))
         list-url (get-in source-map [source-name :list-archive] "")
         filename (str out-dir "/" basename)]
     (when (seq data)
       (spit filename
             (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                  "<rss version=\"2.0\">\n"
                  "  <channel>\n"
                  "    <title>BARK " source-name " " feed-label "</title>\n"
                  "    <link>" list-url "</link>\n"
                  "    <description>Reports from the Bug And Report Keeper</description>\n"
                  items "\n"
                  "  </channel>\n"
                  "</rss>\n"))
       (log/info "Wrote" (count data) "reports to" filename)))))

(defn- format-org-inactive-ts
  "Parse a java.util.Date#toString and return an Org inactive timestamp
  like [2026-03-12 Thu 20:05]."
  [date-raw]
  (if-let [d (parse-date-tostring date-raw)]
    (try
      (let [out-fmt (doto (java.text.SimpleDateFormat. "yyyy-MM-dd EEE HH:mm"
                                                       java.util.Locale/ENGLISH)
                      (.setTimeZone (java.util.TimeZone/getTimeZone "UTC")))]
        (str "[" (.format out-fmt d) "]"))
      (catch Exception _ (str date-raw)))
    (str date-raw)))

(defn- strip-angle-brackets [s]
  (when s (str/replace s #"^<|>$" "")))

(defn- report->org-entry [m]
  (let [todo    (if (= (nth (:flags m "---") 2 \-) \C) "DONE" "TODO")
        prio    (case (:priority m 0)
                  3 "[#A] " 2 "[#B] " 1 "[#C] " "")
        subject (:subject m "")
        tags    (when-let [t (:type m)] (str ":" t ":"))
        org-date (format-org-inactive-ts (:date-raw m))
        props   (remove nil?
                        [(str ":FROM: " (:from m ""))
                         (str ":DATE: " org-date)
                         (when-let [mid (:message-id m)]
                           (str ":MESSAGE-ID: " (strip-angle-brackets mid)))
                         (when-let [a (:archived-at m)]  (str ":ARCHIVED-AT: " a))
                         (str ":FLAGS: " (:flags m "---"))
                         (str ":STATUS: " (:status m 0))
                         (str ":REPLIES: " (:replies m 0))
                         (when-let [v (:version m)]      (str ":VERSION: " v))
                         (when-let [t (:topic m)]        (str ":TOPIC: " t))
                         (when-let [v (:votes m)]        (str ":VOTES: " v))
                         (when-let [v (:votes-up m)]     (str ":VOTES-UP: " v))
                         (when-let [v (:votes-down m)]   (str ":VOTES-DOWN: " v))
                         (when-let [v (:votes-null m)]   (str ":VOTES-NULL: " v))
                         (when-let [a (:acked m)]      (str ":ACKED: " a))
                         (when-let [o (:owned m)]      (str ":OWNED: " o))
                         (when-let [c (:closed m)]     (str ":CLOSED: " c))
                         (when-let [cr (:close-reason m)] (str ":CLOSE-REASON: " cr))
                         (when-let [u (:urgent m)]     (str ":URGENT: " u))
                         (when-let [i (:important m)]  (str ":IMPORTANT: " i))
                         (when-let [d (:deadline m)]     (str ":DEADLINE: " d))
                         (when-let [d (:expiry m)]      (str ":EXPIRY: " d))
                         (when-let [s (:series m)]
                           (str ":SERIES: " (:received s) "/" (:expected s)
                                (when (:closed s) " closed")))])]
    (str "* " todo " " prio subject (when tags (str "  " tags)) "\n"
         (when-let [d (:deadline m)]
           (str "DEADLINE: <" d ">\n"))
         ":PROPERTIES:\n"
         (str/join "\n" props) "\n"
         ":END:\n"
         (when-let [related (seq (:related m))]
           (str "\nRelated:\n"
                (str/join "\n"
                          (map (fn [r]
                                 (str "- [" (:type r) "] " (:message-id r)
                                      (when-let [a (:archived-at r)]
                                        (str " (" a ")"))))
                               related))
                "\n")))))

(defn dump-org!
  "Dump reports as Org for a single source."
  ([reports out-dir source-name source-map maintainers-map]
   (dump-org! reports out-dir source-name source-map maintainers-map "all.org" "reports"))
  ([reports out-dir source-name source-map maintainers-map basename title-label]
   (let [data     (map-reports reports source-map maintainers-map)
         filename (str out-dir "/" basename)]
     (when (seq data)
       (let [entries  (str/join "\n" (map report->org-entry data))]
         (spit filename
               (str "#+TITLE: BARK " source-name " " title-label "\n"
                    "#+DATE: " (java.time.LocalDate/now) "\n\n"
                    entries))
         (log/info "Wrote" (count data) "reports to" filename))))))

(defn dump-votes!
  "Export votes.json for a single source.
  Format: {\"<mid>\": {\"+1\": [{voter, message-id}], \"-1\": [...], \"0\": [...]}}"
  [reports out-dir]
  (let [av       (ctx-votes)
        entries  (reduce
                  (fn [acc report]
                    (let [mid   (:report/message-id report)
                          votes (get av (:db/id report))]
                      (if (seq votes)
                        (let [grouped (group-by :value votes)
                              fmt     (fn [vs]
                                        (mapv (fn [{:keys [voter email-mid]}]
                                                {:voter voter :message-id email-mid})
                                              vs))]
                          (assoc acc mid {"+1" (fmt (:up grouped))
                                         "-1" (fmt (:down grouped))
                                         "0"  (fmt (:null grouped))}))
                        acc)))
                  {}
                  reports)]
    (when (seq entries)
      (let [filename (str out-dir "/votes.json")]
        (spit filename (json/generate-string entries {:pretty true}))
        (log/info "Wrote" (count entries) "report(s) with votes to" filename)))))

(defn dump-patches!
  "Export patch files for a single source."
  [reports patches-dir]
  (let [total (reduce (fn [n report]
                        (let [h   (mid-hash (:report/message-id report))
                              dir (io/file patches-dir h)]
                          (.mkdirs dir)
                          (doseq [p (:report/patches report)
                                  ;; patch/filename may be an absolute path;
                                  ;; extract the basename so io/file stays relative.
                                  :let [fname (.getName (io/file (:patch/filename p)))]]
                            (spit (io/file dir fname) (:patch/text p)))
                          (+ n (count (:report/patches report)))))
                      0
                      (filter #(seq (:report/patches %)) reports))]
    (when (pos? total)
      (log/info "Wrote" total "patch file(s)"))))

;; ---------------------------------------------------------------------------
;; Attachment batch fetch (shared by text and event exports)
;; ---------------------------------------------------------------------------

(defn- batch-fetch-attachments
  "Fetch attachment data for a seq of reports. Returns {message-id -> att-email}."
  [reports]
  (let [db (ctx-db)]
    (into {}
          (keep (fn [report]
                  (let [mid (:report/message-id report)]
                    (when-let [att (fetch-attachment-data db mid)]
                      [mid (:report/email att)]))))
          reports)))

(defn dump-text!
  "Export text attachments (text/plain, text/x-log) to text/<mid-hash>/."
  [reports text-dir]
  (let [txt-reports (filter :report/has-text-attachments reports)
        att-cache   (batch-fetch-attachments txt-reports)
        total (reduce (fn [n report]
                        (let [txt-atts (filter #(and (text-attachment? %)
                                                     (:attachment/data %))
                                               (:email/attachments (get att-cache (:report/message-id report))))]
                          (if (seq txt-atts)
                            (let [h   (mid-hash (:report/message-id report))
                                  dir (io/file text-dir h)]
                              (.mkdirs dir)
                              (doseq [att txt-atts]
                                (spit (io/file dir (.getName (io/file (:attachment/filename att))))
                                      (:attachment/data att)))
                              (+ n (count txt-atts)))
                            n)))
                      0
                      txt-reports)]
    (when (pos? total)
      (log/info "Wrote" total "text file(s)"))))

;; ---------------------------------------------------------------------------
;; Event (ICS) export
;; ---------------------------------------------------------------------------

(defn- has-ics-content?
  "True if an announcement report has ICS content (from digest flags)."
  [report]
  (:report/has-ics report))

(defn- extract-vevents
  "Extract VEVENT blocks from ICS text.
  Uses a reluctant quantifier bounded by END:VEVENT to avoid
  catastrophic backtracking on malformed input."
  [text]
  (when (and text (str/includes? text "BEGIN:VEVENT"))
    (re-seq #"(?s)BEGIN:VEVENT(?:(?!BEGIN:VEVENT).)*?END:VEVENT\r?\n?" text)))

(defn dump-events!
  "Export individual .ics files to events/<mid-hash>/ for announcements with ICS."
  [reports events-dir]
  (let [ics-reports (filter #(and (= :announcement (:report/type %))
                                  (has-ics-content? %))
                            reports)
        att-cache   (batch-fetch-attachments ics-reports)
        total (reduce (fn [n report]
                        (let [atts  (:email/attachments (get att-cache (:report/message-id report)))
                              ics-atts (filter #(and (ics-file? (:attachment/filename %))
                                                     (:attachment/data %))
                                               atts)]
                          (if (seq ics-atts)
                            (let [h   (mid-hash (:report/message-id report))
                                  dir (io/file events-dir h)]
                              (.mkdirs dir)
                              (doseq [att ics-atts]
                                (spit (io/file dir (.getName (io/file (:attachment/filename att))))
                                      (:attachment/data att)))
                              (+ n (count ics-atts)))
                            n)))
                      0
                      ics-reports)]
    (when (pos? total)
      (log/info "Wrote" total "ICS event file(s)"))))

(defn dump-events-filtered!
  "Export events.json/org (open) and events-closed.json/org (closed)
  for announcements that have ICS content."
  [reports reports-dir source-name source-map maintainers-map fmts]
  (let [events      (filter #(and (= :announcement (:report/type %))
                                  (has-ics-content? %))
                            reports)
        open-events  (vec (open-reports events))
        closed-events (vec (filter :report/closed events))]
    (when (seq open-events)
      (when (fmts "json")
        (dump-json! open-events reports-dir source-name source-map maintainers-map
                    "events.json"))
      (when (fmts "org")
        (dump-org! open-events reports-dir source-name source-map maintainers-map
                   "events.org" "events"))
      (when (fmts "rss")
        (dump-rss! open-events reports-dir source-name source-map maintainers-map
                   "events.xml" "events")))
    (when (seq closed-events)
      (when (fmts "json")
        (dump-json! closed-events reports-dir source-name source-map maintainers-map
                    "events-closed.json"))
      (when (fmts "org")
        (dump-org! closed-events reports-dir source-name source-map maintainers-map
                   "events-closed.org" "events (closed)"))
      (when (fmts "rss")
        (dump-rss! closed-events reports-dir source-name source-map maintainers-map
                   "events-closed.xml" "events (closed)")))))

(defn- collect-vevents
  "Extract all VEVENT blocks from a seq of reports with ICS content."
  [reports]
  (let [att-cache (batch-fetch-attachments reports)]
    (mapcat (fn [report]
              (let [email (:report/email (get att-cache (:report/message-id report)))
                    atts  (:email/attachments email)
                    att-vevents (->> atts
                                     (filter #(and (ics-file? (:attachment/filename %))
                                                   (:attachment/data %)))
                                     (mapcat #(extract-vevents (:attachment/data %))))
                    body-vevents (extract-vevents (:email/body-text email))]
                (concat att-vevents body-vevents)))
            reports)))

(defn- spit-ics!
  "Write a VCALENDAR file wrapping the given VEVENT blocks."
  [filename source-name cal-name vevents]
  (when (seq vevents)
    (let [ics-content (str "BEGIN:VCALENDAR\r\n"
                           "VERSION:2.0\r\n"
                           "PRODID:-//BARK//Event Export//EN\r\n"
                           "X-WR-CALNAME:" source-name " " cal-name "\r\n"
                           (str/join "" vevents)
                           "END:VCALENDAR\r\n")]
      (spit filename ics-content)
      (log/info "Wrote" (count vevents) "VEVENT(s) to" filename))))

(defn dump-events-ics!
  "Export combined ICS files for announcements with VEVENT content:
   announcements.ics (all), -open.ics, -closed.ics."
  [reports events-dir source-name]
  (let [all-events    (filter #(and (= :announcement (:report/type %))
                                    (has-ics-content? %))
                              reports)
        open-events   (remove :report/closed all-events)
        closed-events (filter :report/closed all-events)]
    (spit-ics! (str events-dir "/announcements.ics")
               source-name "events" (collect-vevents all-events))
    (spit-ics! (str events-dir "/announcements-open.ics")
               source-name "events (open)" (collect-vevents open-events))
    (spit-ics! (str events-dir "/announcements-closed.ics")
               source-name "events (closed)" (collect-vevents closed-events))))

(defn dump-html!
  "Generate index.html for a single source.
  Uses all-open.json so only open reports are server-rendered;
  closed reports are lazy-loaded by the client from all-closed.json."
  [base-dir reports-dir cli-args]
  (let [json-file (str reports-dir "/all-open.json")]
    (apply process/shell "bb" "scripts/bark-index.clj"
           "-o" (str base-dir "/index.html")
           "--json" json-file
           "--dir" reports-dir
           cli-args)))

(defn dump-stats!
  "Generate stats for a single source."
  [base-dir reports-dir source-name format cli-args]
  (let [dir      (if (= format "html") base-dir reports-dir)
        out-file (str dir (if (= format "html") "/data.html" "/stats.json"))]
    (apply process/shell "bb" "scripts/bark-stats.clj"
           (if (= format "html") "html" "json")
           "-o" out-file
           "-n" source-name
           cli-args)))

(defn dump-docs!
  "Generate docs.html for a single source."
  [base-dir source-name cli-args]
  (apply process/shell (cond-> ["bb" "scripts/bark-docs.clj"
                                "-o" (str base-dir "/docs.html")
                                "--dir" base-dir]
                         source-name (into ["-n" source-name])
                         true (into cli-args))))

;; ---------------------------------------------------------------------------
;; Per-type export
;; ---------------------------------------------------------------------------

(def default-export-formats #{"json" "org" "rss"})

(defn- resolve-export-formats
  "Return the set of export formats for a source.
  Per-source :export-formats in source-map, or the global default."
  [source-name source-map]
  (or (get-in source-map [source-name :export-formats])
      default-export-formats))

(defn- resolve-export-reports
  "Return the set of report type keywords to export for a source,
  or nil (meaning all types)."
  [source-name source-map]
  (get-in source-map [source-name :export-reports]))

(defn- resolve-source-topics-filter
  "Return the topics filter for a source: CLI override > per-source > global.
  Returns a set of lower-cased topic strings, or nil (meaning no filter)."
  [source-name source-map cli-topics-filter]
  (or cli-topics-filter
      (resolve-topics-filter (get-in source-map [source-name :topics-filter]))))

(defn- dump-per-type!
  "Export per-type JSON, Org, and RSS files for all report types present.
  When `changed-types` is non-nil, only re-export files for those types."
  [reports reports-dir source-name source-map maintainers-map fmts
   & {:keys [changed-types]}]
  (doseq [rtype report-types
          :let [typed (filter-reports reports {:type rtype})
                plural (type->plural rtype)]
          :when (and (seq typed)
                     (or (nil? changed-types) (changed-types rtype)))]
    (when (fmts "json")
      (dump-json! typed reports-dir source-name source-map maintainers-map
                  (str plural ".json")))
    (when (fmts "org")
      (dump-org! typed reports-dir source-name source-map maintainers-map
                 (str plural ".org") plural))
    (when (fmts "rss")
      (dump-rss! typed reports-dir source-name source-map maintainers-map
                 (str plural ".xml") plural))))

(defn- dump-open-closed!
  "Export open/closed split files and meta.json with summary counts.
  all-open.json is loaded by index.html on first paint (fast).
  all-closed.json is lazy-loaded when user deactivates the Open filter.
  meta.json contains summary counts per type, used by data.html for KPIs.
  Produces per-type -open and -closed files in all enabled formats.
  When `changed-types` is non-nil, only re-export per-type files for those types
  (the aggregate all-open/all-closed and meta.json are always regenerated)."
  [reports reports-dir source-name source-map maintainers-map fmts
   & {:keys [changed-types]}]
  (let [open       (vec (open-reports reports))
        closed     (vec (filter :report/closed reports))
        counts     {:total        (count reports)
                    :open-count   (count open)
                    :closed-count (count closed)}
        ;; Per-type breakdown for meta.json
        by-type    (group-by :report/type reports)
        type-counts (into {}
                          (map (fn [[t rs]]
                                 [(name t) {:total  (count rs)
                                            :open   (count (remove :report/closed rs))
                                            :closed (count (filter :report/closed rs))}]))
                          by-type)
        tenures    (when-let [db (ctx-db)] (get-tenures db source-name))
        meta-data  (merge counts
                          {:bark-format bark-format
                           :source      source-name
                           :generated   (str (java.util.Date.))
                           :by-type     type-counts
                           :maintainers (tenures-snapshot (or tenures []))}
                          (source-metadata source-name source-map))]
    ;; meta.json
    (spit (str reports-dir "/meta.json")
          (json/generate-string meta-data {:pretty true}))
    (log/info "Wrote meta.json")
    ;; --- Open ---
    (dump-json! open reports-dir source-name source-map maintainers-map
                "all-open.json" counts)
    (when (fmts "rss")
      (dump-rss! open reports-dir source-name source-map maintainers-map
                 "all-open.xml" "open reports"))
    (when (fmts "org")
      (dump-org! open reports-dir source-name source-map maintainers-map
                 "all-open.org" "open reports"))
    ;; --- Closed ---
    (dump-json! closed reports-dir source-name source-map maintainers-map
                "all-closed.json" counts)
    (when (fmts "rss")
      (dump-rss! closed reports-dir source-name source-map maintainers-map
                 "all-closed.xml" "closed reports"))
    (when (fmts "org")
      (dump-org! closed reports-dir source-name source-map maintainers-map
                 "all-closed.org" "closed reports"))
    ;; --- Per-type open & closed (skip unchanged types when incremental) ---
    (doseq [rtype report-types
            :when (or (nil? changed-types) (changed-types rtype))
            :let [plural (type->plural rtype)
                  t-open   (filter-reports open {:type rtype})
                  t-closed (filter-reports closed {:type rtype})]]
      (when (seq t-open)
        (when (fmts "json")
          (dump-json! t-open reports-dir source-name source-map maintainers-map
                      (str plural "-open.json")))
        (when (fmts "rss")
          (dump-rss! t-open reports-dir source-name source-map maintainers-map
                     (str plural "-open.xml") (str plural " (open)")))
        (when (fmts "org")
          (dump-org! t-open reports-dir source-name source-map maintainers-map
                     (str plural "-open.org") (str plural " (open)"))))
      (when (seq t-closed)
        (when (fmts "json")
          (dump-json! t-closed reports-dir source-name source-map maintainers-map
                      (str plural "-closed.json")))
        (when (fmts "rss")
          (dump-rss! t-closed reports-dir source-name source-map maintainers-map
                     (str plural "-closed.xml") (str plural " (closed)")))
        (when (fmts "org")
          (dump-org! t-closed reports-dir source-name source-map maintainers-map
                     (str plural "-closed.org") (str plural " (closed)")))))))

;; ---------------------------------------------------------------------------
;; Root index — public/index.html listing all sources
;; ---------------------------------------------------------------------------

(defn- load-source-meta
  "Read reports/meta.json for a source dir, or nil on failure."
  [base-dir]
  (let [f (io/file base-dir "reports" "meta.json")]
    (when (.exists f)
      (try (json/parse-string (slurp f) true)
           (catch Exception _ nil)))))

(defn dump-root-index!
  "Generate public/index.html listing all exported sources.
  Reads each source's reports/meta.json for summary counts."
  [source-names _source-map]
  (let [rows (for [src-name source-names
                   :let [slug     (slugify src-name)
                         base-dir (str "public/" slug)
                         meta     (load-source-meta base-dir)]
                   :when meta]
               {:name         src-name
                :slug         slug
                :total        (or (:total meta) 0)
                :open         (or (:open-count meta) 0)
                :closed       (or (:closed-count meta) 0)
                :list-archive (:list-archive meta)})
        row-html
        (fn [{:keys [name slug total open closed list-archive]}]
          (str "<tr>"
               "<td><a href=\"" slug "/index.html\">" (xml-escape name) "</a>"
               (when list-archive
                 (str " <a class=\"archive\" href=\"" (xml-escape list-archive)
                      "\" title=\"List archive\">↗</a>"))
               "</td>"
               "<td class=\"num\">" open "</td>"
               "<td class=\"num\">" closed "</td>"
               "<td class=\"num\">" total "</td>"
               "<td class=\"num feeds\">"
               "<a href=\"" slug "/reports/all.xml\">RSS</a> · "
               "<a href=\"" slug "/reports/all.json\">JSON</a>"
               "</td>"
               "</tr>\n"))
        page
        (str
         "<!DOCTYPE html>\n<html lang=\"en\">\n"
         (html-head {:title "BARK — Sources"
                     :css (str "table{margin-top:1.5rem}"
                               "td.num,th.num{text-align:right}"
                               "a.archive{font-size:0.82rem;margin-left:0.4rem;opacity:0.7}"
                               ".feeds{font-size:0.82rem;white-space:nowrap}"
                               ".theme-toggle{cursor:pointer;background:none;border:none;font-size:1.2rem;padding:0.3rem}"
                               footer-css)})
         "<body>\n<main class=\"container\">\n"
         "<nav><ul><li><strong>BARK</strong></li></ul>"
         "<ul><li><button class=\"theme-toggle\" onclick=\"toggleTheme()\" "
         "aria-label=\"Toggle theme\"><span id=\"theme-icon\">🌙</span>"
         "</button></li></ul></nav>\n"
         "<table role=\"grid\">\n"
         "<thead><tr>"
         "<th>Source</th>"
         "<th class=\"num\">Open</th>"
         "<th class=\"num\">Closed</th>"
         "<th class=\"num\">Total</th>"
         "<th class=\"num\">Feeds</th>"
         "</tr></thead>\n<tbody>\n"
         (apply str (map row-html rows))
         "</tbody></table>\n"
         "<footer class=\"bark-footer\">"
         "<a href=\"" bark-repo-url "\">BARK</a> is "
         "<a href=\"https://www.gnu.org/philosophy/free-sw.html\">Free Software</a>"
         "</footer>\n"
         "<script>\n" (wrap-js theme-toggle-js) "\n</script>\n"
         "</main>\n</body>\n</html>\n")]
    (spit-html "public/index.html" page)
    (log/info "Wrote public/index.html with" (count rows) "source(s)")))

(defn export-source!
  "Export a single source in the given format(s).
  Always produces all-open.json and all-closed.json (used by index.html).
  When format is \"all\", per-type feeds respect :export-formats from config.
  `changed-types` (optional set of keywords) limits per-type file regeneration
  to those types during incremental export; aggregate files are always rebuilt."
  [format reports base-dir source-name source-map maintainers-map cli-extra
   & {:keys [changed-types]}]
  (let [reports-dir (str base-dir "/reports")
        patches-dir (str base-dir "/patches")
        events-dir  (str base-dir "/events")
        text-dir    (str base-dir "/text")
        _           (doseq [d [reports-dir patches-dir events-dir text-dir]]
                      (.mkdirs (io/file d)))
        ef          (resolve-export-formats source-name source-map)
        do-format
        (fn [fmt]
          (case fmt
            "json"    (do (dump-json! reports reports-dir source-name source-map maintainers-map)
                          (dump-votes! reports reports-dir)
                          (dump-per-type! reports reports-dir source-name source-map maintainers-map #{"json"})
                          (dump-open-closed! reports reports-dir source-name source-map maintainers-map #{"json"}))
            "rss"     (do (dump-rss!  reports reports-dir source-name source-map maintainers-map)
                          (dump-per-type! reports reports-dir source-name source-map maintainers-map #{"rss"}))
            "org"     (do (dump-org!  reports reports-dir source-name source-map maintainers-map)
                          (dump-per-type! reports reports-dir source-name source-map maintainers-map #{"org"})
                          (dump-open-closed! reports reports-dir source-name source-map maintainers-map #{"org"}))
            "patches" (dump-patches! reports patches-dir)
            "text"    (dump-text! reports text-dir)
            "events"  (do (dump-events! reports events-dir)
                          (dump-events-filtered! reports reports-dir source-name source-map maintainers-map ef)
                          (dump-events-ics! reports events-dir source-name))
            "html"    (do (dump-json! reports reports-dir source-name source-map maintainers-map)
                          (dump-votes! reports reports-dir)
                          (dump-per-type! reports reports-dir source-name source-map maintainers-map #{"json"})
                          (dump-open-closed! reports reports-dir source-name source-map maintainers-map #{"json"})
                          (dump-docs! base-dir source-name cli-extra)
                          (dump-html! base-dir reports-dir cli-extra))
            "stats"   (dump-stats! base-dir reports-dir source-name "json" cli-extra)))]
    (if (= format "all")
      (do (when (ef "json") (dump-json! reports reports-dir source-name source-map maintainers-map))
          ;; Votes are only consumed by JSON and HTML outputs.
          (when (or (ef "json") (ef "html")) (dump-votes! reports reports-dir))
          (when (ef "rss")  (dump-rss!  reports reports-dir source-name source-map maintainers-map))
          (when (ef "org")  (dump-org!  reports reports-dir source-name source-map maintainers-map))
          (dump-per-type! reports reports-dir source-name source-map maintainers-map ef
                          :changed-types changed-types)
          (dump-open-closed! reports reports-dir source-name source-map maintainers-map ef
                             :changed-types changed-types)
          (dump-patches! reports patches-dir)
          (dump-text! reports text-dir)
          (dump-events! reports events-dir)
          (dump-events-filtered! reports reports-dir source-name source-map maintainers-map ef)
          (dump-events-ics! reports events-dir source-name)
          (dump-docs! base-dir source-name cli-extra)
          (dump-html! base-dir reports-dir cli-extra)
          (dump-stats! base-dir reports-dir source-name "json" cli-extra)
          (dump-stats! base-dir reports-dir source-name "html" cli-extra))
      (do-format format))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(def formats #{"json" "rss" "org" "html" "all" "stats" "patches" "text" "events" "root"})

(let [{:keys [format source-name min-priority min-status force-all? theme page-size closed-retention
              topics-filter]
       :or {format "all"}}
      (parse-cli-args *command-line-args*)
      db-path (or (System/getenv "BARK_DB") "data/bark-db")
      conn    (d/get-conn db-path bark-schema {:wal? false})]
  (try
    (when-not (formats format)
      (log/error "Unknown format:" format)
      (log/error "Formats: json rss org html stats patches text events root all")
      (System/exit 1))
    (when (and min-priority (not (#{1 2 3} min-priority)))
      (log/error "Invalid --min-priority:" min-priority "(must be 1, 2, or 3)")
      (System/exit 1))
    (when (and min-status (not (<= 1 min-status 7)))
      (log/error "Invalid --min-status:" min-status "(must be 1–7)")
      (System/exit 1))
    (let [db              (d/db conn)
          last-modified   (get-last-modified db)
          last-export     (get-last-export)
          incremental?    (and (not force-all?)
                               (= format "all")
                               last-export last-modified)
          skip?           (and incremental?
                               (<= (.getTime ^java.util.Date last-modified)
                                   (.getTime ^java.util.Date last-export)))]
      (if skip?
        (log/info "Nothing changed since last export, skipping.")
        ;; Resolve config and source list *before* the expensive DB pull
        ;; so we can determine which sources actually need re-export.
        (let [config          (load-config)
              effective-theme (or theme (:theme config))
              _               (when effective-theme (set-theme! effective-theme))
              source-map      (if config (build-source-map config) {})
              source-names    (if source-name
                                (if (contains? source-map source-name)
                                  [source-name]
                                  (do (log/error "No source named" (str "'" source-name "'"))
                                      (log/error "Available:"
                                                 (str/join ", " (keys source-map)))
                                      (System/exit 1)))
                                (mapv :name (:sources config)))
              ;; Per-source, per-type change detection: {source -> #{types}}.
              ;; Enables both source-level skip and intra-source per-type skip.
              changed-st      (when (and incremental? last-export)
                                (changed-source-types-since db last-export))
              export-names    (if (and incremental? (seq changed-st))
                                (filterv (fn [s] (contains? changed-st s)) source-names)
                                source-names)]
          (when (and incremental? (seq changed-st))
            (log/info "Incremental: changed sources:"
                      (str/join ", " (map (fn [[s ts]] (str s " (" (str/join " " (map name ts)) ")"))
                                          changed-st))))
          (when (and incremental? (seq changed-st) (< (count export-names) (count source-names)))
            (let [skipped (remove (set export-names) source-names)]
              (doseq [s skipped]
                (log/info (str "[" s "]") "no changes, skipping."))))
          ;; Only load all reports and votes when there is actual work to do.
          ;; Track whether any source was actually exported so we can skip
          ;; the root index when nothing changed.
          (let [any-exported?
                (if (and (not= format "root") (seq export-names))
                  (let [maintainers-map (if config (build-maintainers db source-map) {})
                        all-reps        (all-reports-by-date db)
                        _               (init-export-context!
                                          db
                                          (votes-by-report
                                           (d/q '[:find ?r ?val ?voter ?emid
                                                  :where
                                                  [?v :vote/report ?r]
                                                  [?v :vote/value  ?val]
                                                  [?v :vote/voter  ?voter]
                                                  [?v :vote/email  ?e]
                                                  [?e :email/message-id ?emid]]
                                                db))
                                          config)
                        effective-ps    (or page-size (:page-size config))
                        cli-tf          (resolve-topics-filter topics-filter)
                        drop-cutoff     (resolve-closed-retention-date
                                         (or closed-retention (:closed-retention config)))
                        _               (when cli-tf
                                          (log/info "CLI topics filter:" (str/join ", " cli-tf)))
                        _               (when drop-cutoff
                                          (log/info "Dropping reports closed before" drop-cutoff))
                        cli-extra       (let [drop (disj (hash-set format "-n" source-name
                                                                   "--force" "--theme" theme
                                                                   "--page-size" (some-> page-size str)
                                                                   "--closed-retention" closed-retention
                                                                   "--topics-filter" topics-filter) nil)]
                                          (cond-> (vec (remove drop (rest *command-line-args*)))
                                            effective-theme (into ["--theme" effective-theme])
                                            effective-ps    (into ["--page-size" (str effective-ps)])))]
                    (reduce (fn [exported? src-name]
                              (let [reports  (filter-reports all-reps {:source       src-name
                                                                       :min-priority min-priority
                                                                       :min-status   min-status})
                                    er       (resolve-export-reports src-name source-map)
                                    reports  (if er (filter #(contains? er (:report/type %)) reports) reports)
                                    reports  (if drop-cutoff (drop-old-closed reports drop-cutoff) reports)
                                    src-tf   (resolve-source-topics-filter src-name source-map cli-tf)
                                    _        (when src-tf
                                               (log/info (str "[" src-name "]") "topics filter:" (str/join ", " src-tf)))
                                    reports  (filter-by-topics reports src-tf)
                                    base-dir (str "public/" (slugify src-name))
                                    src-changed (when (seq changed-st) (get changed-st src-name))]
                                (if (empty? reports)
                                  (do (log/info "No reports for source" (str "'" src-name "'") ", skipping.")
                                      exported?)
                                  (do (log/info (str "[" src-name "] " (count reports) " report(s)"
                                                     " (" (count (open-reports reports)) " open)"
                                                     (if incremental? " (incremental)" "")))
                                      (export-source! format reports base-dir src-name
                                                      source-map maintainers-map cli-extra
                                                      :changed-types src-changed)
                                      true))))
                            false export-names))
                  false)]
            ;; Only regenerate root index when at least one source was exported,
            ;; or when explicitly requested via "bb export root".
            (when (and (#{"all" "root"} format)
                       (or (= format "root") any-exported?))
              (dump-root-index! source-names source-map)))
          (save-last-export! (java.util.Date.)))))
    (finally
      (d/close conn))))
