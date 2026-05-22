#!/usr/bin/env bb

;; bark-export.clj -- Export BARK reports per source.
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
;;   bb export               -- incremental export (skip if nothing changed)
;;   bb export json          -- export all.json for each source
;;   bb export rss           -- export all.xml for each source
;;   bb export org           -- export all.org for each source
;;   bb export html          -- generate index.html for each source
;;   bb export stats         -- generate stats.json for each source
;;   bb export patches       -- export patch files for each source
;;   bb export events        -- export ICS event files and events.ics for each source
;;   bb export text          -- export text/plain and text/x-log attachments
;;   bb export root          -- regenerate public/index.html (source listing)
;;   bb export all           -- all formats (still incremental)
;;   bb export --force       -- force full export, ignore timestamps
;;   bb export --only-open   -- also export -open files (all-open.json, etc.)
;;   bb export json -n src   -- export only source "src"
;;   bb export json -p 2     -- only priority >= 2
;;   bb export json -s 3     -- only status >= 3
;;
;; Environment / defaults:
;;   BARK_DB -- path to db (default: ./data/bark-db)

(ns bark-export
  (:require [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [taoensso.timbre :as log]
            [bark.common :refer [get-header slugify mid-hash
                                 format-date format-date-iso
                                 report-priority report-status report-descendant-count
                                 parse-cli-args parse-delay parse-cutoff-date
                                 load-config load-mailmap db-path build-source-map
                                 bark-schema bark-format
                                 report-type-spec type->plural
                                 votes-by-report vote-counts
                                 ics-file? text-attachment?]]
            [bark.common-bb :refer [load-datalevin-pod! all-reports dq
                                    fetch-attachment-data get-tenures tenures-snapshot
                                    get-last-modified changed-source-types-since]]
            [bark.html-bb :refer [set-theme! html-head footer-css bark-footer
                                  wrap-js spit-html theme-toggle-js nav-bar]]
            [hiccup2.core :as h]))

(load-datalevin-pod!)

;; ---------------------------------------------------------------------------
;; File-based export timestamp (replaces DB-based save-last-export!)
;; The export is fully read-only w.r.t. the database.
;; ---------------------------------------------------------------------------

(def ^:private last-export-file "data/.last-export")

(defn- get-last-export
  "Read the last export timestamp from data/.last-export, or nil.
  A corrupt file forces a full export on the next run -- log so the
  operator knows why."
  []
  (let [f (io/file last-export-file)]
    (when (.exists f)
      (try (java.util.Date. ^long (parse-long (str/trim (slurp f))))
           (catch Exception e
             (log/warn "Could not parse" last-export-file "-- forcing full export."
                       (.getMessage e))
             nil)))))

(defn- save-last-export!
  "Write the export timestamp to data/.last-export."
  [^java.util.Date ts]
  (io/make-parents last-export-file)
  (spit last-export-file (str (.getTime ts))))

;; ---------------------------------------------------------------------------
;; Atomic export via staging directory
;; ---------------------------------------------------------------------------

(defn- delete-dir!
  "Recursively delete a directory and its contents."
  [^java.io.File dir]
  (when (.exists dir)
    (doseq [f (reverse (file-seq dir))]
      (.delete ^java.io.File f))))

(defn- move-dir!
  "Move `src` to `dst` using Files/move. Tries ATOMIC_MOVE first,
  falls back to a plain move if the filesystem does not support it."
  [^java.io.File src ^java.io.File dst]
  (try
    (java.nio.file.Files/move (.toPath src) (.toPath dst)
                              (into-array java.nio.file.CopyOption
                                          [java.nio.file.StandardCopyOption/ATOMIC_MOVE]))
    (catch Exception _
      (java.nio.file.Files/move (.toPath src) (.toPath dst)
                                (into-array java.nio.file.CopyOption
                                            [java.nio.file.StandardCopyOption/REPLACE_EXISTING])))))

(defn- copy-dir!
  "Recursively copy `src` into `dst`, creating `dst` if needed.
  Used to seed the staging directory with the previous target so an
  incremental export does not lose files for unchanged report types.
  Relies on `file-seq`'s pre-order traversal so each directory is
  created before its children are copied."
  [^java.io.File src ^java.io.File dst]
  (when (.exists src)
    (let [src-path (.toPath src)
          dst-path (.toPath dst)
          opts     (into-array java.nio.file.CopyOption
                               [java.nio.file.StandardCopyOption/REPLACE_EXISTING])]
      (doseq [^java.io.File f (file-seq src)
              :let [target (.resolve dst-path (.relativize src-path (.toPath f)))]]
        (if (.isDirectory f)
          (.mkdirs (.toFile target))
          (java.nio.file.Files/copy (.toPath f) target opts))))))

(defn- clean-stale-old-dirs!
  "Remove any leftover `<target>.old-*` directories from a previous
  crashed swap so they don't accumulate."
  [^java.io.File target]
  (let [parent (.getParentFile target)
        prefix (str (.getName target) ".old-")]
    (when (.isDirectory parent)
      (doseq [^java.io.File f (.listFiles parent)
              :when (and (.isDirectory f)
                         (str/starts-with? (.getName f) prefix))]
        (delete-dir! f)))))

(defn- atomic-swap-dir!
  "Replace `target-dir` with `staging-dir`.
  Moves the old target to a temp name, moves staging into place,
  then deletes the old directory.  On failure of the second move,
  restores the previous target so callers never see a missing dir."
  [staging-dir target-dir]
  (let [target      (io/file target-dir)
        staging     (io/file staging-dir)
        old         (io/file (str target-dir ".old-" (System/currentTimeMillis)))
        had-target? (.exists target)]
    (clean-stale-old-dirs! target)
    (when had-target?
      (move-dir! target old))
    (try
      (move-dir! staging target)
      (when had-target?
        (delete-dir! old))
      (catch Exception e
        (when (and had-target? (.exists old) (not (.exists target)))
          (try (move-dir! old target)
               (catch Exception _
                 (log/error "Could not restore previous target from"
                            (.getAbsolutePath old)))))
        (throw e)))))

;; ---------------------------------------------------------------------------
;; --closed-retention: resolve a date or duration to a cutoff java.util.Date.
;; Reports closed before that date are excluded from export.
;; ---------------------------------------------------------------------------

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
  "Keep only reports whose :report/topic-value matches one of the
  given topics (case-insensitive).  Returns all reports when `topics`
  is nil."
  [reports topics]
  (if topics
    (filter (fn [r]
              (when-let [t (:report/topic-value r)]
                (topics (str/lower-case t))))
            reports)
    reports))

;; ---------------------------------------------------------------------------
;; DB queries
;; ---------------------------------------------------------------------------

(defn all-reports-by-date [db]
  (let [epoch (java.util.Date. 0)]
    (sort-by #(or (get-in % [:report/email :email/date-sent]) epoch)
             #(compare %2 %1)
             (all-reports db))))

;; ---------------------------------------------------------------------------
;; Formatting helpers
;; ---------------------------------------------------------------------------

(defn- attachment-basename
  "Return the basename of an attachment filename (handles absolute paths)."
  [att]
  (.getName (io/file (:attachment/filename att))))

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

(defn- votes-str
  "Format vote counts as \"score/total\" from a seq of vote maps, or nil."
  [votes]
  (when (seq votes)
    (let [{:keys [up down null]} (vote-counts votes)
          total (+ up down null)]
      (when (pos? total)
        (str (- up down) "/" total)))))

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

(defn- build-author-names
  "Build a {lowercased-addr -> latest-known author-name} map by scanning
  all emails.  Used to display role-bearing addresses (owner, etc.) by
  display name in exports rather than as bare email local-parts."
  [db]
  (let [pairs  (dq '[:find ?addr ?name ?date
                     :where
                     [?e :email/author-address ?addr]
                     [?e :email/author-name ?name]
                     [?e :email/date-sent ?date]]
                   db)
        latest (reduce (fn [m [addr name date]]
                         (let [k    (str/lower-case addr)
                               prev (get m k)]
                           (if (or (nil? prev)
                                   (.after ^java.util.Date date ^java.util.Date (second prev)))
                             (assoc m k [name date])
                             m)))
                       {} pairs)]
    (update-vals latest first)))

;; ---------------------------------------------------------------------------
;; Report -> map
;; ---------------------------------------------------------------------------

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
  [[:report/acked-address     :acked]
   [:report/owned-address     :owned]
   [:report/closed-address    :closed]
   [:report/urgent-address    :urgent]
   [:report/important-address :important]])

(def ^:private proxy-address-pairs
  "Triplets [ref-attr address-key proxy-key] for extracting the maintainer
  author-address.  The proxy key is only emitted when it differs from the
  corresponding address key (i.e. a `-by` directive pointed the attribute
  at someone other than the pose email's sender)."
  [[:report/acked     :acked     :acked-proxy]
   [:report/owned     :owned     :owned-proxy]
   [:report/closed    :closed    :closed-proxy]
   [:report/urgent    :urgent    :urgent-proxy]
   [:report/important :important :important-proxy]])

(defn- assoc-owned-name
  "Attach :owned-name when the owner's display name is known (looked up
  via the global author-names map built once per export)."
  [m author-names]
  (if-let [owned (:owned m)]
    (if-let [nm (get author-names (str/lower-case owned))]
      (assoc m :owned-name nm)
      m)
    m))

(defn- assoc-from-addresses
  "Extract addresses from report: direct string attrs and author-address of ref attrs.
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
                (let [addr (:email/author-address v)]
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
             (mapv (fn [att] {:file (str h "/" (attachment-basename att))})
                   (filter #(ics-file? (:attachment/filename %))
                           (:email/attachments @att-email))))
      (:report/has-text-attachments report)
      (assoc :texts
             (mapv (fn [att] {:file (str h "/" (attachment-basename att))})
                   (filter #(text-attachment? %)
                           (:email/attachments @att-email)))))))

;; ---------------------------------------------------------------------------
;; Export context -- bound once per export run via with-export-context.
;; Used by map-reports, dump-events!, dump-text!, and collect-vevents
;; to access the DB and votes without threading parameters through
;; every dump-* function.
;; ---------------------------------------------------------------------------

(def ^:private export-ctx
  "Export context: {:db <datalevin-db> :votes {eid -> [vote-maps]}
  :config <config> :author-names {lc-addr -> display-name}}"
  (atom {:db nil :votes {} :config nil :author-names {}}))

(defn- set-export-context! [db votes config]
  (reset! export-ctx {:db           db
                      :votes        votes
                      :config       config
                      :author-names (merge (build-author-names db)
                                           (load-mailmap))}))

(defn- ctx-db [] (:db @export-ctx))
(defn- ctx-votes [] (:votes @export-ctx))
(defn- ctx-config [] (:config @export-ctx))
(defn- ctx-author-names [] (:author-names @export-ctx))

(def ^:private default-awaiting-delay-days 14)

(defn- awaiting-reply?
  "True when a report is open, last activity was by a maintainer,
  the OP is not a maintainer, and the configured delay has elapsed.
  When the OP is themselves a maintainer, \"awaiting reply\" makes no
  semantic sense -- we are not waiting for a maintainer to answer their
  own thread."
  [report source-name source-map maintainers-map]
  (when (and (not (:report/closed report))
             (:report/last-activity report)
             (:report/last-activity-address report))
    (let [last-addr (str/lower-case (:report/last-activity-address report))
          op-addr   (some-> (get-in report [:report/email :email/author-address])
                            str/lower-case)
          last-role (sender-role last-addr source-name source-map maintainers-map)
          op-role   (when op-addr
                      (sender-role op-addr source-name source-map maintainers-map))]
      (when (and last-role (not op-role))
        (let [config     (ctx-config)
              src-cfg    (get source-map source-name)
              delay-str  (or (:awaiting-delay src-cfg)
                             (:awaiting-delay config))
              delay-days (if delay-str (parse-delay delay-str) default-awaiting-delay-days)
              ^java.util.Date last-act (:report/last-activity report)
              elapsed-ms  (- (System/currentTimeMillis) (.getTime last-act))
              elapsed-days (/ elapsed-ms (* 24 60 60 1000))]
          (>= elapsed-days delay-days))))))

(defn- group-relations
  "Build the per-kind relation summary for a pulled report.
  Outgoing relations (:rel/_from) cover all kinds; incoming :related-to
  (:rel/_to) is added because the symmetric kind canonicalises to the
  smaller-eid side, so the other report only sees it via incoming."
  [report src-type]
  (let [self-eid   (:db/id report)
        archive    (fn [other-r]
                     (when-not (#{:alias :mailbox} src-type)
                       (archived-at (:report/email other-r))))
        format-rel (fn [from-side? rel]
                     (let [other (if from-side? (:rel/to rel) (:rel/from rel))
                           a     (archive other)
                           subj  (get-in other [:report/email :email/subject])]
                       (cond-> {:message-id (:report/message-id other)}
                         (:report/type other) (assoc :type (name (:report/type other)))
                         subj                 (assoc :subject subj)
                         a                    (assoc :archived-at a)
                         (:rel/setter rel)    (assoc :setter (:rel/setter rel))
                         (:rel/posed-at rel)  (assoc :posed-at (format-date-iso (:rel/posed-at rel)))
                         (:rel/value rel)     (assoc :value (:rel/value rel)))))
        active-not-self (fn [other-key]
                          (filter #(and (:rel/active? %)
                                        (not= self-eid (:db/id (other-key %))))))
        out        (sequence (comp (active-not-self :rel/to)
                                   (map (juxt :rel/kind #(format-rel true %))))
                             (:rel/_from report))
        in-related (sequence (comp (active-not-self :rel/from)
                                   (filter #(= :related-to (:rel/kind %)))
                                   (map (juxt :rel/kind #(format-rel false %))))
                             (:rel/_to report))]
    (reduce-kv (fn [m k entries]
                 (assoc m (keyword (name k)) (mapv second entries)))
               {}
               (group-by first (concat out in-related)))))

(defn report->map [report source-map maintainers-map report-votes db]
  (let [email       (:report/email report)
        source-name (:email/source email)
        att-data    (delay (when db
                     (fetch-attachment-data db (:report/message-id report))))
        att-email   (delay (:report/email @att-data))
        from        (or (:email/author-address email) "")
        from-name   (or (get (ctx-author-names) (str/lower-case from))
                        (:email/author-name email))
        arch        (archive-url report email source-map)
        src-type    (get-in source-map [source-name :source-type])
        relations   (group-relations report src-type)
        ;; First :supersedes outgoing (kept as a singular convenience field)
        first-sup   (first (:supersedes relations))
        role        (sender-role from source-name source-map maintainers-map)
        awaiting?   (awaiting-reply? report source-name source-map maintainers-map)]
    (-> {:type     (name (:report/type report))
         :subject  (or (:email/subject email) "")
         :from     from
         :date     (format-date (:email/date-sent email))
         ;; Keep the Date object itself for internal formatters (RSS, Org).
         ;; Cheshire serializes java.util.Date to ISO-8601 in JSON output.
         :date-raw (:email/date-sent email)
         :flags    (flags-str report)
         :status   (report-status report)
         :priority (report-priority report)
         :replies  (report-descendant-count report)}
        (assoc-from-addresses report)
        (assoc-owned-name (ctx-author-names))
        (cond->
         from-name                       (assoc :from-name from-name)
          role                            (assoc :role role)
          (:report/message-id report)     (assoc :message-id (:report/message-id report))
          (:report/version report)        (assoc :version (:report/version report))
          (:report/topic-value report)    (assoc :topic (:report/topic-value report))
          (:report/patch-seq report)      (assoc :patch-seq (:report/patch-seq report))
          (:report/patch-source report)   (assoc :patch-source (mapv name (sort (:report/patch-source report))))
          arch                            (assoc :archived-at arch)
          (:report/deadline-value report) (assoc :deadline (format-date-iso (:report/deadline-value report)))
          (:report/last-activity report)  (assoc :last-activity (format-date-iso (:report/last-activity report)))
          awaiting?                      (assoc :awaiting true)
          (:report/expiry-value report)   (assoc :expiry (format-date-iso (:report/expiry-value report)))
          (:report/close-reason report)   (assoc :close-reason (name (:report/close-reason report)))
          ;; Singular :superseded-by convenience field -- first outgoing
          ;; :supersedes if any.  Subject is also exposed under
          ;; :rel/to.{:report/email :email/subject}.
          first-sup
          (assoc :superseded-by
                 (cond-> {:message-id (:message-id first-sup)}
                   (get-in first-sup [:subject])
                   (assoc :subject (:subject first-sup))))
          (and (= :expired (:report/close-reason report))
               (:email/date-sent (:report/closed report)))
          (assoc :expired-date (format-date-iso (:email/date-sent (:report/closed report)))))
        (merge (report-vote-fields report-votes))
        (cond->
          (:report/series report) (assoc :series (report-series-fields (:report/series report))))
        ;; Qualified relations exported as one field per kind.
        (merge relations)
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

(def report-types
  "Ordered keywords of every report type, driven by `common/report-type-spec`."
  (mapv :type report-type-spec))

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

(def ^:private rfc822-formatter
  ;; DateTimeFormatter is immutable and thread-safe.  Pattern matches
  ;; SimpleDateFormat's "EEE, dd MMM yyyy HH:mm:ss Z" output (e.g.
  ;; "Mon, 12 May 2026 14:30:45 +0000") -- do not substitute with
  ;; RFC_1123_DATE_TIME, which emits "GMT" and a non-padded day.
  (-> (java.time.format.DateTimeFormatter/ofPattern "EEE, dd MMM yyyy HH:mm:ss Z")
      (.withLocale java.util.Locale/ENGLISH)
      (.withZone java.time.ZoneOffset/UTC)))

(defn- rfc822-date
  "Format a java.util.Date as an RFC 822 date string (for RSS)."
  [^java.util.Date d]
  (when d
    (.format rfc822-formatter (.toInstant d))))

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
                  "    <title>BARK " (xml-escape source-name) " " (xml-escape feed-label) "</title>\n"
                  "    <link>" (xml-escape list-url) "</link>\n"
                  "    <description>Reports from the Bug And Report Keeper</description>\n"
                  items "\n"
                  "  </channel>\n"
                  "</rss>\n"))
       (log/info "Wrote" (count data) "reports to" filename)))))

(defn- format-org-inactive-ts
  "Format a java.util.Date as an Org inactive timestamp like
  [2026-03-12 Thu 20:05]."
  [^java.util.Date d]
  (when d
    (let [out-fmt (doto (java.text.SimpleDateFormat. "yyyy-MM-dd EEE HH:mm"
                                                     java.util.Locale/ENGLISH)
                    (.setTimeZone (java.util.TimeZone/getTimeZone "UTC")))]
      (str "[" (.format out-fmt d) "]"))))

(defn- strip-angle-brackets [s]
  (when s (str/replace s #"^<|>$" "")))

(defn- org-safe
  "Strip characters that would break Org structure: newlines (which
  would split a headline or property) and the :END: token (which
  would close a PROPERTIES drawer prematurely)."
  [s]
  (when s
    (-> (str s)
        (str/replace #"[\r\n]+" " ")
        (str/replace #":END:" ":END :"))))

(def ^:private org-property-rows
  "Ordered :PROPERTIES: entries used by `report->org-entry`.  Each row:
  `[<key in report-map> <:LABEL:> <xf> <default>]`.
  An entry is emitted when `(get m key default)` is non-nil; `xf`, when
  non-nil, transforms the value before stringification.  Vector order
  dictates property-drawer order."
  [[:from         "FROM"         org-safe                                  ""]
   [:date-raw     "DATE"         format-org-inactive-ts                    nil]
   [:message-id   "MESSAGE-ID"   #(some-> % strip-angle-brackets org-safe) nil]
   [:archived-at  "ARCHIVED-AT"  org-safe                                  nil]
   [:flags        "FLAGS"        nil                                       "---"]
   [:status       "STATUS"       nil                                       0]
   [:replies      "REPLIES"      nil                                       0]
   [:version      "VERSION"      org-safe                                  nil]
   [:topic        "TOPIC"        org-safe                                  nil]
   [:votes        "VOTES"        nil                                       nil]
   [:votes-up     "VOTES-UP"     nil                                       nil]
   [:votes-down   "VOTES-DOWN"   nil                                       nil]
   [:votes-null   "VOTES-NULL"   nil                                       nil]
   [:acked        "ACKED"        org-safe                                  nil]
   [:owned        "OWNED"        org-safe                                  nil]
   [:closed       "CLOSED"       org-safe                                  nil]
   [:close-reason "CLOSE-REASON" nil                                       nil]
   [:urgent       "URGENT"       org-safe                                  nil]
   [:important    "IMPORTANT"    org-safe                                  nil]
   [:deadline     "DEADLINE"     nil                                       nil]
   [:expiry       "EXPIRY"       nil                                       nil]])

(defn- org-property-line
  "Render one :PROPERTIES: entry from a row of `org-property-rows`,
  or `nil` to skip it."
  [m [k label xf default]]
  (let [raw (get m k default)]
    (when (some? raw)
      (str ":" label ": " (if xf (xf raw) raw)))))

(defn- report->org-entry [m]
  (let [todo    (if (= (nth (:flags m "---") 2 \-) \C) "DONE" "TODO")
        prio    (case (:priority m 0)
                  3 "[#A] " 2 "[#B] " 1 "[#C] " "")
        subject (org-safe (:subject m ""))
        tags    (when-let [t (:type m)] (str ":" t ":"))
        props   (cond-> (keep #(org-property-line m %) org-property-rows)
                  (:series m)
                  (concat [(let [s (:series m)]
                             (str ":SERIES: " (:received s) "/" (:expected s)
                                  (when (:closed s) " closed")))]))]
    (str "* " todo " " prio subject (when tags (str "  " tags)) "\n"
         (when-let [d (:deadline m)]
           (str "DEADLINE: <" d ">\n"))
         ":PROPERTIES:\n"
         (str/join "\n" props) "\n"
         ":END:\n"
         (let [;; Union of all qualified-relation kinds, deduped by mid.
               all-related (->> [:resolves :resolved-by
                                 :supersedes :superseded-by
                                 :duplicates :duplicated-by
                                 :related-to]
                                (mapcat #(get m %))
                                (filter :message-id)
                                (group-by :message-id)
                                vals
                                (map first))]
           (when (seq all-related)
             (str "\nRelated:\n"
                  (str/join "\n"
                            (map (fn [r]
                                   (str "- [" (:type r) "] " (:message-id r)
                                        (when-let [a (:archived-at r)]
                                          (str " (" a ")"))))
                                 all-related))
                  "\n"))))))

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

(defn- patch-basename
  "Return the basename of a :report/patches entry (handles absolute paths)."
  [p]
  (.getName (io/file (:patch/filename p))))

(defn dump-patches!
  "Export patch files for a single source."
  [reports patches-dir]
  (let [total (reduce (fn [n report]
                        (let [h   (mid-hash (:report/message-id report))
                              dir (io/file patches-dir h)]
                          (.mkdirs dir)
                          (doseq [p (:report/patches report)]
                            (spit (io/file dir (patch-basename p)) (:patch/text p)))
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
                                (spit (io/file dir (attachment-basename att))
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
                                (spit (io/file dir (attachment-basename att))
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

(defn- resolve-source-topics-filter
  "Return the topics filter for a source: CLI override > per-source > global.
  Returns a set of lower-cased topic strings, or nil (meaning no filter)."
  [source-name source-map cli-topics-filter]
  (or cli-topics-filter
      (resolve-topics-filter (get-in source-map [source-name :topics-filter]))))

(defn- dump-typed-formats!
  "Write `reports` to disk in every format enabled by `fmts`: JSON (if
  `fmts \"json\"` or `:json-always?`), RSS (if `fmts \"rss\"`), Org (if
  `fmts \"org\"`).  `basename` is the file stem (no extension); `label`
  is the human title used in RSS/Org headers."
  [reports reports-dir source-name source-map maintainers-map fmts basename label
   & {:keys [json-always? counts]}]
  (when (or json-always? (fmts "json"))
    (dump-json! reports reports-dir source-name source-map maintainers-map
                (str basename ".json") counts))
  (when (fmts "rss")
    (dump-rss! reports reports-dir source-name source-map maintainers-map
               (str basename ".xml") label))
  (when (fmts "org")
    (dump-org! reports reports-dir source-name source-map maintainers-map
               (str basename ".org") label)))

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
    (dump-typed-formats! typed reports-dir source-name source-map maintainers-map
                         fmts plural plural)))

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
    (dump-typed-formats! open reports-dir source-name source-map maintainers-map
                         fmts "all-open" "open reports"
                         :json-always? true :counts counts)
    (dump-typed-formats! closed reports-dir source-name source-map maintainers-map
                         fmts "all-closed" "closed reports"
                         :json-always? true :counts counts)
    (doseq [rtype report-types
            :when (or (nil? changed-types) (changed-types rtype))
            :let [plural (type->plural rtype)
                  t-open   (filter-reports open {:type rtype})
                  t-closed (filter-reports closed {:type rtype})]]
      (when (seq t-open)
        (dump-typed-formats! t-open reports-dir source-name source-map maintainers-map
                             fmts (str plural "-open") (str plural " (open)")))
      (when (seq t-closed)
        (dump-typed-formats! t-closed reports-dir source-name source-map maintainers-map
                             fmts (str plural "-closed") (str plural " (closed)"))))))

;; ---------------------------------------------------------------------------
;; Root index -- public/index.html listing all sources
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
         (html-head {:title "BARK -- Sources"
                     :css (str "table{margin-top:1.5rem}"
                               "td.num,th.num{text-align:right}"
                               "a.archive{font-size:0.82rem;margin-left:0.4rem;opacity:0.7}"
                               ".feeds{font-size:0.82rem;white-space:nowrap}"
                               ".theme-toggle{cursor:pointer;background:none;border:none;font-size:1.2rem;padding:0.3rem}"
                               footer-css)})
         "<body>\n<main class=\"container\">\n"
         (h/html (nav-bar "BARK" nil))
         "\n<table role=\"grid\">\n"
         "<thead><tr>"
         "<th>Source</th>"
         "<th class=\"num\">Open</th>"
         "<th class=\"num\">Closed</th>"
         "<th class=\"num\">Total</th>"
         "<th class=\"num\">Feeds</th>"
         "</tr></thead>\n<tbody>\n"
         (apply str (map row-html rows))
         "</tbody></table>\n"
         (h/html (bark-footer {:feeds false}))
         "\n<script>\n" (wrap-js theme-toggle-js) "\n</script>\n"
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
      config  (load-config)
      dbp     (db-path config)
      conn    (d/get-conn dbp bark-schema {:wal? false})]
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
        ;; Resolve source list *before* the expensive DB pull
        ;; so we can determine which sources actually need re-export.
        (let [effective-theme (or theme (:theme config))
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
                        votes           (votes-by-report
                                          (d/q '[:find ?r ?val ?voter ?emid
                                                 :where
                                                 [?v :vote/report ?r]
                                                 [?v :vote/value  ?val]
                                                 [?v :vote/voter  ?voter]
                                                 [?v :vote/email  ?e]
                                                 [?e :email/message-id ?emid]]
                                               db))
                        effective-ps    page-size
                        cli-tf          (resolve-topics-filter topics-filter)
                        drop-cutoff     (parse-cutoff-date closed-retention)
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
                    (set-export-context! db votes config)
                    (reduce (fn [exported? src-name]
                              (let [reports  (filter-reports all-reps {:source       src-name
                                                                       :min-priority min-priority
                                                                       :min-status   min-status})
                                    rt       (get-in source-map [src-name :report-types])
                                    reports  (if rt (filter #(contains? rt (:report/type %)) reports) reports)
                                    reports  (if drop-cutoff (drop-old-closed reports drop-cutoff) reports)
                                    src-tf   (resolve-source-topics-filter src-name source-map cli-tf)
                                    _        (when src-tf
                                               (log/info (str "[" src-name "]") "topics filter:" (str/join ", " src-tf)))
                                    reports  (filter-by-topics reports src-tf)
                                    slug     (slugify src-name)
                                    staging  (str "public/.staging-" slug)
                                    final-dir (str "public/" slug)
                                    src-changed (when (seq changed-st) (get changed-st src-name))]
                                (if (empty? reports)
                                  (do (log/info "No reports for source" (str "'" src-name "'") ", skipping.")
                                      exported?)
                                  (do (log/info (str "[" src-name "] " (count reports) " report(s)"
                                                     " (" (count (open-reports reports)) " open)"
                                                     (if incremental? " (incremental)" "")))
                                      (try
                                        (delete-dir! (io/file staging))
                                        ;; Incremental writes only changed types
                                        ;; into staging; seed it with the previous
                                        ;; target so unchanged files survive the
                                        ;; atomic swap.
                                        (when src-changed
                                          (copy-dir! (io/file final-dir)
                                                     (io/file staging)))
                                        (export-source! format reports staging src-name
                                                        source-map maintainers-map cli-extra
                                                        :changed-types src-changed)
                                        (atomic-swap-dir! staging final-dir)
                                        (catch Exception e
                                          (log/error e "Export failed for" src-name "-- cleaning up staging dir")
                                          (delete-dir! (io/file staging))
                                          (throw e)))
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
