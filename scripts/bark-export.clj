#!/usr/bin/env bb

;; bark-export.clj — Export BARK reports per source.
;;
;; Each source gets its own directory tree under public/:
;;   public/<source-name>/index.html
;;   public/<source-name>/stats.html
;;   public/<source-name>/howto.html
;;   public/<source-name>/reports/all.json
;;   public/<source-name>/reports/all.rss
;;   public/<source-name>/reports/all.org
;;   public/<source-name>/reports/bugs.json  (etc.)
;;   public/<source-name>/reports/stats.json
;;   public/<source-name>/patches/<mid-hash>/<file>
;;
;; Usage:
;;   bb export               — export all sources, all formats
;;   bb export json          — export all.json for each source
;;   bb export rss           — export all.rss for each source
;;   bb export org           — export all.org for each source
;;   bb export html          — generate index.html for each source
;;   bb export stats         — generate stats.json for each source
;;   bb export patches       — export patch files for each source
;;   bb export all           — all of the above
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
         format-date format-date-iso report-priority report-status
         report-descendant-count all-reports report-pull-pattern
         parse-cli-args load-config build-source-map bark-schema)

(load-file "scripts/bark-common.clj")

(load-datalevin-pod!)

;; ---------------------------------------------------------------------------
;; DB queries (all-reports and report-pull-pattern loaded from bark-common.clj)
;; ---------------------------------------------------------------------------

(defn all-reports-by-date [db]
  (sort-by #(get-in % [:report/email :email/date-sent]) #(compare %2 %1)
           (all-reports db)))

;; ---------------------------------------------------------------------------
;; Formatting helpers
;; ---------------------------------------------------------------------------

(def ^:private flag-defs
  [[:report/acked "A"] [:report/owned "O"] [:report/closed "C"]])

(defn- flags-str [report]
  (apply str (map (fn [[k c]] (if (get report k) c \-)) flag-defs)))

;; format-date and format-date-iso are defined in bark-common.clj

(defn- votes-str [report]
  (let [up   (or (:report/votes-up report) 0)
        down (or (:report/votes-down report) 0)
        null (or (:report/votes-null report) 0)
        total (+ up down null)]
    (when (pos? total)
      (str (- up down) "/" total))))

;; ---------------------------------------------------------------------------
;; Config & source map — loaded from bark-common.clj
;; ---------------------------------------------------------------------------

(defn- build-maintainers
  "Gather per-source maintainer sets from DB roles.
   Returns source-name -> #{maintainer-emails}."
  [db source-map]
  (into {}
        (map (fn [[source-name _]]
               (let [roles (d/pull db '[:roles/maintainers]
                                   [:roles/source source-name])
                     v     (:roles/maintainers roles)
                     maints (cond (nil? v) #{}
                                  (string? v) #{(str/lower-case v)}
                                  :else (set (map str/lower-case v)))]
                 [source-name maints])))
        source-map))

;; ---------------------------------------------------------------------------
;; Report -> map
;; ---------------------------------------------------------------------------

;; get-header loaded from bark-common.clj

(defn- archived-at [email]
  (get-header (:email/headers-edn email) "Archived-At"))

(defn- sender-role
  "Determine role of sender for a given source context."
  [from source-name source-map maintainers-map]
  (when (and (seq from) source-name)
    (let [from-lc  (str/lower-case from)
          src-info (get source-map source-name)
          admin    (some-> (:admin src-info) str/lower-case)]
      (cond
        (= from-lc admin)                                            "admin"
        (contains? (get maintainers-map source-name #{}) from-lc)    "maintainer"
        :else                                                        nil))))

(def ^:private from-address-fields
  "Report attrs whose :email/from-address should be extracted into the output map."
  [[:report/acked :acked] [:report/owned :owned] [:report/closed :closed]
   [:report/urgent :urgent] [:report/important :important]
   [:report/acked-proxy :acked-proxy] [:report/owned-proxy :owned-proxy]
   [:report/closed-proxy :closed-proxy] [:report/urgent-proxy :urgent-proxy]
   [:report/important-proxy :important-proxy]])

(defn- assoc-from-addresses
  "Extract :email/from-address from report attrs defined in from-address-fields."
  [m report]
  (reduce (fn [m [rk mk]]
            (if-let [v (get report rk)]
              (assoc m mk (:email/from-address v))
              m))
          m from-address-fields))

(defn report->map [report source-map maintainers-map]
  (let [email       (:report/email report)
        source-name (:email/source email)
        from        (or (:email/from-address email) "")
        raw-arch    (archived-at email)
        mid         (some-> (:report/message-id report)
                            (str/replace #"^<|>$" ""))
        fmt-str     (get-in source-map [source-name :archive-format-string])
        arch        (if (and fmt-str mid)
                      (str/replace fmt-str "%s" mid)
                      raw-arch)
        votes       (votes-str report)
        series      (:report/series report)
        related     (:report/related report)
        role        (sender-role from source-name source-map maintainers-map)]
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
          (:report/patch-source report)   (assoc :patch-source (mapv name (:report/patch-source report)))
          arch                            (assoc :archived-at arch)
          (:report/deadline report)       (assoc :deadline (format-date-iso (:report/deadline report)))
          votes                           (assoc :votes votes)
          (pos? (or (:report/votes-up report) 0))
          (assoc :votes-up (:report/votes-up report))
          (pos? (or (:report/votes-down report) 0))
          (assoc :votes-down (:report/votes-down report))
          (pos? (or (:report/votes-null report) 0))
          (assoc :votes-null (:report/votes-null report))
          series                          (assoc :series
                                                 (let [patches (:series/patches series)]
                                                   {:received (count patches)
                                                    :expected (:series/expected series)
                                                    :complete (= (count patches)
                                                                 (:series/expected series))
                                                    :closed   (some? (:series/closed series))}))
          (seq related)                   (assoc :related
                                                 (mapv (fn [r]
                                                         (let [arch (archived-at (:report/email r))]
                                                           (cond-> {:type       (name (:report/type r))
                                                                    :message-id (:report/message-id r)}
                                                             arch (assoc :archived-at arch))))
                                                       related))
          (seq (:report/patches report))
          (assoc :patches
                 (let [h (mid-hash (:report/message-id report))]
                   (mapv (fn [p]
                           (cond-> {:file   (str h "/" (:patch/filename p))
                                    :source (name (:patch/source p))}
                             (:patch/author p)  (assoc :author  (:patch/author p))
                             (:patch/subject p) (assoc :subject (:patch/subject p))
                             (:patch/date p)    (assoc :date    (:patch/date p))))
                         (:report/patches report))))))))

;; ---------------------------------------------------------------------------
;; Source metadata for JSON envelope
;; ---------------------------------------------------------------------------

(defn- source-metadata
  "Build metadata map for a single source."
  [source-name source-map]
  (let [cfg (get source-map source-name)]
    (cond-> {}
      (:list-id cfg)       (assoc :list-id       (:list-id cfg))
      (:list-post cfg)     (assoc :list-post     (:list-post cfg))
      (:list-archive cfg)  (assoc :list-archive  (:list-archive cfg))
      (:bark-path cfg)     (assoc :bark-path     (:bark-path cfg)))))

;; ---------------------------------------------------------------------------
;; XML helpers
;; ---------------------------------------------------------------------------

(defn- xml-escape [s]
  (when s
    (-> s
        (str/replace "&" "&amp;")
        (str/replace "<" "&lt;")
        (str/replace ">" "&gt;")
        (str/replace "\"" "&quot;"))))

;; ---------------------------------------------------------------------------
;; Filtering
;; ---------------------------------------------------------------------------

(defn- filter-by-source
  "Filter reports to only those from the given source name."
  [reports source-name]
  (filter #(= source-name (get-in % [:report/email :email/source])) reports))

(defn- filter-by-priority
  "Keep only reports with priority >= min-p."
  [reports min-p]
  (filter #(>= (report-priority %) min-p) reports))

(defn- filter-by-status
  "Keep only reports with status >= min-s."
  [reports min-s]
  (filter #(>= (report-status %) min-s) reports))

(defn- filter-by-type
  "Keep only reports of the given type keyword."
  [reports rtype]
  (filter #(= rtype (:report/type %)) reports))

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
   (dump-json! reports out-dir source-name source-map maintainers-map "all.json"))
  ([reports out-dir source-name source-map maintainers-map basename]
   (let [data     (mapv #(report->map % source-map maintainers-map) reports)
         meta     (source-metadata source-name source-map)
         envelope (cond-> {:format-version "0.2.1"
                           :source         source-name
                           :reports        data}
                    (seq meta) (merge meta))
         filename (str out-dir "/" basename)]
     (spit filename (json/generate-string envelope {:pretty true}))
     (log/info "Wrote" (count data) "reports to" filename))))

(defn- rfc822-date
  "RFC 822 date from a java.util.Date#toString string."
  [date-str]
  (try
    (let [in-fmt  (doto (java.text.SimpleDateFormat. "EEE MMM dd HH:mm:ss zzz yyyy"
                                                     java.util.Locale/ENGLISH)
                    (.setLenient true))
          d       (.parse in-fmt (str date-str))
          out-fmt (doto (java.text.SimpleDateFormat. "EEE, dd MMM yyyy HH:mm:ss Z"
                                                     java.util.Locale/ENGLISH)
                    (.setTimeZone (java.util.TimeZone/getTimeZone "UTC")))]
      (.format out-fmt d))
    (catch Exception _ nil)))

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
                      (when-let [d (:deadline m)] (str " deadline:" d))))]
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

(defn dump-rss!
  "Dump reports as RSS 2.0 for a single source.
  Only includes open reports, capped at rss-limit (50)."
  ([reports out-dir source-name source-map maintainers-map]
   (dump-rss! reports out-dir source-name source-map maintainers-map "all.rss" "reports"))
  ([reports out-dir source-name source-map maintainers-map basename feed-label]
   (let [capped   (->> reports open-reports (take rss-limit))
         data     (mapv #(report->map % source-map maintainers-map) capped)
         items    (str/join "\n" (map report->rss-item data))
         list-url (get-in source-map [source-name :list-archive] "")
         filename (str out-dir "/" basename)]
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
     (log/info "Wrote" (count data) "reports to" filename))))

(defn- report->org-entry [m]
  (let [todo    (if (= (nth (:flags m "---") 2 \-) \C) "DONE" "TODO")
        prio    (case (:priority m 0)
                  3 "[#A] " 2 "[#A] " 1 "[#B] " "")
        subject (:subject m "")
        tags    (when-let [t (:type m)] (str ":" t ":"))
        props   (remove nil?
                        [(str ":FROM: " (:from m ""))
                         (str ":DATE: " (:date m ""))
                         (when-let [mid (:message-id m)] (str ":MESSAGE-ID: " mid))
                         (when-let [a (:archived-at m)]  (str ":ARCHIVED-AT: " a))
                         (str ":FLAGS: " (:flags m "---"))
                         (str ":STATUS: " (:status m 0))
                         (str ":PRIORITY: " (:priority m 0))
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
                         (when-let [u (:urgent m)]     (str ":URGENT: " u))
                         (when-let [i (:important m)]  (str ":IMPORTANT: " i))
                         (when-let [d (:deadline m)]     (str ":DEADLINE: " d))
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
   (let [data     (mapv #(report->map % source-map maintainers-map) reports)
         entries  (str/join "\n" (map report->org-entry data))
         filename (str out-dir "/" basename)]
     (spit filename
           (str "#+TITLE: BARK " source-name " " title-label "\n"
                "#+DATE: " (java.time.LocalDate/now) "\n\n"
                entries))
     (log/info "Wrote" (count data) "reports to" filename))))

(defn dump-patches!
  "Export patch files for a single source."
  [reports patches-dir]
  (let [total (reduce (fn [n report]
                        (let [h   (mid-hash (:report/message-id report))
                              dir (io/file patches-dir h)]
                          (.mkdirs dir)
                          (doseq [p (:report/patches report)]
                            (spit (io/file dir (:patch/filename p)) (:patch/text p)))
                          (+ n (count (:report/patches report)))))
                      0
                      (filter #(seq (:report/patches %)) reports))]
    (when (pos? total)
      (log/info "Wrote" total "patch file(s)"))))

(defn dump-html!
  "Generate index.html for a single source."
  [base-dir reports-dir cli-args]
  (let [json-file (str reports-dir "/all.json")]
    (apply process/shell "bb" "scripts/bark-index.clj"
           "-o" (str base-dir "/index.html")
           "--json" json-file
           "--dir" reports-dir
           cli-args)))

(defn dump-stats!
  "Generate stats for a single source."
  [base-dir reports-dir source-name format cli-args]
  (let [dir      (if (= format "html") base-dir reports-dir)
        out-file (str dir (if (= format "html") "/stats.html" "/stats.json"))]
    (apply process/shell "bb" "scripts/bark-stats.clj"
           (if (= format "html") "html" "json")
           "-o" out-file
           "-n" source-name
           cli-args)))

(defn dump-howto!
  "Generate howto.html for a single source."
  [base-dir reports-dir source-name]
  (apply process/shell "bb" "scripts/bark-howto.clj"
         "-o" (str base-dir "/howto.html")
         "--dir" base-dir
         (when source-name ["-n" source-name])))

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

(defn- dump-per-type!
  "Export per-type JSON, Org, and RSS files for all report types present."
  [reports reports-dir source-name source-map maintainers-map fmts]
  (doseq [rtype report-types
          :let [typed (filter-by-type reports rtype)
                plural (type->plural rtype)]
          :when (seq typed)]
    (when (fmts "json")
      (dump-json! typed reports-dir source-name source-map maintainers-map
                  (str plural ".json")))
    (when (fmts "org")
      (dump-org! typed reports-dir source-name source-map maintainers-map
                 (str plural ".org") plural))
    (when (fmts "rss")
      (dump-rss! typed reports-dir source-name source-map maintainers-map
                 (str plural ".rss") plural))))

;; ---------------------------------------------------------------------------
;; Per-source export orchestration
;; ---------------------------------------------------------------------------

(defn export-source!
  "Export a single source in the given format(s).
  When format is \"all\", per-type feeds respect :export-formats from config."
  [format reports base-dir source-name source-map maintainers-map cli-extra]
  (let [reports-dir (str base-dir "/reports")
        patches-dir (str base-dir "/patches")
        _           (doseq [d [reports-dir patches-dir]]
                      (.mkdirs (io/file d)))
        ef          (resolve-export-formats source-name source-map)
        do-format   (fn [fmt]
                      (case fmt
                        "json"    (do (dump-json! reports reports-dir source-name source-map maintainers-map)
                                      (dump-per-type! reports reports-dir source-name source-map maintainers-map #{"json"}))
                        "rss"     (do (dump-rss!  reports reports-dir source-name source-map maintainers-map)
                                      (dump-per-type! reports reports-dir source-name source-map maintainers-map #{"rss"}))
                        "org"     (do (dump-org!  reports reports-dir source-name source-map maintainers-map)
                                      (dump-per-type! reports reports-dir source-name source-map maintainers-map #{"org"}))
                        "patches" (dump-patches! reports patches-dir)
                        "html"    (do (dump-json! reports reports-dir source-name source-map maintainers-map)
                                      (dump-per-type! reports reports-dir source-name source-map maintainers-map #{"json"})
                                      (dump-howto! base-dir reports-dir source-name)
                                      (dump-html! base-dir reports-dir cli-extra))
                        "stats"   (dump-stats! base-dir reports-dir source-name "json" cli-extra)))]
    (if (= format "all")
      (do (when (ef "json") (dump-json! reports reports-dir source-name source-map maintainers-map))
          (when (ef "rss")  (dump-rss!  reports reports-dir source-name source-map maintainers-map))
          (when (ef "org")  (dump-org!  reports reports-dir source-name source-map maintainers-map))
          (dump-per-type! reports reports-dir source-name source-map maintainers-map ef)
          (dump-patches! reports patches-dir)
          (dump-howto! base-dir reports-dir source-name)
          (dump-html! base-dir reports-dir cli-extra)
          (dump-stats! base-dir reports-dir source-name "json" cli-extra)
          (dump-stats! base-dir reports-dir source-name "html" cli-extra))
      (do-format format))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(def formats #{"json" "rss" "org" "html" "all" "stats" "patches"})

(let [{:keys [format source-name min-priority min-status]
       :or {format "all"}}
      (parse-cli-args *command-line-args*)
      db-path (or (System/getenv "BARK_DB") "data/bark-db")
      conn    (d/get-conn db-path bark-schema {:wal? false})]
  (try
    (when-not (formats format)
      (log/error "Unknown format:" format)
      (log/error "Formats: json rss org html stats patches all")
      (System/exit 1))
    (when (and min-priority (not (#{1 2 3} min-priority)))
      (log/error "Invalid --min-priority:" min-priority "(must be 1, 2, or 3)")
      (System/exit 1))
    (when (and min-status (not (<= 1 min-status 7)))
      (log/error "Invalid --min-status:" min-status "(must be 1–7)")
      (System/exit 1))
    (let [db              (d/db conn)
          config          (load-config)
          source-map      (if config (build-source-map config) {})
          maintainers-map (if config (build-maintainers db source-map) {})
          all-reps        (all-reports-by-date db)
          source-names    (if source-name
                            (if (contains? source-map source-name)
                              [source-name]
                              (do (log/error "No source named" (str "'" source-name "'"))
                                  (log/error "Available:"
                                                (str/join ", " (keys source-map)))
                                  (System/exit 1)))
                            (mapv :name (:sources config)))
          cli-extra       (remove #{format "-n" source-name} (rest *command-line-args*))]
      (doseq [src-name source-names]
        (let [reports (filter-by-source all-reps src-name)
              reports (if min-priority (filter-by-priority reports min-priority) reports)
              reports (if min-status   (filter-by-status reports min-status) reports)
              er      (resolve-export-reports src-name source-map)
              reports (if er (filter #(contains? er (:report/type %)) reports) reports)
              base-dir (str "public/" (slugify src-name))]
          (log/info (str "[" src-name "]") (count reports) "report(s)")
          (if (empty? reports)
            (log/info "No reports for source" (str "'" src-name "'") ", skipping.")
            (export-source! format reports base-dir src-name
                            source-map maintainers-map cli-extra)))))
    (finally
      (d/close conn))))
