#!/usr/bin/env bb

;; bark-export.clj — Export BARK reports per source.
;;
;; Each source gets its own directory under public/:
;;   public/<source-name>/reports.json
;;   public/<source-name>/reports.rss
;;   public/<source-name>/reports.org
;;   public/<source-name>/index.html
;;   public/<source-name>/stats.html
;;   public/<source-name>/howto.html
;;   public/<source-name>/patches/<mid-hash>/<file>
;;
;; Usage:
;;   bb export               — export all sources, all formats
;;   bb export json          — export reports.json for each source
;;   bb export rss           — export reports.rss for each source
;;   bb export org           — export reports.org for each source
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

(load-file "scripts/bark-common.clj")

(load-datalevin-pod!)

;; ---------------------------------------------------------------------------
;; Schema — loaded from resources/bark-schema.edn
;; ---------------------------------------------------------------------------

(def schema
  (edn/read-string (slurp "resources/bark-schema.edn")))

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

(def priority        report-priority)
(def status          report-status)
(def descendant-count report-descendant-count)

(defn- format-date [date]
  (let [s (str (or date ""))]
    (subs s 0 (min 16 (count s)))))

(defn- votes-str [report]
  (let [up   (or (:report/votes-up report) 0)
        down (or (:report/votes-down report) 0)
        total (+ up down)]
    (when (pos? total)
      (str up "/" total))))

;; ---------------------------------------------------------------------------
;; Config & source map — loaded from bark-common.clj
;; ---------------------------------------------------------------------------

(defn- build-maintainers
  "Gather per-source maintainer sets from DB roles.
   Returns source-name → #{maintainer-emails}."
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
;; Report → map
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
    (cond-> {:type     (name (:report/type report))
             :subject  (or (:email/subject email) "")
             :from     from
             :date     (format-date (:email/date-sent email))
             :date-raw (str (:email/date-sent email))
             :flags    (flags-str report)
             :status   (status report)
             :priority (priority report)
             :replies  (descendant-count report)}
      (:email/from-name email)        (assoc :from-name (:email/from-name email))
      role                            (assoc :role role)
      (:report/acked report)          (assoc :acked (:email/from-address (:report/acked report)))
      (:report/owned report)          (assoc :owned (:email/from-address (:report/owned report)))
      (:report/closed report)         (assoc :closed (:email/from-address (:report/closed report)))
      (:report/message-id report)     (assoc :message-id (:report/message-id report))
      (:report/version report)        (assoc :version (:report/version report))
      (:report/topic report)          (assoc :topic (:report/topic report))
      (:report/patch-seq report)      (assoc :patch-seq (:report/patch-seq report))
      (:report/patch-source report)   (assoc :patch-source (mapv name (:report/patch-source report)))
      arch                            (assoc :archived-at arch)
      votes                           (assoc :votes votes)
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
                       (cond-> {:file   (str "patches/" h "/" (:patch/filename p))
                                :source (name (:patch/source p))}
                         (:patch/author p)  (assoc :author  (:patch/author p))
                         (:patch/subject p) (assoc :subject (:patch/subject p))
                         (:patch/date p)    (assoc :date    (:patch/date p))))
                     (:report/patches report)))))))

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
;; Per-source export functions
;; ---------------------------------------------------------------------------

(defn dump-json!
  "Dump reports as JSON for a single source."
  [reports out-dir source-name source-map maintainers-map]
  (let [data     (mapv #(report->map % source-map maintainers-map) reports)
        meta     (source-metadata source-name source-map)
        envelope (cond-> {:source source-name :reports data}
                   (seq meta) (merge meta))
        filename (str out-dir "/reports.json")]
    (spit filename (json/generate-string envelope {:pretty true}))
    (println (str "  Wrote " (count data) " reports to " filename))))

(defn- rfc822-date
  "RFC 822 date from an ISO-ish date string or inst."
  [date-str]
  (try
    (let [inst (java.time.Instant/parse date-str)]
      (.format (doto (java.text.SimpleDateFormat. "EEE, dd MMM yyyy HH:mm:ss Z")
                 (.setTimeZone (java.util.TimeZone/getTimeZone "UTC")))
               (java.util.Date/from inst)))
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
                      (when-let [v (:version m)] (str " version:" v))
                      (when-let [t (:topic m)]   (str " topic:" t))))]
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
  "Dump reports as RSS 2.0 for a single source."
  [reports out-dir source-name source-map maintainers-map]
  (let [data     (mapv #(report->map % source-map maintainers-map) reports)
        items    (str/join "\n" (map report->rss-item data))
        list-url (get-in source-map [source-name :list-archive] "")
        filename (str out-dir "/reports.rss")]
    (spit filename
          (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
               "<rss version=\"2.0\">\n"
               "  <channel>\n"
               "    <title>BARK " source-name " reports</title>\n"
               "    <link>" list-url "</link>\n"
               "    <description>Reports from the Bug And Report Keeper</description>\n"
               items "\n"
               "  </channel>\n"
               "</rss>\n"))
    (println (str "  Wrote " (count data) " reports to " filename))))

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
                         (when-let [a (seq (:acked m))]  (str ":ACKED-BY: " a))
                         (when-let [o (seq (:owned m))]  (str ":OWNED-BY: " o))
                         (when-let [c (seq (:closed m))] (str ":CLOSED-BY: " c))
                         (when-let [s (:series m)]
                           (str ":SERIES: " (:received s) "/" (:expected s)
                                (when (:closed s) " closed")))])]
    (str "* " todo " " prio subject (when tags (str "  " tags)) "\n"
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
  [reports out-dir source-name source-map maintainers-map]
  (let [data     (mapv #(report->map % source-map maintainers-map) reports)
        entries  (str/join "\n" (map report->org-entry data))
        filename (str out-dir "/reports.org")]
    (spit filename
          (str "#+TITLE: BARK " source-name " reports\n"
               "#+DATE: " (str (java.time.LocalDate/now)) "\n\n"
               entries))
    (println (str "  Wrote " (count data) " reports to " filename))))

(defn dump-patches!
  "Export patch files for a single source."
  [reports out-dir]
  (let [patch-reports (filter #(seq (:report/patches %)) reports)
        total         (atom 0)]
    (doseq [report patch-reports]
      (let [mid  (:report/message-id report)
            h    (mid-hash mid)
            dir  (io/file out-dir "patches" h)]
        (.mkdirs dir)
        (doseq [p (:report/patches report)]
          (let [f (io/file dir (:patch/filename p))]
            (spit f (:patch/text p))
            (swap! total inc)))))
    (when (pos? @total)
      (println (str "  Wrote " @total " patch file(s) from "
                    (count patch-reports) " report(s)")))))

(defn dump-html!
  "Generate index.html for a single source."
  [out-dir source-name cli-args]
  (let [json-file (str out-dir "/reports.json")]
    (apply process/shell "bb" "scripts/bark-index.clj"
           "-o" (str out-dir "/index.html")
           "--json" json-file
           cli-args)))

(defn dump-stats!
  "Generate stats for a single source."
  [out-dir source-name format cli-args]
  (let [out-file (str out-dir (if (= format "html") "/stats.html" "/stats.json"))]
    (apply process/shell "bb" "scripts/bark-stats.clj"
           (if (= format "html") "html" "json")
           "-o" out-file
           "-n" source-name
           cli-args)))

(defn dump-howto!
  "Generate howto.html for a single source."
  [out-dir]
  (process/shell "bb" "scripts/bark-howto.clj"
                 "-o" (str out-dir "/howto.html")))

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
  (filter #(>= (priority %) min-p) reports))

(defn- filter-by-status
  "Keep only reports with status >= min-s."
  [reports min-s]
  (filter #(>= (status %) min-s) reports))

;; ---------------------------------------------------------------------------
;; Per-source export orchestration
;; ---------------------------------------------------------------------------

(defn export-source!
  "Export a single source in the given format(s)."
  [format reports out-dir source-name source-map maintainers-map cli-extra]
  (.mkdirs (io/file out-dir))
  (let [do-format (fn [fmt]
                    (case fmt
                      "json"    (dump-json!    reports out-dir source-name source-map maintainers-map)
                      "rss"     (dump-rss!     reports out-dir source-name source-map maintainers-map)
                      "org"     (dump-org!     reports out-dir source-name source-map maintainers-map)
                      "patches" (dump-patches! reports out-dir)
                      "html"    (do (dump-json! reports out-dir source-name source-map maintainers-map)
                                    (dump-howto! out-dir)
                                    (dump-html!  out-dir source-name cli-extra))
                      "stats"   (dump-stats! out-dir source-name "json" cli-extra)))]
    (if (= format "all")
      (do (do-format "json")
          (do-format "rss")
          (do-format "org")
          (do-format "patches")
          (dump-howto! out-dir)
          (dump-html!  out-dir source-name cli-extra)
          (dump-stats! out-dir source-name "json" cli-extra)
          (dump-stats! out-dir source-name "html" cli-extra))
      (do-format format))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(def formats #{"json" "rss" "org" "html" "all" "stats" "patches"})

(let [{:keys [format source-name min-priority min-status]
       :or {format "all"}}
      (parse-cli-args *command-line-args*)
      db-path (or (System/getenv "BARK_DB") "data/bark-db")
      conn    (d/get-conn db-path schema {:wal? false})]
  (try
    (when-not (formats format)
      (println (str "Unknown format: " format))
      (println "Formats: json rss org html stats patches all")
      (System/exit 1))
    (when (and min-priority (not (#{1 2 3} min-priority)))
      (println (str "Invalid --min-priority: " min-priority " (must be 1, 2, or 3)"))
      (System/exit 1))
    (when (and min-status (not (<= 1 min-status 7)))
      (println (str "Invalid --min-status: " min-status " (must be 1–7)"))
      (System/exit 1))
    (let [db              (d/db conn)
          config          (load-config)
          source-map      (if config (build-source-map config) {})
          maintainers-map (if config (build-maintainers db source-map) {})
          all-reps        (all-reports-by-date db)
          source-names    (if source-name
                            (if (contains? source-map source-name)
                              [source-name]
                              (do (println (str "Error: no source named '" source-name "'"))
                                  (println (str "Available: "
                                                (str/join ", " (keys source-map))))
                                  (System/exit 1)))
                            (mapv :name (:sources config)))
          cli-extra       (remove #{format "-n" source-name} (rest *command-line-args*))]
      (doseq [src-name source-names]
        (let [reports (filter-by-source all-reps src-name)
              reports (if min-priority (filter-by-priority reports min-priority) reports)
              reports (if min-status   (filter-by-status reports min-status) reports)
              out-dir (str "public/" src-name)]
          (println (str "[" src-name "] " (count reports) " report(s)"))
          (if (empty? reports)
            (println (str "  No reports for source '" src-name "', skipping."))
            (export-source! format reports out-dir src-name
                            source-map maintainers-map cli-extra)))))
    (finally
      (d/close conn))))
