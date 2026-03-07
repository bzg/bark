#!/usr/bin/env bb

;; bark-export.clj — Dump BARK reports as JSON, RSS, Org, or HTML.
;;
;; Usage:
;;   bb export               — dump all reports as reports.json
;;   bb export json          — dump all reports as reports.json
;;   bb export rss           — dump all reports as reports.rss
;;   bb export org           — dump all reports as reports.org
;;   bb export html          — generate public/index.html and public/stats.html
;;   bb export stats         — generate public/stats.json
;;   bb export all           — export to public/: reports.json, reports.rss,
;;                             reports.org, index.html, stats.html
;;   bb export json -p 2     — only priority >= 2
;;   bb export json -s 3     — only status >= 3 (i.e. acked+owned+closed or above)
;;
;; Environment / defaults:
;;   BARK_DB — path to db (default: ./data/bark-db)

(require '[babashka.pods :as pods]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io])

(load-file "scripts/bark-common.clj")

(pods/load-pod 'huahaiy/datalevin "0.10.7")

(require '[pod.huahaiy.datalevin :as d])

;; ---------------------------------------------------------------------------
;; Schema — loaded from resources/bark-schema.edn
;; ---------------------------------------------------------------------------

(def schema
  (edn/read-string (slurp "resources/bark-schema.edn")))

;; ---------------------------------------------------------------------------
;; DB queries
;; ---------------------------------------------------------------------------

(def report-pull-pattern
  '[:db/id :report/type :report/version :report/topic
    :report/patch-seq :report/patch-source :report/message-id
    {:report/acked [:email/from-address]}
    {:report/owned [:email/from-address]}
    {:report/closed [:email/from-address]}
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

(defn all-reports [db]
  (->> (d/q (list :find (list 'pull '?r report-pull-pattern) :where ['?r :report/type '_]) db)
       (map first)
       (sort-by #(get-in % [:report/email :email/date-sent]) #(compare %2 %1))))

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

(defn report->map [report source-map maintainers-map multi-src?]
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
      (:email/from-name email)          (assoc :from-name (:email/from-name email))
      role                              (assoc :role role)
      (and multi-src? source-name)      (assoc :source source-name)
      (:report/acked report)          (assoc :acked (:email/from-address (:report/acked report)))
      (:report/owned report)          (assoc :owned (:email/from-address (:report/owned report)))
      (:report/closed report)         (assoc :closed (:email/from-address (:report/closed report)))
      (:report/message-id report)   (assoc :message-id (:report/message-id report))
      (:report/version report)      (assoc :version (:report/version report))
      (:report/topic report)        (assoc :topic (:report/topic report))
      (:report/patch-seq report)    (assoc :patch-seq (:report/patch-seq report))
      (:report/patch-source report) (assoc :patch-source (mapv name (:report/patch-source report)))
      arch                          (assoc :archived-at arch)
      votes                         (assoc :votes votes)
      series                        (assoc :series
                                           (let [patches (:series/patches series)]
                                             {:received (count patches)
                                              :expected (:series/expected series)
                                              :complete (= (count patches)
                                                           (:series/expected series))
                                              :closed   (some? (:series/closed series))}))
      (seq related)                 (assoc :related
                                           (mapv (fn [r]
                                                   (let [arch (archived-at (:report/email r))]
                                                     (cond-> {:type       (name (:report/type r))
                                                              :message-id (:report/message-id r)}
                                                       arch (assoc :archived-at arch))))
                                                 related)))))

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
;; JSON output
;; ---------------------------------------------------------------------------

(defn dump-json!
  "Dump reports as JSON to a file."
  [reports filename source-map maintainers-map multi-src?]
  (let [data (mapv #(report->map % source-map maintainers-map multi-src?) reports)]
    (spit filename (json/generate-string data {:pretty true}))
    (println (str "Wrote " (count data) " reports to " filename))))

;; ---------------------------------------------------------------------------
;; RSS output
;; ---------------------------------------------------------------------------

(defn- rfc822-date
  "RFC 822 date from an ISO-ish date string or inst."
  [date-str]
  (try
    (let [inst (java.time.Instant/parse date-str)]
      (.format (doto (java.text.SimpleDateFormat. "EEE, dd MMM yyyy HH:mm:ss Z")
                 (.setTimeZone (java.util.TimeZone/getTimeZone "UTC")))
               (java.util.Date/from inst)))
    (catch Exception _ nil)))

(defn- rss-author
  "Format author as 'email (name)' per RSS spec."
  [m]
  (let [email (:from m)
        name  (:from-name m)]
    (xml-escape (if (and name (not= name email))
                  (str email " (" name ")")
                  email))))

(defn- rss-guid
  "Use archived-at as guid if available, else message-id."
  [m]
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
  "Dump reports as RSS 2.0 to a file."
  [reports filename label source-map maintainers-map multi-src?]
  (let [data  (mapv #(report->map % source-map maintainers-map multi-src?) reports)
        items (str/join "\n" (map report->rss-item data))
        link  "https://www.softwareheritage.org"]
    (spit filename
          (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
               "<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\">\n"
               "  <channel>\n"
               "    <title>BARK " label " reports</title>\n"
               "    <link>" link "</link>\n"
               "    <description>Reports from the Bug And Report Keeper</description>\n"
               "    <atom:link href=\"\" rel=\"self\" type=\"application/rss+xml\"/>\n"
               items "\n"
               "  </channel>\n"
               "</rss>\n"))
    (println (str "Wrote " (count data) " reports to " filename))))

;; ---------------------------------------------------------------------------
;; Org output
;; ---------------------------------------------------------------------------

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
  "Dump reports as Org to a file."
  [reports filename label source-map maintainers-map multi-src?]
  (let [data    (mapv #(report->map % source-map maintainers-map multi-src?) reports)
        entries (str/join "\n" (map report->org-entry data))]
    (spit filename
          (str "#+TITLE: BARK " label " reports\n"
               "#+DATE: " (str (java.time.LocalDate/now)) "\n\n"
               entries))
    (println (str "Wrote " (count data) " reports to " filename))))

;; ---------------------------------------------------------------------------
;; CLI helpers
;; ---------------------------------------------------------------------------

(def parse-args parse-cli-args)

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
;; Main
;; ---------------------------------------------------------------------------

(def formats #{"json" "rss" "org" "html" "all" "stats"})

(let [{:keys [format source-name min-priority min-status]
       :or {format "json"}}
      (parse-args *command-line-args*)
      db-path (or (System/getenv "BARK_DB") "data/bark-db")
      conn    (d/get-conn db-path schema {:wal? false})]
  (try
    (when-not (formats format)
      (println (str "Unknown format: " format))
      (println "Formats: json rss org html stats all")
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
          multi-src?      (> (count source-map) 1)
          label           "report"
          all-reps        (all-reports db)
          reports         (if source-name
                            (let [matching (filter-by-source all-reps source-name)]
                              (if (and (empty? matching) (not (contains? source-map source-name)))
                                (do (println (str "Error: no source named '" source-name "'"))
                                    (println (str "Available: "
                                                  (str/join ", " (keys source-map))))
                                    (System/exit 1))
                                matching))
                            all-reps)
          reports         (if min-priority
                            (filter-by-priority reports min-priority)
                            reports)
          reports         (if min-status
                            (filter-by-status reports min-status)
                            reports)
          _               (.mkdirs (clojure.java.io/file "public"))
          basename        "public/reports"]
      (if (empty? reports)
        (println (str "No reports found."
                      (when source-name (str " (source: " source-name ")"))))
        (case format
          "json"  (dump-json! reports (str basename ".json") source-map maintainers-map multi-src?)
          "rss"   (dump-rss!  reports (str basename ".rss") label source-map maintainers-map multi-src?)
          "org"   (dump-org!  reports (str basename ".org") label source-map maintainers-map multi-src?)
          "html"  (do (dump-json! reports "public/reports.json" source-map maintainers-map multi-src?)
                      (babashka.process/shell "bb scripts/bark-howto.clj")
                      (apply babashka.process/shell "bb scripts/bark-index.clj" *command-line-args*))
          "stats" (apply babashka.process/shell "bb scripts/bark-stats.clj" *command-line-args*)
          "all"   (let [extra (rest *command-line-args*)]
                    (dump-json! reports "public/reports.json" source-map maintainers-map multi-src?)
                    (dump-rss!  reports "public/reports.rss"  label source-map maintainers-map multi-src?)
                    (dump-org!  reports "public/reports.org"  label source-map maintainers-map multi-src?)
                    (babashka.process/shell "bb scripts/bark-howto.clj")
                    (apply babashka.process/shell "bb scripts/bark-index.clj" extra)
                    (apply babashka.process/shell "bb scripts/bark-stats.clj" extra)
                    (apply babashka.process/shell "bb scripts/bark-stats.clj" "html" extra)))))
    (finally
      (d/close conn))))
