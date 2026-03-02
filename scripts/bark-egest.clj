#!/usr/bin/env bb

;; bark-egest.clj — Dump BARK reports as JSON, RSS, or Org.
;;
;; Usage:
;;   bb bugs-json           — dump bugs as bugs.json
;;   bb reports-rss         — dump all reports as reports.rss
;;   bb patches-org         — dump patches as patches.org
;;
;; Environment / defaults:
;;   BARK_DB — path to db (default: ./data/bark-db)

(require '[babashka.pods :as pods]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[clojure.edn :as edn])

(pods/load-pod 'huahaiy/datalevin "0.10.5")

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
    :report/acked :report/owned :report/closed
    :report/urgent :report/important
    :report/votes-up :report/votes-down
    :report/descendants :report/digested-at
    {:report/related [:report/type :report/message-id
                      {:report/email [:email/headers-edn]}]}
    {:report/series [:series/id :series/expected :series/closed
                     {:series/patches [:db/id]}
                     {:series/cover-letter [:email/message-id]}]}
    {:report/email [:email/subject :email/from-address :email/from-name
                    :email/date-sent :email/imap-uid
                    :email/headers-edn]}])

(defn all-reports-by-type [db report-type]
  (->> (d/q (list :find (list 'pull '?r report-pull-pattern)
                  :in '$ '?type :where ['?r :report/type '?type])
            db report-type)
       (map first)
       (sort-by #(get-in % [:report/email :email/date-sent]) #(compare %2 %1))))

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
  (apply str (map (fn [[k c]] (if (get report k) c "-")) flag-defs)))

(defn- priority [report]
  (+ (if (:report/urgent report) 2 0)
     (if (:report/important report) 1 0)))

(defn- descendant-count [report]
  (let [d (:report/descendants report)]
    (if (coll? d) (count d) 0)))

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
;; Report → map
;; ---------------------------------------------------------------------------

(defn- get-header
  "Case-insensitive header lookup from a parsed headers map."
  [headers header-name]
  (let [lname (str/lower-case header-name)]
    (some (fn [[k v]] (when (= (str/lower-case k) lname) v)) headers)))

(defn- archived-at [email]
  (when-let [edn-str (:email/headers-edn email)]
    (get-header (edn/read-string edn-str) "Archived-At")))

(defn report->map [report]
  (let [email   (:report/email report)
        arch    (archived-at email)
        votes   (votes-str report)
        series  (:report/series report)
        related (:report/related report)]
    (cond-> {:type     (name (:report/type report))
             :subject  (or (:email/subject email) "")
             :from     (or (:email/from-address email) "")
             :date     (format-date (:email/date-sent email))
             :date-raw (str (:email/date-sent email))
             :flags    (flags-str report)
             :priority (priority report)
             :replies  (descendant-count report)}
      (:email/from-name email)      (assoc :from-name (:email/from-name email))
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
  [reports filename]
  (let [data (mapv report->map reports)]
    (spit filename (json/generate-string data {:pretty true}))
    (println (str "Wrote " (count data) " report(s) to " filename))))

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
  [reports filename label]
  (let [data  (mapv report->map reports)
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
    (println (str "Wrote " (count data) " report(s) to " filename))))

;; ---------------------------------------------------------------------------
;; Org output
;; ---------------------------------------------------------------------------

(defn- report->org-entry [m]
  (let [todo    (if (= (nth (:flags m "---") 2) \C) "DONE" "TODO")
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
                   (str ":PRIORITY: " (:priority m 0))
                   (str ":REPLIES: " (:replies m 0))
                   (when-let [v (:version m)]      (str ":VERSION: " v))
                   (when-let [t (:topic m)]        (str ":TOPIC: " t))
                   (when-let [v (:votes m)]        (str ":VOTES: " v))
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
  [reports filename label]
  (let [data    (mapv report->map reports)
        entries (str/join "\n" (map report->org-entry data))]
    (spit filename
          (str "#+TITLE: BARK " label " reports\n"
               "#+DATE: " (str (java.time.LocalDate/now)) "\n\n"
               entries))
    (println (str "Wrote " (count data) " report(s) to " filename))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(def formats #{"json" "rss" "org"})

(let [args    *command-line-args*
      command (or (first args) "reports")
      format  (or (second args) "json")
      db-path (or (System/getenv "BARK_DB") "data/bark-db")
      conn    (d/get-conn db-path schema)]
  (try
    (when-not (formats format)
      (println (str "Unknown format: " format))
      (println "Formats: json rss org")
      (System/exit 1))
    (let [db (d/db conn)
          [report-type label]
          (case command
            "bugs"          [:bug "bug"]
            "patches"       [:patch "patch"]
            "requests"      [:request "request"]
            "announcements" [:announcement "announcement"]
            "releases"      [:release "release"]
            "changes"       [:change "change"]
            "reports"       [nil "report"]
            (do (println (str "Unknown command: " command))
                (println "Usage: bb <command> [json|rss|org]")
                (println "Commands: bugs patches requests announcements releases changes reports")
                (System/exit 1)))
          reports  (if report-type
                     (all-reports-by-type db report-type)
                     (all-reports db))
          basename command]
      (if (empty? reports)
        (println (str "No " label " reports found."))
        (case format
          "json" (dump-json! reports (str basename ".json"))
          "rss"  (dump-rss!  reports (str basename ".rss") label)
          "org"  (dump-org!  reports (str basename ".org") label))))
    (finally
      (d/close conn))))
