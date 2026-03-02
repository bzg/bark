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
                    :email/date-sent :email/mailbox :email/imap-uid
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
;; Config & mailbox map
;; ---------------------------------------------------------------------------

(defn- load-config []
  (let [f (clojure.java.io/file "config.edn")]
    (when (.exists f) (edn/read-string (slurp f)))))

(defn- mailbox-id [mb] (str (:host mb) ":" (:user mb)))

(defn- build-mailbox-map
  "Build mailbox-id → {:name :email :admin} map.
   Per-mailbox :admin takes precedence over top-level :admin."
  [config]
  (let [default-admin (:admin config)]
    (into {}
          (map (fn [mb]
                 [(mailbox-id mb)
                  {:name  (:name mb)
                   :email (:email mb)
                   :admin (str/lower-case (or (:admin mb) default-admin ""))}]))
          (:mailboxes config))))

(defn- build-maintainers
  "Gather per-mailbox maintainer sets from DB roles.
   Returns mailbox-id → #{maintainer-emails}."
  [config db mailbox-map]
  (into {}
        (map (fn [[mb-id {:keys [email]}]]
               (let [roles (d/pull db '[:roles/maintainers]
                                   [:roles/mailbox-email email])
                     v     (:roles/maintainers roles)
                     maints (cond (nil? v) #{}
                                  (string? v) #{(str/lower-case v)}
                                  :else (set (map str/lower-case v)))]
                 [mb-id maints])))
        mailbox-map))

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

(defn- sender-role
  "Determine role of sender for a given mailbox context."
  [from mb-id mailbox-map maintainers-map]
  (when (and (seq from) mb-id)
    (let [from-lc (str/lower-case from)
          mb-info (get mailbox-map mb-id)
          admin   (:admin mb-info)]
      (cond
        (= from-lc admin)                                        "admin"
        (contains? (get maintainers-map mb-id #{}) from-lc)      "maintainer"
        :else                                                    nil))))

(defn report->map [report mailbox-map maintainers-map multi-mailbox?]
  (let [email   (:report/email report)
        mb-id   (:email/mailbox email)
        from    (or (:email/from-address email) "")
        arch    (archived-at email)
        votes   (votes-str report)
        series  (:report/series report)
        related (:report/related report)
        role    (sender-role from mb-id mailbox-map maintainers-map)
        mb-name (get-in mailbox-map [mb-id :name])]
    (cond-> {:type     (name (:report/type report))
             :subject  (or (:email/subject email) "")
             :from     from
             :date     (format-date (:email/date-sent email))
             :date-raw (str (:email/date-sent email))
             :flags    (flags-str report)
             :priority (priority report)
             :replies  (descendant-count report)}
      (:email/from-name email)      (assoc :from-name (:email/from-name email))
      role                          (assoc :role role)
      (and multi-mailbox? mb-name)  (assoc :mailbox mb-name)
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
  [reports filename mailbox-map maintainers-map multi-mb?]
  (let [data (mapv #(report->map % mailbox-map maintainers-map multi-mb?) reports)]
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
  [reports filename label mailbox-map maintainers-map multi-mb?]
  (let [data  (mapv #(report->map % mailbox-map maintainers-map multi-mb?) reports)
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
  [reports filename label mailbox-map maintainers-map multi-mb?]
  (let [data    (mapv #(report->map % mailbox-map maintainers-map multi-mb?) reports)
        entries (str/join "\n" (map report->org-entry data))]
    (spit filename
          (str "#+TITLE: BARK " label " reports\n"
               "#+DATE: " (str (java.time.LocalDate/now)) "\n\n"
               entries))
    (println (str "Wrote " (count data) " reports to " filename))))

;; ---------------------------------------------------------------------------
;; CLI helpers
;; ---------------------------------------------------------------------------

(defn- parse-args [args]
  (loop [opts {} [a & more] args]
    (cond
      (nil? a)                    opts
      (#{"-m" "--mailbox"} a)     (recur (assoc opts :mailbox-name (first more)) (rest more))
      (nil? (:command opts))      (recur (assoc opts :command a) more)
      (nil? (:format opts))       (recur (assoc opts :format a) more)
      :else                       opts)))

(defn- resolve-mailbox-ids
  "Given a mailbox name, return matching mailbox-ids from mailbox-map."
  [mailbox-map name]
  (keep (fn [[mb-id info]]
          (when (= (:name info) name) mb-id))
        mailbox-map))

(defn- filter-by-mailbox
  "Filter reports to only those from the given mailbox-ids."
  [reports mb-ids]
  (let [id-set (set mb-ids)]
    (filter #(contains? id-set (get-in % [:report/email :email/mailbox])) reports)))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(def formats #{"json" "rss" "org"})

(let [{:keys [command format mailbox-name]
       :or {command "reports" format "json"}}
      (parse-args *command-line-args*)
      db-path (or (System/getenv "BARK_DB") "data/bark-db")
      conn    (d/get-conn db-path schema)]
  (try
    (when-not (formats format)
      (println (str "Unknown format: " format))
      (println "Formats: json rss org")
      (System/exit 1))
    (let [db              (d/db conn)
          config          (load-config)
          mailbox-map     (if config (build-mailbox-map config) {})
          maintainers-map (if config (build-maintainers config db mailbox-map) {})
          multi-mb?       (> (count mailbox-map) 1)
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
                (println "Usage: bb export <command> [json|rss|org] [-m mailbox]")
                (println "Commands: bugs patches requests announcements releases changes reports")
                (System/exit 1)))
          all-reports (if report-type
                        (all-reports-by-type db report-type)
                        (all-reports db))
          reports     (if mailbox-name
                        (let [mb-ids (resolve-mailbox-ids mailbox-map mailbox-name)]
                          (if (empty? mb-ids)
                            (do (println (str "Error: no mailbox named '" mailbox-name "'"))
                                (println (str "Available: "
                                              (str/join ", " (keep :name (vals mailbox-map)))))
                                (System/exit 1))
                            (filter-by-mailbox all-reports mb-ids)))
                        all-reports)
          basename    command]
      (if (empty? reports)
        (println (str "No " label " reports found."
                      (when mailbox-name (str " (mailbox: " mailbox-name ")"))))
        (case format
          "json" (dump-json! reports (str basename ".json") mailbox-map maintainers-map multi-mb?)
          "rss"  (dump-rss!  reports (str basename ".rss") label mailbox-map maintainers-map multi-mb?)
          "org"  (dump-org!  reports (str basename ".org") label mailbox-map maintainers-map multi-mb?))))
    (finally
      (d/close conn))))
