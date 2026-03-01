#!/usr/bin/env bb

;; bark-egest.clj — Display and dump BARK reports.
;;
;; Interactive display via gum (charmbracelet.com/gum) and bblgum,
;; or JSON dump to file.
;;
;; Usage:
;;   bb bugs                — interactive table of bug reports
;;   bb bugs json           — dump bugs as bugs.json
;;   bb patches / patches json
;;   bb requests / requests json
;;   bb announcements / announcements json
;;   bb releases / releases json
;;   bb changes / changes json
;;   bb reports / reports json
;;   bb roles               — show per-mailbox roles
;;
;; Environment / defaults:
;;   BARK_DB — path to db (default: ./data/bark-db)

(require '[babashka.pods :as pods]
         '[babashka.deps :as deps]
         '[babashka.process :as process]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.pprint :as pprint])

(deps/add-deps '{:deps {io.github.lispyclouds/bblgum
                         {:git/sha "881a426d9df9df40eb305ceaeb3996ea1c7ae0d3"}}})

(require '[bblgum.core :as gum])

(pods/load-pod 'huahaiy/datalevin "0.10.5")

(require '[pod.huahaiy.datalevin :as d])

;; ---------------------------------------------------------------------------
;; Schema — loaded from shared bark-schema.edn
;; ---------------------------------------------------------------------------

(def schema
  (edn/read-string (slurp "bark-schema.edn")))

;; ---------------------------------------------------------------------------
;; Config
;; ---------------------------------------------------------------------------

(defn- mailbox-id [mb] (str (:host mb) ":" (:user mb)))

(defn- build-mailbox-map [config]
  (let [default-admin (:admin config)]
    (into {}
          (map (fn [mb]
                 [(mailbox-id mb)
                  {:email              (:email mb)
                   :admin              (or (:admin mb) default-admin)
                   :mailing-list-email (:mailing-list-email mb)}]))
          (:mailboxes config))))

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
    {:report/series [:series/id :series/expected :series/closed
                     {:series/patches [:db/id]}
                     {:series/cover-letter [:email/message-id]}]}
    {:report/email [:email/subject :email/from-address
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

(defn get-roles [db mailbox-email]
  (or (d/pull db '[:roles/admin :roles/maintainers :roles/ignored]
              [:roles/mailbox-email mailbox-email])
      {}))

;; ---------------------------------------------------------------------------
;; Formatting helpers
;; ---------------------------------------------------------------------------

(def ^:private flag-defs
  [[:report/acked "A"] [:report/owned "O"] [:report/closed "C"]
   [:report/urgent "U"] [:report/important "I"]])

(defn- flags-str [report]
  (apply str (map (fn [[k c]] (if (get report k) c "-")) flag-defs)))

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

(defn- series-str
  "Format series info: received/expected, e.g. \"2/5\"."
  [report]
  (when-let [s (:report/series report)]
    (let [received (count (:series/patches s))
          expected (:series/expected s)
          closed?  (some? (:series/closed s))]
      (str received "/" expected (when closed? " closed")))))

(defn- extra-str [report]
  (let [parts (remove nil?
                [(when-let [v (:report/version report)] v)
                 (when-let [t (:report/topic report)] t)
                 (when-let [s (:report/patch-seq report)] s)
                 (when-let [sources (:report/patch-source report)]
                   (str "src:" (str/join "," (map name sources))))
                 (when-let [v (votes-str report)] (str "votes:" v))
                 (when-let [s (series-str report)] (str "series:" s))])]
    (when (seq parts) (str/join " " parts))))

;; ---------------------------------------------------------------------------
;; Roles helpers
;; ---------------------------------------------------------------------------

(defn- roles-set [roles attr]
  (let [v (get roles attr)]
    (cond (nil? v) #{} (string? v) #{v} :else (set v))))

;; ---------------------------------------------------------------------------
;; Report → map for JSON
;; ---------------------------------------------------------------------------

(defn- get-header
  "Case-insensitive header lookup from a parsed headers map."
  [headers name]
  (some (fn [[k v]] (when (= (str/lower-case k) (str/lower-case name)) v)) headers))

(defn- archived-at [email]
  (when-let [edn-str (:email/headers-edn email)]
    (get-header (edn/read-string edn-str) "Archived-At")))

(defn- report->map [report]
  (let [email (:report/email report)
        arch  (archived-at email)
        votes (votes-str report)
        series (:report/series report)]
    (cond-> {:type    (name (:report/type report))
             :subject (or (:email/subject email) "")
             :from    (or (:email/from-address email) "")
             :date    (format-date (:email/date-sent email))
             :flags   (flags-str report)
             :replies (descendant-count report)}
      (:report/message-id report)   (assoc :message-id (:report/message-id report))
      (:report/version report)      (assoc :version (:report/version report))
      (:report/topic report)        (assoc :topic (:report/topic report))
      (:report/patch-seq report)    (assoc :patch-seq (:report/patch-seq report))
      (:report/patch-source report) (assoc :patch-source (mapv name (:report/patch-source report)))
      arch                          (assoc :archived-at arch)
      votes                         (assoc :votes votes)
      series                        (assoc :series
                                           {:received (count (:series/patches series))
                                            :expected (:series/expected series)
                                            :complete (= (count (:series/patches series))
                                                         (:series/expected series))
                                            :closed   (some? (:series/closed series))}))))

;; ---------------------------------------------------------------------------
;; Display: gum table
;; ---------------------------------------------------------------------------

(defn- report->row
  "Format a report as a tab-separated row for gum table."
  [report show-type?]
  (let [email (:report/email report)]
    (str/join "\t"
              (concat
               (when show-type? [(name (:report/type report))])
               [(flags-str report)
                (str (descendant-count report))
                (or (:email/from-address email) "?")
                (format-date (:email/date-sent email))
                (or (:email/subject email) "(no subject)")]))))

(defn- report->line
  "Format a report as a plain text line."
  [report show-type?]
  (let [email (:report/email report)]
    (str (when show-type? (format "[%-12s] " (name (:report/type report))))
         (format "%-5s %3d %-25s %s  %s"
                 (flags-str report)
                 (descendant-count report)
                 (or (:email/from-address email) "?")
                 (format-date (:email/date-sent email))
                 (or (:email/subject email) "(no subject)"))
         (when-let [e (extra-str report)] (str " " e)))))

(defn- gum-available? []
  (try
    (zero? (:exit (process/shell {:out :string :err :string :continue true} "gum" "--version")))
    (catch Exception _ false)))

;; ---------------------------------------------------------------------------
;; Display
;; ---------------------------------------------------------------------------

(defn display-reports!
  "Display reports interactively with gum table.
  Selecting a row prints the full report data as EDN."
  [reports label show-type?]
  (if (empty? reports)
    (println (str "No " label "s found."))
    (if (gum-available?)
      (let [columns (concat
                     (when show-type? ["Type"])
                     ["Flags" "#" "From" "Date" "Subject"])
            rows    (mapv #(report->row % show-type?) reports)
            input   (str/join "\n" rows)
            {:keys [status result]}
            (gum/gum :table
                     :in input
                     :columns (str/join "," columns)
                     :separator "\t")]
        (when (and (zero? status) (seq result))
          ;; gum table returns the selected CSV row — match it back to report
          (let [selected (first result)
                idx      (.indexOf ^java.util.List rows selected)]
            (when (>= idx 0)
              (let [report (nth reports idx)]
                (pprint/pprint (report->map report)))))))
      ;; Plain text fallback
      (do (println (str (count reports) " " label "(s):\n"))
          (doseq [r reports]
            (println (str "  " (report->line r show-type?))))))))

(defn dump-reports!
  "Dump reports as JSON to a file."
  [reports filename]
  (let [data (mapv report->map reports)]
    (spit filename (json/generate-string data {:pretty true}))
    (println (str "Wrote " (count data) " report(s) to " filename))))

;; ---------------------------------------------------------------------------
;; Commands
;; ---------------------------------------------------------------------------

(defn cmd-list [db report-type output]
  (let [reports (if report-type
                  (all-reports-by-type db report-type)
                  (all-reports db))
        label   (if report-type (name report-type) "report")]
    (case output
      "json" (dump-reports! reports (str label "s.json"))
      ;; default: interactive display
      (display-reports! reports label (nil? report-type)))))

(defn cmd-roles [db mailbox-map]
  (if (empty? mailbox-map)
    (println "No mailboxes configured.")
    (doseq [[mb-id {:keys [email]}] (sort-by key mailbox-map)]
      (let [roles   (get-roles db email)
            maints  (roles-set roles :roles/maintainers)
            ignored (roles-set roles :roles/ignored)]
        (println (str "\n" email " (" mb-id ")"))
        (println (str "  Admin:       " (or (:roles/admin roles) "(none)")))
        (println (str "  Maintainers: " (if (seq maints)  (str/join ", " (sort maints))  "(none)")))
        (println (str "  Ignored:     " (if (seq ignored) (str/join ", " (sort ignored)) "(none)")))))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [args    *command-line-args*
      command (or (first args) "reports")
      output  (second args)
      db-path (or (System/getenv "BARK_DB") "data/bark-db")
      config  (let [f (clojure.java.io/file "config.edn")]
                (when (.exists f) (edn/read-string (slurp f))))
      conn    (d/get-conn db-path schema)]
  (try
    (let [db          (d/db conn)
          mailbox-map (if config (build-mailbox-map config) {})]
      (case command
        "bugs"          (cmd-list db :bug output)
        "patches"       (cmd-list db :patch output)
        "requests"      (cmd-list db :request output)
        "announcements" (cmd-list db :announcement output)
        "releases"      (cmd-list db :release output)
        "changes"       (cmd-list db :change output)
        "reports"       (cmd-list db nil output)
        "roles"         (cmd-roles db mailbox-map)
        (do (println (str "Unknown command: " command))
            (println "Usage: bb <command> [json]")
            (println "")
            (println "Commands:")
            (println "  bugs / patches / requests / announcements")
            (println "  releases / changes / reports / roles")
            (println "")
            (println "Append 'json' to dump as <type>s.json"))))
    (finally
      (d/close conn))))
