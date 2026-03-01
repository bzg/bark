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
         '[clojure.edn :as edn])

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
    :report/descendants :report/digested-at
    {:report/email [:email/subject :email/from-address
                    :email/date-sent :email/imap-uid]}])

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

(defn- flags-str [report]
  (str (if (:report/acked report)     "A" "-")
       (if (:report/owned report)     "O" "-")
       (if (:report/closed report)    "C" "-")
       (if (:report/urgent report)    "U" "-")
       (if (:report/important report) "I" "-")))

(defn- descendant-count [report]
  (let [d (:report/descendants report)]
    (if (coll? d) (count d) 0)))

(defn- format-date [date]
  (let [s (str (or date ""))]
    (subs s 0 (min 16 (count s)))))

(defn- extra-str [report]
  (let [parts (remove nil?
                [(when-let [v (:report/version report)] v)
                 (when-let [t (:report/topic report)] t)
                 (when-let [s (:report/patch-seq report)] s)
                 (when-let [sources (:report/patch-source report)]
                   (str "src:" (str/join "," (map name sources))))])]
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

(defn- report->map [report]
  (let [email (:report/email report)]
    (cond-> {:type    (name (:report/type report))
             :subject (or (:email/subject email) "")
             :from    (or (:email/from-address email) "")
             :date    (format-date (:email/date-sent email))
             :flags   (flags-str report)
             :replies (descendant-count report)}
      (:report/message-id report)  (assoc :message-id (:report/message-id report))
      (:report/version report)     (assoc :version (:report/version report))
      (:report/topic report)       (assoc :topic (:report/topic report))
      (:report/patch-seq report)   (assoc :patch-seq (:report/patch-seq report))
      (:report/patch-source report) (assoc :patch-source (mapv name (:report/patch-source report))))))

;; ---------------------------------------------------------------------------
;; Display: gum table
;; ---------------------------------------------------------------------------

(defn- report->row
  "Format a report as a CSV row for gum table."
  [report show-type?]
  (let [email (:report/email report)
        ;; Escape commas in fields by quoting
        esc   (fn [s] (if (and s (str/includes? s ","))
                        (str "\"" (str/replace s "\"" "\"\"") "\"")
                        (or s "")))]
    (str/join ","
              (concat
               (when show-type? [(name (:report/type report))])
               [(flags-str report)
                (str (descendant-count report))
                (esc (or (:email/from-address email) "?"))
                (format-date (:email/date-sent email))
                (esc (or (:email/subject email) "(no subject)"))
                (esc (or (extra-str report) ""))]))))

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

(defn display-reports!
  "Display reports interactively with gum table, or fall back to plain text."
  [reports label show-type?]
  (if (empty? reports)
    (println (str "No " label "s found."))
    (if (gum-available?)
      (let [header (str/join ","
                             (concat
                              (when show-type? ["Type"])
                              ["Flags" "#" "From" "Date" "Subject" "Extra"]))
            rows   (mapv #(report->row % show-type?) reports)
            input  (str/join "\n" (cons header rows))
            {:keys [result]} (gum/gum :table :in input :print true)]
        ;; gum table -p prints to stdout via bblgum, result captured
        (doseq [line result] (println line)))
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
