#!/usr/bin/env bb

;; bark-egest.clj — Dump BARK reports as JSON.
;;
;; Usage:
;;   bb bugs-json           — dump bugs as bugs.json
;;   bb patches-json        — dump patches as patches.json
;;   bb reports-json        — dump all reports as reports.json
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
    {:report/related [:report/type :report/message-id]}
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

;; ---------------------------------------------------------------------------
;; Report → map for JSON
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
                                           (let [patches (:series/patches series)]
                                             {:received (count patches)
                                              :expected (:series/expected series)
                                              :complete (= (count patches)
                                                           (:series/expected series))
                                              :closed   (some? (:series/closed series))}))
      (seq related)                 (assoc :related
                                           (mapv (fn [r]
                                                   {:type       (name (:report/type r))
                                                    :message-id (:report/message-id r)})
                                                 related)))))

;; ---------------------------------------------------------------------------
;; JSON dump
;; ---------------------------------------------------------------------------

(defn dump-reports!
  "Dump reports as JSON to a file."
  [reports filename]
  (let [data (mapv report->map reports)]
    (spit filename (json/generate-string data {:pretty true}))
    (println (str "Wrote " (count data) " report(s) to " filename))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [args    *command-line-args*
      command (or (first args) "reports")
      db-path (or (System/getenv "BARK_DB") "data/bark-db")
      conn    (d/get-conn db-path schema)]
  (try
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
                (println "Usage: bb <command>")
                (println "Commands: bugs patches requests announcements releases changes reports")
                (System/exit 1)))]
      (let [reports (if report-type
                      (all-reports-by-type db report-type)
                      (all-reports db))]
        (dump-reports! reports (str label "s.json"))))
    (finally
      (d/close conn))))
