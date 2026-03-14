#!/usr/bin/env bb

;; bark-stats.clj — Compute statistics from BARK data.
;;
;; Usage:
;;   bb stats                        -> writes public/stats.json
;;   bb stats html                   -> writes public/data.html (Vega-Lite charts)
;;   bb scripts/bark-stats.clj html -o path/to/data.html
;;
;; Environment / defaults:
;;   BARK_DB — path to db (default: ./data/bark-db)

(require '[cheshire.core :as json]
         '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[hiccup2.core :as h])

;; Forward-declared for clj-kondo (provided at runtime by load-file calls below).
(declare load-datalevin-pod! all-reports report-pull-pattern
         report-priority report-status report-descendant-count
         format-date format-date-iso parse-cli-args load-config
         pico-cdn bark-description bark-repo-url footer-css
         bark-footer wrap-js theme-toggle-js bark-schema
         nav-bar theme-toggle-btn)

(load-file "scripts/bark-common.clj")
(load-file "scripts/bark-html.clj")

(load-datalevin-pod!)

;; ---------------------------------------------------------------------------
;; JS (loaded from resources/ at build time)
;; ---------------------------------------------------------------------------

(def ^:private stats-js (slurp "resources/bark-stats.js"))

(def db-path
  (or (System/getenv "BARK_DB") "./data/bark-db"))

;; ---------------------------------------------------------------------------
;; Queries (all-reports and report-pull-pattern loaded from bark-common.clj)
;; ---------------------------------------------------------------------------

(defn total-emails [db]
  (-> (d/q '[:find (count ?e) :where [?e :email/message-id _]] db)
      ffirst (or 0)))

;; ---------------------------------------------------------------------------
;; Time helpers
;; ---------------------------------------------------------------------------

(defn now-ms [] (System/currentTimeMillis))
(def one-year-ms (* 365 24 60 60 1000))

(defn within-last-year? [inst]
  (when inst (> (.getTime inst) (- (now-ms) one-year-ms))))

(defn days-between [a b]
  (when (and a b)
    (/ (Math/abs (- (.getTime b) (.getTime a)))
       (* 24.0 60 60 1000))))

(defn round2 [x]
  (when x (/ (Math/round (* x 100.0)) 100.0)))

;; ---------------------------------------------------------------------------
;; Stat helpers
;; ---------------------------------------------------------------------------

(defn report-date   [r] (get-in r [:report/email :email/date-sent]))
(defn report-author [r] (get-in r [:report/email :email/from-address]))

(defn open->close-days [r]
  (days-between (report-date r)
                (get-in r [:report/closed :email/date-sent])))

(defn median [sorted-nums]
  (let [n (count sorted-nums)]
    (when (pos? n)
      (if (odd? n)
        (nth sorted-nums (quot n 2))
        (/ (+ (nth sorted-nums (quot n 2))
              (nth sorted-nums (dec (quot n 2))))
           2.0)))))

;; ---------------------------------------------------------------------------
;; Stat computations
;; ---------------------------------------------------------------------------

(defn reports-per-type [reports]
  (->> reports
       (filter #(within-last-year? (report-date %)))
       (group-by #(some-> (:report/type %) name))
       (into {} (map (fn [[t rs]] [t (count rs)])))))

(def ^:private ym-formatter
  "Locale-independent yyyy-MM formatter (UTC)."
  (doto (java.text.SimpleDateFormat. "yyyy-MM" java.util.Locale/ENGLISH)
    (.setTimeZone (java.util.TimeZone/getTimeZone "UTC"))))

(defn- date->ym
  "Extract 'yyyy-MM' from a java.util.Date using a locale-independent formatter.
  Returns nil on failure."
  [date]
  (when date
    (try (.format ym-formatter date)
         (catch Exception _ nil))))

(defn- current-ym
  "Return [year month] for right now, locale-independent."
  []
  (let [s (.format ym-formatter (java.util.Date.))
        [year month] (str/split s #"-")]
    [(parse-long year) (parse-long month)]))

(defn reports-by-month [reports]
  (let [;; Group all reports by yyyy-MM (local time, matching what users see)
        counts (->> reports
                    (keep (fn [r] (date->ym (report-date r))))
                    frequencies)
        ;; Generate the last 12 month labels
        [cy cm] (current-ym)
        months  (vec (for [i (range 11 -1 -1)]
                       (let [total (+ (* cy 12) (dec cm) (- i))
                             y     (quot total 12)
                             m     (inc (mod total 12))]
                         (format "%04d-%02d" y m))))]
    ;; Return eager vector of [month count] pairs
    (mapv (fn [m] [m (get counts m 0)]) months)))

(defn email-vs-reports-ratio [reports total-email-count]
  (let [n (count (filter #(within-last-year? (report-date %)) reports))]
    {:reports-last-year n
     :total-emails      total-email-count
     :ratio             (when (pos? total-email-count)
                          (round2 (/ n (double total-email-count))))}))

(defn time-to-close-stats [reports]
  (let [durations (->> reports
                       (filter #(and (:report/closed %) (report-date %)))
                       (map open->close-days)
                       (remove nil?)
                       sort vec)]
    (when (seq durations)
      {:count          (count durations)
       :min-days       (round2 (first durations))
       :max-days       (round2 (last durations))
       :avg-days       (round2 (/ (reduce + durations) (count durations)))
       :median-days    (round2 (median durations))
       :buckets        {:same-day       (count (filter #(<  % 1)    durations))
                        :within-week    (count (filter #(and (>= % 1)  (< % 7))   durations))
                        :within-month   (count (filter #(and (>= % 7)  (< % 30))  durations))
                        :within-quarter (count (filter #(and (>= % 30) (<= % 90)) durations))
                        :longer         (count (filter #(>  % 90)    durations))}})))

(defn top-openers [reports n]
  (->> reports
       (filter #(within-last-year? (report-date %)))
       (group-by report-author)
       (map (fn [[addr rs]]
              {:address addr
               :name    (get-in (first rs) [:report/email :email/from-name])
               :count   (count rs)}))
       (sort-by :count >) (take n)))

(def closable-types
  "Report types where open/closed status is meaningful."
  #{:bug :patch :request})

(defn open-closed-ratio [reports]
  (let [closable (filter #(closable-types (:report/type %)) reports)
        open     (count (remove :report/closed closable))
        closed   (count (filter :report/closed closable))]
    {:open open :closed closed
     :ratio (when (pos? (+ open closed))
              (round2 (/ open (double (+ open closed)))))}))

(defn vote-leaders [reports n]
  (->> reports
       (filter #(pos? (+ (or (:report/votes-up %) 0)
                         (or (:report/votes-down %) 0)
                         (or (:report/votes-null %) 0))))
       (map (fn [r] {:message-id (:report/message-id r)
                     :topic      (get-in r [:report/email :email/subject])
                     :votes-up   (or (:report/votes-up r) 0)
                     :votes-down (or (:report/votes-down r) 0)
                     :votes-null (or (:report/votes-null r) 0)
                     :score      (- (or (:report/votes-up r) 0) (or (:report/votes-down r) 0))}))
       (sort-by :score >) (take n)))

(defn compute-stats
  ([reports] (compute-stats reports nil))
  ([reports db]
   (let [last-year     (filter #(within-last-year? (report-date %)) reports)
         closable-yr   (filter #(closable-types (:report/type %)) last-year)
         total-emails  (when db (total-emails db))]
     (cond->
       {:generated-at      (str (java.util.Date.))
        :reports-per-type  (reports-per-type reports)
        :reports-by-month  (reports-by-month reports)
        :time-to-close     (time-to-close-stats reports)
        :open-closed-ratio (open-closed-ratio reports)
        :open-last-year    (count (remove :report/closed closable-yr))
        :total-last-year   (count last-year)
        :top-openers       (top-openers reports 10)
        :vote-leaders      (vote-leaders reports 10)}
       total-emails (assoc :email-ratio (email-vs-reports-ratio reports total-emails))))))

;; ---------------------------------------------------------------------------
;; HTML / Vega-Lite rendering
;; ---------------------------------------------------------------------------

(defn vl [title mark data encoding & [extra]]
  (merge {:$schema  "https://vega.github.io/schema/vega-lite/v5.json"
          :title    title
          :width    "container"
          :mark     (merge {:type mark :tooltip true} (when (= mark "bar") {:cornerRadiusEnd 3}))
          :data     {:values data}
          :encoding encoding}
         extra))

(def stats-css (str "
   main.container { max-width: 1600px; }
  .kpis { display: flex; flex-wrap: wrap; gap: 1rem; margin-bottom: 1rem; }
  .kpi  { border: 1px solid var(--pico-muted-border-color); border-radius: var(--pico-border-radius);
          padding: 0.9rem 1.3rem; min-width: 130px; flex: 1 1 130px; }
  .kpi-v { font-size: 1.9rem; font-weight: 700; color: var(--pico-primary); }
  .kpi-l { font-size: 0.75rem; color: var(--pico-muted-color); margin-top: 0.2rem; }
  .kpi-s { font-size: 0.7rem;  color: var(--pico-muted-color); opacity: 0.7; margin-top: 0.1rem; }
  .grid  { display: grid; grid-template-columns: repeat(auto-fit, minmax(340px, 1fr)); gap: 1.2rem; }
  .box   { border: 1px solid var(--pico-muted-border-color); border-radius: var(--pico-border-radius);
           padding: 1rem; }
  .chart { width: 100%; }
  .meta  { font-size: 0.78rem; color: var(--pico-muted-color); margin-bottom: 2rem; }
  .theme-toggle { cursor: pointer; background: none; border: none; font-size: 1.2rem; padding: 0.3rem; }
" footer-css))

(defn chart-div [id spec]
  (str "<div class=\"chart\" id=\"" id "\"></div>"
       "<script>" (wrap-js (str "barkSpecs['" id "']=" (json/generate-string spec) ";")) "</script>"))

;; Individual chart specs

(defn chart-by-month [by-month]
  (vl "Reports per month (last 12 months)" "bar"
      (vec (map (fn [[m c]] {"month" m "count" c}) by-month))
      {:x {:field "month" :type "ordinal" :sort "ascending"
           :axis  {:labelAngle -45}}
       :y {:field "count" :type "quantitative" :title "Reports"}}))

(defn chart-per-type [per-type]
  (vl "Report types (last year)" "arc"
      (map (fn [[t c]] {"type" t "count" c}) per-type)
      {:theta {:field "count" :type "quantitative"}
       :color {:field "type"  :type "nominal" :title "Type"}}
      {:height 220}))

(defn chart-ttc [ttc]
  (let [order ["same-day" "≤1 week" "≤1 month" "≤3 months" ">3 months"]
        data  [{"b" "same-day"   "n" (get-in ttc [:buckets :same-day] 0)}
               {"b" "≤1 week"    "n" (get-in ttc [:buckets :within-week] 0)}
               {"b" "≤1 month"   "n" (get-in ttc [:buckets :within-month] 0)}
               {"b" "≤3 months"  "n" (get-in ttc [:buckets :within-quarter] 0)}
               {"b" ">3 months"  "n" (get-in ttc [:buckets :longer] 0)}]]
    (vl "Time to close" "bar" data
        {:x {:field "b" :type "ordinal" :title nil :sort order}
         :y {:field "n" :type "quantitative" :title "Reports"}})))

(defn chart-openers [openers]
  (let [data (->> openers
                  (map (fn [{:keys [address name count]}]
                         {"user" (or (when (seq name) name) address) "count" count})))]
    (vl "Top 10 openers (last year)" "bar" data
        {:y {:field "user"  :type "ordinal" :title nil
             :sort {:field "count" :order "descending"}
             :axis  {:labelLimit 180}}
         :x {:field "count" :type "quantitative" :title "Reports opened"}})))

;; HTML assembly

(defn kpi [value label & [sub]]
  (str "<div class=\"kpi\">"
       "<div class=\"kpi-v\">" value "</div>"
       "<div class=\"kpi-l\">" label "</div>"
       (when sub (str "<div class=\"kpi-s\">" sub "</div>"))
       "</div>"))

(defn chart-box [id spec]
  (str "<div class=\"box\">" (chart-div id spec) "</div>"))

;; ---------------------------------------------------------------------------
;; data.org rendering (reuses org table parser from bark-howto logic)
;; ---------------------------------------------------------------------------

(defn- org-inline-data [s]
  (-> s
      (str/replace #"\[\[([^\]]+)\]\[([^\]]+)\]\]" "<a href=\"$1\">$2</a>")
      (str/replace #"\[\[([^\]]+)\]\]" "<a href=\"$1\">$1</a>")))

(defn- parse-data-table
  "Parse org table lines into an HTML table string."
  [lines]
  (let [rows (->> lines
                  (remove #(re-matches #"\s*\|[-+]+\|\s*" %))
                  (mapv (fn [line]
                          (->> (str/split line #"\|" -1)
                               (drop 1) butlast
                               (mapv str/trim)))))]
    (when (seq rows)
      (let [header (first rows)
            body   (rest rows)]
        (str "<table>\n<thead><tr>"
             (str/join (map #(str "<th>" (org-inline-data %) "</th>") header))
             "</tr></thead>\n<tbody>\n"
             (str/join (map (fn [r]
                              (str "<tr>"
                                   (str/join (map #(str "<td>" (org-inline-data %) "</td>") r))
                                   "</tr>\n"))
                            body))
             "</tbody></table>")))))

(defn render-data-section
  "Render resources/data.org as an HTML section, filtering dead links."
  [out-dir]
  (let [org-text (slurp "resources/data.org")
        lines    (str/split-lines org-text)
        tlines   (filterv #(str/starts-with? (str/trim %) "|") lines)
        ;; Filter out rows whose link targets don't exist
        filtered (if out-dir
                   (filterv (fn [line]
                              (if (re-find #"\[\[" line)
                                (let [targets (re-seq #"\[\[([^\]]+)\]\[" line)]
                                  (some (fn [[_ target]]
                                          (.exists (io/file out-dir target)))
                                        targets))
                                true))
                            tlines)
                   tlines)]
    (when (seq filtered)
      (str "<h3>Available data</h3>\n"
           (parse-data-table filtered)))))

;; ---------------------------------------------------------------------------
;; HTML assembly
;; ---------------------------------------------------------------------------

(defn render-html [stats out-dir]
  (let [{:keys [generated-at reports-per-type reports-by-month
                time-to-close open-closed-ratio open-last-year
                top-openers email-ratio]} stats
        n-yr (reduce + (vals reports-per-type))
        pct  #(when % (str (Math/round (* 100.0 %)) "%"))
        nav-html (str (h/html (nav-bar "BARK — Data" "data")))
        data-section (render-data-section out-dir)]
    (str
     "<!DOCTYPE html>\n"
     "<html lang=\"en\" data-theme=\"light\">\n"
     "<head>\n"
     "<meta charset=\"UTF-8\">\n"
     "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">\n"
     "<meta name=\"color-scheme\" content=\"light dark\">\n"
     "<meta name=\"description\" content=\"" bark-description "\">\n"
     "<meta property=\"og:title\" content=\"BARK — Data\">\n"
     "<meta property=\"og:description\" content=\"" bark-description "\">\n"
     "<meta property=\"og:type\" content=\"website\">\n"
     "<link rel=\"stylesheet\" href=\"" pico-cdn "\">\n"
     "<script src=\"https://cdn.jsdelivr.net/npm/vega@5/build/vega.min.js\"></script>\n"
     "<script src=\"https://cdn.jsdelivr.net/npm/vega-lite@5/build/vega-lite.min.js\"></script>\n"
     "<script src=\"https://cdn.jsdelivr.net/npm/vega-embed@6/build/vega-embed.min.js\"></script>\n"
     "<title>BARK — Data</title>\n"
     "<style>" stats-css "</style>\n"
     "<script>\n" (wrap-js theme-toggle-js) "\n</script>\n"
     "<script>\n" (wrap-js stats-js) "\n</script>\n"
     "</head>\n<body>\n"
     "<main class=\"container\">\n"
     nav-html "\n"
     "<p class=\"meta\">Generated " generated-at "</p>\n"

     (when data-section (str data-section "\n"))

     "<h3>Statistics</h3>\n"
     "<div class=\"kpis\">\n"
     (kpi n-yr "Reports (last year)"
          (str open-last-year " still open"))
     (kpi (:open open-closed-ratio) "Open (all time)"
          (str (pct (:ratio open-closed-ratio)) " of all"))
     (kpi (:closed open-closed-ratio) "Closed (all time)")
     (when time-to-close
       (kpi (str (:median-days time-to-close) "d") "Median to close"
            (str "avg " (:avg-days time-to-close) "d")))
     (when email-ratio
       (kpi (or (:ratio email-ratio) "—") "Report/email ratio"
            (str (:reports-last-year email-ratio) " reports / "
                 (:total-emails email-ratio) " emails")))
     "</div>\n"

     "<div class=\"grid\">\n"
     (chart-box "chart-month"   (chart-by-month reports-by-month))
     (chart-box "chart-type"    (chart-per-type reports-per-type))
     (when time-to-close
       (chart-box "chart-ttc"   (chart-ttc time-to-close)))
     (chart-box "chart-openers" (chart-openers top-openers))
     "</div>\n"

     "</main>\n"
     (str (h/html (bark-footer)))
     "</body>\n</html>\n")))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn -main [& args]
  (let [opts        (parse-cli-args args)
        html?       (= (:format opts) "html")
        source-name (:source-name opts)
        out-file    (or (:out-file opts)
                        (if html? "public/web/data.html" "public/reports/stats.json"))
        out-dir     (or (:out-dir opts)
                        (.getParent (io/file out-file)))
        conn        (d/get-conn db-path bark-schema {:wal? false})
        db          (d/db conn)
        all-reps    (all-reports db)
        reports     (if source-name
                      (filter #(= source-name (get-in % [:report/email :email/source])) all-reps)
                      all-reps)
        stats       (compute-stats reports db)]
    (io/make-parents out-file)
    (if html?
      (do (spit out-file (render-html stats out-dir))
          (log/info "Wrote" out-file "(HTML," (count reports) "reports)"))
      (do (spit out-file (json/generate-string stats {:pretty true}))
          (log/info "Wrote" out-file "(JSON," (count reports) "reports)")))))
(apply -main *command-line-args*)
