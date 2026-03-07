#!/usr/bin/env bb

;; bark-stats.clj — Compute statistics from BARK data.
;;
;; Usage:
;;   bb stats                        → writes public/stats.json
;;   bb stats html                   → writes public/stats.html (Vega-Lite charts)
;;   bb scripts/bark-stats.clj html -o path/to/stats.html
;;
;; Environment / defaults:
;;   BARK_DB — path to db (default: ./data/bark-db)

(require '[babashka.pods :as pods]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io])

(load-file "scripts/bark-common.clj")
(load-file "scripts/bark-html.clj")

(pods/load-pod 'huahaiy/datalevin "0.10.7")
(require '[pod.huahaiy.datalevin :as d])

;; ---------------------------------------------------------------------------
;; Schema & DB
;; ---------------------------------------------------------------------------

(def schema
  (edn/read-string (slurp "resources/bark-schema.edn")))

(def db-path
  (or (System/getenv "BARK_DB") "./data/bark-db"))

;; ---------------------------------------------------------------------------
;; Queries
;; ---------------------------------------------------------------------------

(def report-pull-pattern
  '[:db/id :report/type :report/digested-at
    :report/urgent :report/important
    :report/votes-up :report/votes-down
    {:report/acked  [:email/from-address :email/date-sent]}
    {:report/owned  [:email/from-address :email/date-sent]}
    {:report/closed [:email/from-address :email/date-sent]}
    {:report/series [:series/id :series/closed]}
    {:report/email  [:email/from-address :email/from-name
                     :email/date-sent :email/source :email/subject]}])

(defn all-reports [db]
  (->> (d/q '[:find (pull ?r pattern)
              :in $ pattern
              :where [?r :report/type _]]
            db report-pull-pattern)
       (map first)))

(defn total-emails [db]
  (-> (d/q '[:find (count ?e) :where [?e :email/uid _]] db)
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

(defn reports-by-month [reports]
  (->> reports
       (filter #(within-last-year? (report-date %)))
       (group-by #(subs (str (report-date %)) 0 7))
       (into (sorted-map) (map (fn [[m rs]] [m (count rs)])))))

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
       :buckets        {:same-day       (count (filter #(<  % 1)   durations))
                        :within-week    (count (filter #(<= 1  % 7)  durations))
                        :within-month   (count (filter #(<= 8  % 30) durations))
                        :within-quarter (count (filter #(<= 31 % 90) durations))
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

(defn top-trigger-users [reports n]
  (let [collect (fn [k rs] (->> rs (keep #(get-in % [k :email/from-address])) frequencies))
        ackers  (collect :report/acked  reports)
        owners  (collect :report/owned  reports)
        closers (collect :report/closed reports)
        users   (into #{} (concat (keys ackers) (keys owners) (keys closers)))]
    (->> users
         (map (fn [u] {:address u
                       :acked   (get ackers  u 0)
                       :owned   (get owners  u 0)
                       :closed  (get closers u 0)
                       :total   (+ (get ackers u 0) (get owners u 0) (get closers u 0))}))
         (sort-by :total >) (take n))))

(defn source-breakdown [reports]
  (->> reports
       (filter #(within-last-year? (report-date %)))
       (group-by #(get-in % [:report/email :email/source]))
       (into {} (map (fn [[src rs]] [(or src "unknown") (count rs)])))))

(defn open-closed-ratio [reports]
  (let [open   (count (remove :report/closed reports))
        closed (count (filter :report/closed reports))]
    {:open open :closed closed
     :ratio (when (pos? (+ open closed))
              (round2 (/ open (double (+ open closed)))))}))

(defn patch-series-stats [reports]
  (let [with-series   (filter :report/series reports)
        series-ids    (->> with-series (map #(get-in % [:report/series :series/id])) distinct count)
        closed-series (->> with-series (filter #(get-in % [:report/series :series/closed]))
                                       (map #(get-in % [:report/series :series/id])) distinct count)]
    {:unique-series series-ids :closed-series closed-series
     :patch-reports (count with-series)}))

(defn vote-leaders [reports n]
  (->> reports
       (filter #(pos? (or (:report/votes-up %) 0)))
       (map (fn [r] {:message-id (:report/message-id r)
                     :topic      (get-in r [:report/email :email/subject])
                     :votes-up   (or (:report/votes-up r) 0)
                     :votes-down (or (:report/votes-down r) 0)
                     :score      (- (or (:report/votes-up r) 0) (or (:report/votes-down r) 0))}))
       (sort-by :score >) (take n)))

(defn compute-stats [reports]
  (let [last-year (filter #(within-last-year? (report-date %)) reports)]
    {:generated-at      (str (java.util.Date.))
     :reports-per-type  (reports-per-type reports)
     :reports-by-month  (reports-by-month reports)
     :time-to-close     (time-to-close-stats reports)
     :open-closed-ratio (open-closed-ratio reports)
     :open-last-year    (count (remove :report/closed last-year))
     :total-last-year   (count last-year)
     :top-openers       (top-openers reports 10)}))

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

(def stats-css "
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
")

(defn chart-div [id spec]
  (str "<div class=\"chart\" id=\"" id "\"></div>"
       "<script>barkSpecs['" id "']=" (json/generate-string spec) ";</script>"))

;; Individual chart specs

(defn chart-by-month [by-month]
  (vl "Reports per month (last 12 months)" "bar"
      (map (fn [[m c]] {"month" m "count" c}) by-month)
      {:x {:field "month" :type "ordinal" :title nil :sort nil
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

(defn chart-sources [sources]
  (vl "Reports by source (last year)" "bar"
      (map (fn [[s c]] {"source" s "count" c}) sources)
      {:x {:field "source" :type "ordinal" :title nil
           :sort  "-y" :axis {:labelAngle -30 :labelLimit 120}}
       :y {:field "count"  :type "quantitative" :title "Reports"}}))

(defn chart-openers [openers]
  (let [data (->> openers
                  (map (fn [{:keys [address name count]}]
                         {"user" (or (when (seq name) name) address) "count" count})))]
    (vl "Top 10 openers (last year)" "bar" data
        {:y {:field "user"  :type "ordinal" :title nil
             :sort {:field "count" :order "descending"}
             :axis  {:labelLimit 180}}
         :x {:field "count" :type "quantitative" :title "Reports opened"}})))

(defn chart-triggers [trigger-users]
  (let [data (mapcat (fn [{:keys [address acked owned closed]}]
                       [{"user" address "action" "acked"  "n" acked}
                        {"user" address "action" "owned"  "n" owned}
                        {"user" address "action" "closed" "n" closed}])
                     trigger-users)]
    (vl "Maintainer activity (all time)" "bar" data
        {:y     {:field "user"   :type "ordinal" :title nil
                 :sort  "-x" :axis {:labelLimit 180}}
         :x     {:field "n"      :type "quantitative" :title "Actions"}
         :color {:field "action" :type "nominal"
                 :scale {:domain ["acked" "owned" "closed"]
                         :range  ["#aec6cf" "#7fb3d3" "#2e86c1"]}}})))

;; HTML assembly

(defn kpi [value label & [sub]]
  (str "<div class=\"kpi\">"
       "<div class=\"kpi-v\">" value "</div>"
       "<div class=\"kpi-l\">" label "</div>"
       (when sub (str "<div class=\"kpi-s\">" sub "</div>"))
       "</div>"))

(defn chart-box [id spec]
  (str "<div class=\"box\">" (chart-div id spec) "</div>"))

(defn render-html [stats]
  (let [{:keys [generated-at reports-per-type reports-by-month
                time-to-close open-closed-ratio open-last-year
                total-last-year top-openers]} stats
        n-yr (reduce + (vals reports-per-type))
        pct  #(when % (str (Math/round (* 100.0 %)) "%"))]
    (str
     "<!DOCTYPE html>\n"
     "<html lang=\"en\" data-theme=\"light\">\n"
     "<head>\n"
     "<meta charset=\"UTF-8\">\n"
     "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">\n"
     "<meta name=\"color-scheme\" content=\"light dark\">\n"
     "<link rel=\"stylesheet\" href=\"" pico-cdn "\">\n"
     "<script src=\"https://cdn.jsdelivr.net/npm/vega@5/build/vega.min.js\"></script>\n"
     "<script src=\"https://cdn.jsdelivr.net/npm/vega-lite@5/build/vega-lite.min.js\"></script>\n"
     "<script src=\"https://cdn.jsdelivr.net/npm/vega-embed@6/build/vega-embed.min.js\"></script>\n"
     "<title>BARK — Statistics</title>\n"
     "<style>" stats-css "</style>\n"
     "<script>\n"
     "var barkSpecs={};\n"
     "function barkTheme(){return document.documentElement.getAttribute('data-theme')==='dark'?'dark':'excel';}\n"
     "function barkRenderAll(){"
     "Object.entries(barkSpecs).forEach(function(kv){"
     "vegaEmbed('#'+kv[0],kv[1],{actions:false,renderer:'svg',theme:barkTheme()});});}"
     "\n"
     ;; shared toggleTheme from bark-html.clj, patched to also re-render charts
     (str/replace theme-toggle-js
                  "}\n"
                  "\n  barkRenderAll();\n}\n")
     "\n</script>\n"
     "</head>\n<body>\n"
     "<main class=\"container\">\n"
     "<nav><ul><li><strong>BARK — Statistics</strong></li></ul>"
     "<ul>"
     "<li><a href=\"index.html\">Reports</a></li>"
     (when (.exists (clojure.java.io/file "public/howto.html"))
       "<li><a href=\"howto.html\">How-to</a></li>")
     "<li><button class=\"theme-toggle\" onclick=\"toggleTheme()\" aria-label=\"Toggle theme\">"
     "<span id=\"theme-icon\">🌙</span></button></li>"
     "</ul></nav>\n"
     "<p class=\"meta\">Generated " generated-at "</p>\n"

     "<h2>Summary</h2>\n<div class=\"kpis\">\n"
     (kpi n-yr "Reports (last year)"
          (str open-last-year " still open"))
     (kpi (:open open-closed-ratio) "Open (all time)"
          (str (pct (:ratio open-closed-ratio)) " of all"))
     (kpi (:closed open-closed-ratio) "Closed (all time)")
     (when time-to-close
       (kpi (str (:median-days time-to-close) "d") "Median to close"
            (str "avg " (:avg-days time-to-close) "d")))
     "</div>\n"

     "<h2>Charts</h2>\n<div class=\"grid\">\n"
     (chart-box "chart-month"   (chart-by-month reports-by-month))
     (chart-box "chart-type"    (chart-per-type reports-per-type))
     (when time-to-close
       (chart-box "chart-ttc"   (chart-ttc time-to-close)))
     (chart-box "chart-openers" (chart-openers top-openers))
     "</div>\n"

     "<script>document.addEventListener('DOMContentLoaded',barkRenderAll);</script>\n"
     "</main>\n</body>\n</html>\n")))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn -main [& args]
  (let [opts     (parse-cli-args args)
        html?    (= (:format opts) "html")
        out-file (or (:out-file opts)
                     (if html? "public/stats.html" "public/stats.json"))
        conn     (d/get-conn db-path schema {:wal? false})
        db       (d/db conn)
        reports  (all-reports db)
        stats    (compute-stats reports)]
    (io/make-parents out-file)
    (if html?
      (do (spit out-file (render-html stats))
          (println (str "Wrote " out-file " (HTML, " (count reports) " reports)")))
      (do (spit out-file (json/generate-string stats {:pretty true}))
          (println (str "Wrote " out-file " (JSON, " (count reports) " reports)"))))))

(apply -main *command-line-args*)
