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
         '[hiccup2.core :as h]
         '[taoensso.timbre :as log]
         '[bark.common :refer [report-priority report-status
                               report-descendant-count format-date format-date-iso
                               parse-cli-args load-config bark-schema
                               votes-by-report vote-counts]]
         '[bark.common-bb :refer [load-datalevin-pod! all-reports dq]]
         '[bark.html-bb :refer [pico-cdn resolved-theme set-theme!
                                bark-description bark-repo-url footer-css
                                bark-footer wrap-js spit-html theme-toggle-js
                                nav-bar html-head
                                org-inline parse-org-table]])

(load-datalevin-pod!)

;; ---------------------------------------------------------------------------
;; JS (loaded from resources/ at build time)
;; ---------------------------------------------------------------------------

(def ^:private stats-js (slurp "resources/bark-stats.js"))

(def db-path
  (or (System/getenv "BARK_DB") "./data/bark-db"))

;; ---------------------------------------------------------------------------
;; Time helpers
;; ---------------------------------------------------------------------------

(defn now-ms [] (System/currentTimeMillis))
(def one-year-ms (* 365 24 60 60 1000))

(defn within-last-year? [inst]
  (when inst (> (.getTime inst) (- (now-ms) one-year-ms))))

;; days-between (integer, shared) is in bark-common.clj.
;; This variant returns fractional days for statistical averaging.
(defn- days-between-frac [a b]
  (when (and a b)
    (/ (Math/abs (- (.getTime b) (.getTime a)))
       (* 24.0 60 60 1000))))

(defn round2 [x]
  (when x (/ (Math/round (* x 100.0)) 100.0)))

;; ---------------------------------------------------------------------------
;; Queries (all-reports and report-pull-pattern loaded from bark-common.clj)
;; ---------------------------------------------------------------------------

(defn emails-last-year [db]
  (let [threshold (java.util.Date. (- (System/currentTimeMillis) one-year-ms))]
    (-> (d/q '[:find (count ?e)
               :in $ ?threshold
               :where
               [?e :email/message-id _]
               [?e :email/date-sent ?date]
               [(>= ?date ?threshold)]]
             db threshold)
        ffirst (or 0))))

(defn all-participants
  "Fetch all participant entities from the database."
  [db]
  (d/q '[:find ?email ?source ?since
         :where
         [?e :participant/email ?email]
         [?e :participant/source ?source]
         [?e :participant/since ?since]]
       db))

(defn all-contributors
  "Fetch participants who have submitted code (contributor-since is set)."
  [db]
  (d/q '[:find ?email ?source ?since
         :where
         [?e :participant/email ?email]
         [?e :participant/source ?source]
         [?e :participant/contributor-since ?since]]
       db))

(defn- all-active-tenures
  "Return all currently-active tenures (no :to) across all sources
  as maps with :email and :from."
  [db]
  (let [eids (d/q '[:find [?e ...]
                    :where [?e :maint-tenure/email _]]
                  db)]
    (->> eids
         (map (fn [eid]
                (d/pull db '[:maint-tenure/email
                             :maint-tenure/from
                             :maint-tenure/to] eid)))
         (remove :maint-tenure/to)
         (mapv (fn [m] {:email (:maint-tenure/email m)
                        :from  (:maint-tenure/from m)})))))

(defn total-maintainers
  "Count distinct currently-active maintainer addresses across all sources."
  [db]
  (->> (all-active-tenures db)
       (keep :email)
       distinct
       count))

(defn all-maintainer-since-dates
  "Return :from dates of currently-active tenures as ISO strings
  (\"yyyy-MM-dd\"). Tenures with :from = nil are excluded — they are
  counted via `maintainers-without-since` and fed as `n-always` to the
  cumulative chart."
  [db]
  (let [fmt (doto (java.text.SimpleDateFormat. "yyyy-MM-dd")
              (.setTimeZone (java.util.TimeZone/getTimeZone "UTC")))]
    (->> (all-active-tenures db)
         (keep :from)
         (mapv #(.format fmt ^java.util.Date %)))))

(defn- maintainers-without-since
  "Count currently-active tenures with no :from (seeded 'forever')."
  [db]
  (->> (all-active-tenures db)
       (filter #(nil? (:from %)))
       count))

;; ---------------------------------------------------------------------------
;; Stat helpers
;; ---------------------------------------------------------------------------

(defn report-date   [r] (get-in r [:report/email :email/date-sent]))
(defn report-author [r] (get-in r [:report/email :email/from-address]))

(defn open->close-days [r]
  (days-between-frac (report-date r)
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

(defn- make-ym-formatter
  "Create a locale-independent yyyy-MM formatter (UTC).
  SimpleDateFormat is not thread-safe, so create a fresh instance each time."
  ^java.text.SimpleDateFormat []
  (doto (java.text.SimpleDateFormat. "yyyy-MM" java.util.Locale/ENGLISH)
    (.setTimeZone (java.util.TimeZone/getTimeZone "UTC"))))

(defn- date->ym
  "Extract 'yyyy-MM' from a java.util.Date using a locale-independent formatter.
  Returns nil on failure."
  [date]
  (when date
    (try (.format (make-ym-formatter) date)
         (catch Exception _ nil))))

(defn- current-ym
  "Return [year month] for right now, locale-independent."
  []
  (let [s (.format (make-ym-formatter) (java.util.Date.))
        [year month] (str/split s #"-")]
    [(parse-long year) (parse-long month)]))

(defn- last-12-months
  "Return a vector of 12 \"yyyy-MM\" strings ending with the current month."
  []
  (let [[cy cm] (current-ym)]
    (vec (for [i (range 11 -1 -1)]
           (let [total (+ (* cy 12) (dec cm) (- i))]
             (format "%04d-%02d" (quot total 12) (inc (mod total 12))))))))

(defn- cumulative-by-month
  "Given a {\"yyyy-MM\" count} frequency map and a base count (entries before
  the 12-month window), return [[month cumulative] ...] over the last 12 months."
  [by-ym base-count]
  (let [months      (last-12-months)
        first-month (first months)
        base        (+ base-count
                       (->> by-ym
                            (filter (fn [[ym _]] (neg? (compare ym first-month))))
                            (map val)
                            (reduce + 0)))
        cumulative  (rest (reductions + base (map #(get by-ym % 0) months)))]
    (mapv vector months cumulative)))

(defn reports-by-month [reports]
  (let [counts (->> reports
                    (keep (fn [r] (date->ym (report-date r))))
                    frequencies)
        months (last-12-months)]
    (mapv (fn [m] [m (get counts m 0)]) months)))

(defn contributors-by-month
  "Cumulative contributor count per month over the last 12 months.
  `contributors` is the result of `all-contributors` (tuples of [email source since])."
  [contributors]
  (let [by-ym (->> contributors
                   (keep (fn [[_ _ since]] (date->ym since)))
                   frequencies)]
    (cumulative-by-month by-ym 0)))

(defn participants-by-month
  "Cumulative participant count per month over the last 12 months.
  `participants` is the result of `all-participants` (tuples of [email source since])."
  [participants]
  (let [by-ym (->> participants
                   (keep (fn [[_ _ since]] (date->ym since)))
                   frequencies)]
    (cumulative-by-month by-ym 0)))

(defn maintainers-by-month
  "Cumulative maintainer count per month over the last 12 months.
  `since-dates` is a seq of \"yyyy-MM-dd\" strings from :roles/maintainer-since.
  `n-always` is the count of current maintainers with no since-date
  (config-seeded without :since or directive-added) — they are counted
  as present in every month."
  [since-dates n-always]
  (let [by-ym (->> since-dates
                   (keep (fn [d] (when (>= (count d) 7) (subs d 0 7))))
                   frequencies)]
    (cumulative-by-month by-ym n-always)))

(defn email-vs-reports-ratio [reports emails-last-year-count]
  (let [n (count (filter #(within-last-year? (report-date %)) reports))]
    {:reports-last-year n
     :emails-last-year  emails-last-year-count
     :ratio             (when (pos? emails-last-year-count)
                          (round2 (/ n (double emails-last-year-count))))}))

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

(defn open-closed-ratio [reports]
  (let [open     (count (remove :report/closed reports))
        closed   (count (filter :report/closed reports))]
    {:open open :closed closed
     :ratio (when (pos? (+ open closed))
              (round2 (/ open (double (+ open closed)))))}))

(defn closed-cancel-breakdown
  "Among closed reports, count canceled, expired, and resolved per type."
  [reports]
  (let [closed (filter :report/closed reports)]
    (->> closed
         (group-by #(some-> (:report/type %) name))
         (into {}
               (map (fn [[t rs]]
                      (let [canceled (count (filter #(= :canceled (:report/close-reason %)) rs))
                            expired  (count (filter #(= :expired (:report/close-reason %)) rs))
                            resolved (- (count rs) canceled expired)]
                        [t {:canceled canceled :expired expired :resolved resolved}])))))))

(defn vote-leaders [reports all-votes n]
  (->> reports
       (keep (fn [r]
               (when-let [votes (seq (get all-votes (:db/id r)))]
                 (let [{:keys [up down null]} (vote-counts votes)]
                   (when (pos? (+ up down null))
                     {:message-id (:report/message-id r)
                      :topic      (get-in r [:report/email :email/subject])
                      :votes-up   up
                      :votes-down down
                      :votes-null null
                      :score      (- up down)})))))
       (sort-by :score >) (take n)))

(defn compute-stats
  ([reports] (compute-stats reports nil nil))
  ([reports db] (compute-stats reports db nil))
  ([reports db source-name]
   (let [last-year     (filter #(within-last-year? (report-date %)) reports)
         open-yr       (remove :report/closed last-year)
         emails-yr     (when db (emails-last-year db))
         contributors  (when db (cond->> (all-contributors db)
                                  source-name (filter #(= source-name (second %)))))
         participants  (when db (cond->> (all-participants db)
                                  source-name (filter #(= source-name (second %)))))
         n-maintainers (when db (total-maintainers db))
         maint-since   (when db (all-maintainer-since-dates db))
         n-always      (when db (maintainers-without-since db))
         all-votes     (if db
                         (votes-by-report
                          (d/q '[:find ?r ?val ?voter ?emid
                                 :where
                                 [?v :vote/report ?r]
                                 [?v :vote/value  ?val]
                                 [?v :vote/voter  ?voter]
                                 [?v :vote/email  ?e]
                                 [?e :email/message-id ?emid]]
                               db))
                         {})]
     (cond->
       {:generated-at      (str (java.util.Date.))
        :reports-per-type  (reports-per-type reports)
        :reports-by-month  (reports-by-month reports)
        :time-to-close     (time-to-close-stats reports)
        :open-closed-ratio (open-closed-ratio reports)
        :open-last-year    (count open-yr)
        :total-last-year   (count last-year)
        :top-openers       (top-openers reports 10)
        :vote-leaders      (vote-leaders reports all-votes 10)
        :closed-cancel     (closed-cancel-breakdown reports)}
       emails-yr     (assoc :email-ratio (email-vs-reports-ratio reports emails-yr))
       contributors  (assoc :contributors-by-month (contributors-by-month contributors)
                            :total-contributors (count contributors))
       participants  (assoc :participants-by-month (participants-by-month participants)
                            :total-participants (count participants))
       n-maintainers (assoc :total-maintainers n-maintainers)
       maint-since   (assoc :maintainers-by-month
                           (maintainers-by-month maint-since (or n-always 0)))))))

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

(def stats-css (slurp "resources/bark-data.css"))

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

(defn chart-cancel-breakdown [cancel-data]
  (let [data (mapcat (fn [[t {:keys [canceled expired resolved]}]]
                       [{"type" t "reason" "Canceled" "count" canceled}
                        {"type" t "reason" "Expired"  "count" (or expired 0)}
                        {"type" t "reason" "Resolved" "count" resolved}])
                     cancel-data)]
    (vl "Closed reports: canceled, expired & resolved" "bar" (vec data)
        {:x       {:field "type"   :type "nominal"      :title "Type"}
         :y       {:field "count"  :type "quantitative"  :title "Reports"}
         :color   {:field "reason" :type "nominal"       :title "Close reason"}
         :xOffset {:field "reason"}})))

(defn chart-people
  "Participants, contributors & maintainers line chart with separate lines and legend."
  [participants-by-month contributors-by-month maintainers-by-month]
  (let [partic-data  (mapv (fn [[m c]] {"month" m "count" c "role" "Participants"})
                           participants-by-month)
        contrib-data (mapv (fn [[m c]] {"month" m "count" c "role" "Contributors"})
                           contributors-by-month)
        maint-data   (mapv (fn [[m c]] {"month" m "count" c "role" "Maintainers"})
                           maintainers-by-month)
        data         (into (into partic-data contrib-data) maint-data)]
    {:$schema  "https://vega.github.io/schema/vega-lite/v5.json"
     :title    "Participants, contributors & maintainers (last 12 months)"
     :width    "container"
     :data     {:values data}
     :mark     {:type "line" :point true :tooltip true}
     :encoding {:x     {:field "month" :type "ordinal" :sort "ascending"
                        :axis {:labelAngle -45}}
                :y     {:field "count" :type "quantitative"
                        :title "People"}
                :color {:field "role" :type "nominal" :title "Role"
                        :scale {:range ["#72b362" "#4c78a8" "#e45756"]}}}}))

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
;; data.org rendering (reuses org table parser from bark-docs logic)
;; ---------------------------------------------------------------------------

;; parse-org-table is provided by bark-html.clj (shared with bark-docs).

(defn- strip-dead-data-links
  "Remove individual org links [[target][label]] whose file does not exist.
  Cleans up leftover separators (', ')."
  [line out-dir]
  (let [replaced (str/replace
                  line
                  #"\[\[([^\]]+)\]\[([^\]]+)\]\]"
                  (fn [[whole target _label]]
                    (if (.exists (io/file out-dir target))
                      whole
                      "")))
        ;; Clean up separators left by removed links
        cleaned (-> replaced
                    (str/replace #",\s*,+" ",")
                    (str/replace #"\|\s*," "| ")
                    (str/replace #",\s*\|" " |")
                    ;; Clean cells that only have commas/spaces left
                    (str/replace #"\|\s*,\s*\|" "| |")
                    (str/replace #"\|\s+,\s*$" "| "))]
    cleaned))

(defn render-data-section
  "Render resources/data.org as an HTML section, filtering dead links."
  [out-dir]
  (let [org-text (slurp "resources/data.org")
        lines    (str/split-lines org-text)
        tlines   (filterv #(str/starts-with? (str/trim %) "|") lines)
        filtered (if out-dir
                   (let [processed (mapv (fn [line]
                                           {:orig line
                                            :had-links (boolean (re-find #"\[\[" line))
                                            :result (if (re-find #"\[\[" line)
                                                      (strip-dead-data-links line out-dir)
                                                      line)})
                                         tlines)]
                     (->> processed
                          (remove (fn [{:keys [had-links result]}]
                                    (and had-links (not (re-find #"\[\[" result)))))
                          (mapv :result)))
                   tlines)]
    (when (seq filtered)
      (str "<h3>Available data</h3>\n"
           (parse-org-table filtered)))))

;; ---------------------------------------------------------------------------
;; HTML assembly
;; ---------------------------------------------------------------------------

(defn render-html [stats out-dir]
  (let [{:keys [generated-at reports-per-type reports-by-month
                time-to-close open-closed-ratio open-last-year
                top-openers email-ratio closed-cancel
                participants-by-month contributors-by-month maintainers-by-month
                total-participants total-contributors total-maintainers]} stats
        nav-html (str (h/html (nav-bar "BARK — Data" "data")))
        data-section (render-data-section out-dir)
        vega-scripts (str "<script src=\"https://cdn.jsdelivr.net/npm/vega@5/build/vega.min.js\"></script>\n"
                          "<script src=\"https://cdn.jsdelivr.net/npm/vega-lite@5/build/vega-lite.min.js\"></script>\n"
                          "<script src=\"https://cdn.jsdelivr.net/npm/vega-embed@6/build/vega-embed.min.js\"></script>\n"
                          "<script>\n" (wrap-js theme-toggle-js) "\n</script>\n"
                          "<script>\n" (wrap-js stats-js) "\n</script>\n")
        n-yr (reduce + (vals reports-per-type))
        pct  #(when % (str (Math/round (* 100.0 %)) "%"))]
    (str
     "<!DOCTYPE html>\n"
     "<html lang=\"en\" data-theme=\"light\">\n"
     (html-head {:title      "BARK — Data"
                 :css        stats-css
                 :extra-head (str "<noscript><style>.grid{display:none}</style></noscript>\n"
                                  vega-scripts)})
     "<body>\n"
     "<main class=\"container\">\n"
     nav-html "\n"
     "<p class=\"meta\">Generated " generated-at "</p>\n"

     (when data-section (str data-section "\n"))

     "<h3>Statistics</h3>\n"
     "<div id=\"kpi-area\" class=\"kpis\">\n"
     ;; Baked fallback (replaced by stats.json fetch when served over HTTP)
     (kpi n-yr "Reports (last year)"
          (str open-last-year " still open"))
     (kpi (:open open-closed-ratio) "Open (all time)"
          (str (pct (:ratio open-closed-ratio)) " of all"))
     (kpi (:closed open-closed-ratio) "Closed (all time)")
     (when time-to-close
       (kpi (str (:median-days time-to-close) "d") "Median to close"
            (str "avg " (:avg-days time-to-close) "d")))
     (when email-ratio
       (kpi (or (:ratio email-ratio) "—") "Report/email ratio (last year)"
            (str (:reports-last-year email-ratio) " reports / "
                 (:emails-last-year email-ratio) " emails")))
     (when total-participants
       (kpi total-participants "Participants"
            (when total-contributors (str total-contributors " contributors"))))
     (when total-maintainers
       (kpi total-maintainers "Maintainers"))
     "</div>\n"

     "<div class=\"grid\">\n"
     (chart-box "chart-month"   (chart-by-month reports-by-month))
     (chart-box "chart-type"    (chart-per-type reports-per-type))
     (when (or (seq participants-by-month) (seq contributors-by-month))
       (chart-box "chart-people"
                  (chart-people (or participants-by-month [])
                                (or contributors-by-month [])
                                (or maintainers-by-month []))))
     (when time-to-close
       (chart-box "chart-ttc"   (chart-ttc time-to-close)))
     (chart-box "chart-openers" (chart-openers top-openers))
     (when (seq closed-cancel)
       (chart-box "chart-cancel" (chart-cancel-breakdown closed-cancel)))
     "</div>\n"

     "</main>\n"
     (h/html (bark-footer {:ical (.exists (io/file out-dir "events" "announcements.ics"))}))
     "</body>\n</html>\n")))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn- load-stats-json
  "Read a published stats.json and reconstruct the in-memory shape produced
  by `compute-stats`. Cheshire keywordizes every map key, so we restore string
  keys for the two maps whose keys are report-type names (e.g. \"bug\", \"patch\")
  rather than fixed enum tags. Without this restore, Vega data points would
  carry keyword values like :bug instead of \"bug\"."
  [path]
  (-> (json/parse-string (slurp path) true)
      (update :reports-per-type #(some-> % (update-keys name)))
      (update :closed-cancel    #(some-> % (update-keys name)))))

(defn- generate-json! [out-file source-name]
  (let [conn (d/get-conn db-path bark-schema {:wal? false})]
    (try
      (let [db       (d/db conn)
            all-reps (all-reports db)
            reports  (if source-name
                       (filter #(= source-name (get-in % [:report/email :email/source])) all-reps)
                       all-reps)
            stats    (compute-stats reports db source-name)]
        (spit out-file (json/generate-string stats {:pretty true}))
        (log/info "Wrote" out-file "(JSON," (count reports) "reports)"))
      (finally
        (try (d/close conn) (catch Exception _ nil))))))

(defn- generate-html! [out-file out-dir json-file]
  (let [json-path (or json-file (str out-dir "/reports/stats.json"))]
    (when-not (.exists (io/file json-path))
      (throw (ex-info (str "stats.json not found at " json-path
                           " — generate it first with `bb stats`")
                      {:path json-path})))
    (let [stats (load-stats-json json-path)]
      (spit-html out-file (render-html stats out-dir))
      (log/info "Wrote" out-file "(HTML, from" json-path ")"))))

(defn -main [& args]
  (let [opts        (parse-cli-args args)
        _           (when-let [t (:theme opts)] (set-theme! t))
        html?       (= (:format opts) "html")
        source-name (:source-name opts)
        out-file    (or (:out-file opts)
                        (if html? "public/web/data.html" "public/reports/stats.json"))
        out-dir     (or (:out-dir opts)
                        (.getParent (io/file out-file)))]
    (io/make-parents out-file)
    (if html?
      (generate-html! out-file out-dir (:json-file opts))
      (generate-json! out-file source-name))))
(apply -main *command-line-args*)
