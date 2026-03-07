#!/usr/bin/env bb

;; bark-index.clj — Generate the reports index HTML page from BARK data.
;;
;; Reads reports.json (produced by bark-export) and config.edn, then
;; builds a standalone HTML page.  Most logic is in Clojure; JS is
;; limited to client-side filtering, sorting, theme toggle, and URL
;; permalink state.
;;
;; Usage:
;;   bb export html                              → via bb task (preferred)
;;   bb scripts/bark-index.clj                   → writes public/index.html
;;   bb scripts/bark-index.clj -o reports.html   → writes reports.html

(require '[babashka.process :as process]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[hiccup2.core :as h])

(load-file "scripts/bark-common.clj")
(load-file "scripts/bark-html.clj")

;; ---------------------------------------------------------------------------
;; Config
;; ---------------------------------------------------------------------------

(def default-json "public/reports.json")
(def default-output "public/index.html")
(def bark-doc-url "https://codeberg.org/bzg/bark/src/branch/main/docs/howto.org")

(def type-labels {"bug" "bug" "announcement" "ann" "request" "req"
                  "patch" "patch" "release" "rel" "change" "chg"})

;; ---------------------------------------------------------------------------
;; Date normalization
;; ---------------------------------------------------------------------------

(def ^:private month-numbers
  {"Jan" "01" "Feb" "02" "Mar" "03" "Apr" "04"
   "May" "05" "Jun" "06" "Jul" "07" "Aug" "08"
   "Sep" "09" "Oct" "10" "Nov" "11" "Dec" "12"})

(defn- parse-to-iso-date [s]
  (when (seq s)
    (let [s (str/trim s)]
      (or
       (when (and (>= (count s) 10)
                  (re-matches #"\d{4}-\d{2}-\d{2}.*" s))
         (subs s 0 10))
       (when-let [[_ mon day year]
                  (re-find #"^\w+ (\w+) (\d+) .* (\d{4})$" s)]
         (when-let [m (month-numbers mon)]
           (str year "-" m "-" (format "%02d" (parse-long day)))))
       (when-let [[_ mon day]
                  (re-find #"^\w+ (\w+) (\d+) " s)]
         (when-let [m (month-numbers mon)]
           (str (.getYear (java.time.LocalDate/now))
                "-" m "-" (format "%02d" (parse-long day)))))
       ""))))

;; ---------------------------------------------------------------------------
;; Hiccup helpers
;; ---------------------------------------------------------------------------

(defn- status-square [flags]
  (let [f   (or flags "---")
        a?  (= (nth f 0 \-) \A)
        o?  (= (nth f 1 \-) \O)
        c?  (= (nth f 2 \-) \C)
        [icon label] (cond
                       c?          ["🟥" "Closed"]
                       (and a? o?) ["🟩" "Acked, Owned"]
                       a?          ["🟨" "Acked"]
                       o?          ["🟦" "Owned"]
                       :else       ["⬜" "Open"])]
    [:span {:title (str label " (" flags ")") :aria-label label} icon]))

(defn- priority-square [priority]
  (let [p (or priority 0)
        [icon label] (case (int p)
                       3 ["🟥" "Urgent + Important"]
                       2 ["🟧" "Urgent"]
                       1 ["🟨" "Important"]
                       ["⬜" "Normal"])]
    [:span {:title (str label " (" p ")") :aria-label label} icon]))

(defn- subject-el [subject role archived-at]
  (let [inner (case role
                "admin"      [:strong subject]
                "maintainer" [:em subject]
                subject)]
    (if archived-at
      [:a {:href archived-at} inner]
      inner)))

(defn- related-mids [related]
  (when (seq related)
    (str/join "," (keep #(get % "message-id") related))))

(defn- related-link [related]
  (when-let [mids (related-mids related)]
    [:small.secondary
     " "
     [:a {:href "#"
          :onclick (str "setSearch('m:" mids "'); return false;")
          :title "Filter related reports"}
      (str "↳ " (count related) " related")]]))

(defn- patch-link [patches]
  (when (seq patches)
    (let [n    (count patches)
          href (if (= 1 n)
                 (get (first patches) "file")
                 (let [f (get (first patches) "file")]
                   ;; directory: strip filename from first patch path
                   (str/replace f #"/[^/]+$" "/")))
          label (if (= 1 n) "1 patch file" (str n " patch files"))]
      [:small.secondary
       " "
       [:a {:href href :title label :aria-label label} "📎"]])))

(defn- report-row [{:strs [type subject from from-name date date-raw flags status priority
                           replies archived-at message-id related role source
                           acked owned closed patches]}]
  (let [label    (get type-labels type type)
        closed?  (and flags (>= (count flags) 3) (= (nth flags 2 \-) \C))
        iso-date (or (parse-to-iso-date (or date-raw date "")) "")
        author   (or (when (seq from-name) from-name) from)]
    [:tr {:data-type     type
          :data-closed   (str closed?)
          :data-mid      (or message-id "")
          :data-from     (str/lower-case (or from ""))
          :data-subject  (str/lower-case (or subject ""))
          :data-date     iso-date
          :data-source   (or source "")
          :data-acked    (str/lower-case (or acked ""))
          :data-owned    (str/lower-case (or owned ""))
          :data-closedby (str/lower-case (or closed ""))
          :data-priority (str (or priority 0))
          :data-search   (str/lower-case (str subject " " from " " author " " iso-date))}
     [:td [:mark {:data-type type} label]]
     [:td {:data-value (str (or status 0))} (status-square flags)]
     [:td (priority-square priority)]
     [:td (subject-el subject role archived-at) (related-link related) (patch-link patches)]
     [:td.secondary {:title from} author]
     [:td {:data-value iso-date} [:small (or iso-date date "")]]
     [:td {:style "text-align:center"} (or replies 0)]]))

;; ---------------------------------------------------------------------------
;; CSS (inlined)
;; ---------------------------------------------------------------------------

(def page-css "
  main.container { max-width: 1600px; }
  mark[data-type=bug]          { --pico-mark-background-color: #c0392b22; --pico-mark-color: #c0392b; }
  mark[data-type=announcement] { --pico-mark-background-color: #1a7a8a22; --pico-mark-color: #1a7a8a; }
  mark[data-type=request]      { --pico-mark-background-color: #b8860b22; --pico-mark-color: #b8860b; }
  mark[data-type=patch]        { --pico-mark-background-color: #27ae6022; --pico-mark-color: #27ae60; }
  mark[data-type=release]      { --pico-mark-background-color: #8e44ad22; --pico-mark-color: #8e44ad; }
  mark[data-type=change]       { --pico-mark-background-color: #2c3e5022; --pico-mark-color: #2c3e50; }
  mark { font-size: 0.75rem; font-weight: 600; text-transform: uppercase;
         letter-spacing: 0.05em; padding: 0.15rem 0.4rem; border-radius: 2px; }
  .toolbar { display: flex; gap: 0.75rem; flex-wrap: wrap; align-items: center; margin-bottom: 1rem; }
  .filters { display: flex; gap: 0.4rem; flex-wrap: wrap; }
  .filters button { padding: 0.3rem 0.7rem; font-size: 0.8rem; }
  .filters button.outline { opacity: 0.5; }
  .filters.status-filters button.acked-btn         { background: #b8860b; border-color: #b8860b; color: #fff; }
  .filters.status-filters button.acked-btn.outline  { background: none; color: #b8860b; opacity: 0.5; }
  .filters.status-filters button.owned-btn         { background: #1a5a8a; border-color: #1a5a8a; color: #fff; }
  .filters.status-filters button.owned-btn.outline  { background: none; color: #1a5a8a; opacity: 0.5; }
  input[type=search] { max-width: 25vw; min-width: 200px; margin-bottom: 0; }
  th[data-sort] { cursor: pointer; user-select: none; white-space: nowrap; }
  th[data-sort]:hover { text-decoration: underline; }
  th[data-sort]::after { content: ' ↕'; opacity: 0.3; font-size: 0.75em; }
  th[data-sort].asc::after  { content: ' ↑'; opacity: 0.7; }
  th[data-sort].desc::after { content: ' ↓'; opacity: 0.7; }
  tr.hidden { display: none; }
  #status { font-size: 0.8rem; margin-bottom: 0.5rem; }
  .theme-toggle { cursor: pointer; background: none; border: none; font-size: 1.2rem; padding: 0.3rem; }
  .controls { display: flex; gap: 0.5rem; align-items: center; }
  .controls label { font-size: 0.85rem; margin-bottom: 0; cursor: pointer; }
  .controls input[type=checkbox] { margin: 0; }
")

;; ---------------------------------------------------------------------------
;; JS — client-side filtering, sorting, URL state, theme toggle.
;; ---------------------------------------------------------------------------

(def ^:private index-js (slurp "resources/bark-index.js"))

(defn page-js [types-json all-open?]
  (str "var barkConfig = {types:" types-json
       ",allOpen:" (if all-open? "true" "false") "};\n"
       index-js "\n"
       theme-toggle-js))

;; ---------------------------------------------------------------------------
;; Page assembly
;; ---------------------------------------------------------------------------

(defn page [reports min-status out-dir]
  (let [types      (vec (distinct (map #(get % "type") reports)))
        types-json (json/generate-string types)
        all-open?  (and min-status (>= min-status 4))
        has-rss?   (.exists (clojure.java.io/file out-dir "reports.rss"))
        has-org?   (.exists (clojure.java.io/file out-dir "reports.org"))
        has-json?  (.exists (clojure.java.io/file out-dir "reports.json"))
        generated-at (str (java.util.Date.))
        rss-href   "reports.rss"
        org-href   "reports.org"
        json-href  "reports.json"
        cols       [[:th {:data-sort "type"     :onclick "sortTable(0,'type')"}     "Type"]
                    [:th {:data-sort "status"   :onclick "sortTable(1,'status')"}   "Status"]
                    [:th {:data-sort "priority" :onclick "sortTable(2,'priority')"} "Priority"]
                    [:th {:data-sort "subject"  :onclick "sortTable(3,'subject')"}  "Subject"]
                    [:th {:data-sort "from"     :onclick "sortTable(4,'from')"}     "Author"]
                    [:th {:data-sort "date"     :onclick "sortTable(5,'date')"}     "Date"]
                    [:th {:data-sort "replies"  :onclick "sortTable(6,'replies')"}  "↩"]]]
    (str
     "<!DOCTYPE html>\n"
     (h/html
      [:html {:lang "en" :data-theme "light"}
       [:head
        [:meta {:charset "UTF-8"}]
        [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
        [:meta {:name "color-scheme" :content "light dark"}]
        [:link {:rel "stylesheet" :href pico-cdn}]
        (when has-rss?
          [:link {:rel "alternate" :type "application/rss+xml"
                  :title "BARK Reports RSS" :href rss-href}])
        [:title "BARK — Reports"]
        [:style (h/raw page-css)]]
       [:body
        [:main.container
         [:nav
          [:ul [:li [:strong "BARK — Reports"]]]
          [:ul
           (when has-json?
             [:li [:a {:href json-href :title "JSON data"} "JSON"]])
           (when has-rss?
             [:li [:a {:href rss-href :title "RSS feed"} "RSS"]])
           (when has-org?
             [:li [:a {:href org-href :title "Org file"} "Org"]])
           [:li [:a {:href bark-doc-url :title "BARK documentation"} "Docs"]]
           [:li [:a {:href "howto.html" :title "How-to"} "How-to"]]
           [:li [:a {:href "stats.html" :title "Statistics"} "Stats"]]
           [:li (theme-toggle-btn)]]]
         [:p {:style "font-size:0.78rem;color:var(--pico-muted-color);margin-bottom:1rem"}
          (str "Generated " generated-at)]
         [:div.toolbar
          [:input#si {:type        "search"
                      :placeholder "Search"
                      :oninput     "filterRows()"}]
          [:div.filters
           (for [t types]
             [:button {:data-type t
                       :onclick (str "toggleType('" t "',this)")}
              (get type-labels t t)])]
          [:div.filters.status-filters
           [:button#btn-acked.acked-btn.outline
            {:onclick "toggleAcked(this)" :title "Toggle visibility of acked reports"}
            "Acked"]
           [:button#btn-owned.owned-btn.outline
            {:onclick "toggleOwned(this)" :title "Toggle visibility of owned reports"}
            "Owned"]]
          (when-not all-open?
            [:div.controls
             [:label
              [:input#show-closed {:type "checkbox" :onchange "toggleClosed()"}]
              " Only closed"]])]
         [:div#status]
         [:figure {:style "overflow-x:auto"}
          [:table.striped
           [:thead [:tr (seq cols)]]
           [:tbody
            (for [r reports]
              (report-row r))]]]
         [:script (h/raw (page-js types-json all-open?))]]]]))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [{:keys [out-file json-file min-status]}
      (parse-cli-args *command-line-args*)
      json-file (or json-file default-json)
      out-file  (or out-file default-output)
      out-dir   (or (.getParent (clojure.java.io/file out-file)) "public")]
  (.mkdirs (clojure.java.io/file out-dir))
  (let [envelope (json/parse-string (slurp json-file))
        reports  (get envelope "reports" envelope)
        html     (page reports min-status out-dir)]
    (spit out-file html)
    (binding [*out* *err*]
      (println (str "Wrote " (count reports) " reports to " out-file)))))
