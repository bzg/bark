#!/usr/bin/env bb

;; bark-html.clj — Generate a static HTML page from BARK reports.
;;
;; Calls bark-egest to produce reports.json, then embeds the data
;; in a standalone HTML page with Pico CSS 2.0 (light/dark mode),
;; search, type filters, and sortable columns.
;;
;; Usage:
;;   bb scripts/bark-html.clj                   → writes index.html
;;   bb scripts/bark-html.clj -o reports.html   → writes reports.html
;;   bb html                                    → via bb task

(require '[babashka.process :as process]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[hiccup2.core :as h])

;; ---------------------------------------------------------------------------
;; Config
;; ---------------------------------------------------------------------------

(def pico-cdn "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css")
(def json-file "reports.json")
(def default-output "index.html")

(def type-labels {"bug" "bug" "announcement" "ann" "request" "req"
                  "patch" "patch" "release" "rel" "change" "chg"})

;; ---------------------------------------------------------------------------
;; Generate reports.json via bark-egest
;; ---------------------------------------------------------------------------

(defn generate-json! []
  (let [{:keys [exit]} (process/shell "bb" "scripts/bark-egest.clj" "reports" "json")]
    (when-not (zero? exit)
      (binding [*out* *err*]
        (println "Error: bark-egest exited with" exit))
      (System/exit 1))
    (when-not (.exists (clojure.java.io/file json-file))
      (binding [*out* *err*]
        (println (str "Error: " json-file " not found after running bark-egest")))
      (System/exit 1))))

;; ---------------------------------------------------------------------------
;; Hiccup rendering
;; ---------------------------------------------------------------------------

(defn- status-square
  "Colored square for acked/owned/closed status with a11y fallback."
  [flags]
  (let [f   (or flags "---")
        a?  (= (nth f 0) \A)
        o?  (= (nth f 1) \O)
        c?  (= (nth f 2) \C)
        [icon label] (cond
                       c?          ["🟥" "Closed"]
                       (and a? o?) ["🟩" "Acked, Owned"]
                       a?          ["🟨" "Acked"]
                       o?          ["🟦" "Owned"]
                       :else       ["⬜" "Open"])]
    [:span {:title (str label " (" flags ")") :aria-label label} icon]))

(defn- priority-square
  "Colored square for priority with a11y fallback."
  [priority]
  (let [p (or priority 0)
        [icon label] (case p
                       3 ["🟥" "Urgent + Important"]
                       2 ["🟧" "Urgent"]
                       1 ["🟨" "Important"]
                         ["⬜" "Normal"])]
    [:span {:title (str label " (" p ")") :aria-label label} icon]))

(defn report-row [{:strs [type subject from date flags priority replies archived-at]}]
  (let [label (get type-labels type type)]
    [:tr {:data-type type
          :data-search (str/lower-case (str subject " " from " " date))}
     [:td [:mark {:data-type type} label]]
     [:td (status-square flags)]
     [:td (priority-square priority)]
     [:td (if archived-at [:a {:href archived-at} subject] subject)]
     [:td.secondary from]
     [:td [:small date]]
     [:td {:style "text-align:center"} (or replies 0)]]))

;; ---------------------------------------------------------------------------
;; CSS & JS (inlined)
;; ---------------------------------------------------------------------------

(def page-css "
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
  input[type=search] { max-width: 240px; margin-bottom: 0; }
  th[data-sort] { cursor: pointer; user-select: none; }
  th[data-sort]:hover { text-decoration: underline; }
  th[data-sort]::after { content: ' ↕'; opacity: 0.3; font-size: 0.75em; }
  th[data-sort].asc::after  { content: ' ↑'; opacity: 0.7; }
  th[data-sort].desc::after { content: ' ↓'; opacity: 0.7; }
  tr.hidden { display: none; }
  #status { font-size: 0.8rem; margin-bottom: 0.5rem; }
  .theme-toggle { cursor: pointer; background: none; border: none; font-size: 1.2rem; padding: 0.3rem; }
")

(defn page-js [types]
  (let [active-init (str "{"
                         (str/join "," (map #(str % ":true") (map #(str "'" % "'") types)))
                         "}")]
    (str "
  var activeTypes = " active-init ";

  function filterRows() {
    var q = document.querySelector('input[type=search]').value.toLowerCase();
    var rows = document.querySelectorAll('tbody tr');
    var visible = 0;
    rows.forEach(function(tr) {
      var show = activeTypes[tr.dataset.type] && (!q || tr.dataset.search.indexOf(q) !== -1);
      tr.classList.toggle('hidden', !show);
      if (show) visible++;
    });
    document.getElementById('status').textContent = visible + '/' + rows.length + ' reports';
  }

  function toggleType(type, btn) {
    activeTypes[type] = !activeTypes[type];
    btn.classList.toggle('outline');
    filterRows();
  }

  var sortState = {};
  function sortTable(colIdx, key) {
    var tbody = document.querySelector('tbody');
    var rows = Array.from(tbody.querySelectorAll('tr'));
    var dir = sortState[key] === 'asc' ? 'desc' : 'asc';
    sortState = {};
    sortState[key] = dir;
    document.querySelectorAll('th[data-sort]').forEach(function(th) {
      th.classList.remove('asc', 'desc');
    });
    document.querySelector('th[data-sort=\"' + key + '\"]').classList.add(dir);
    rows.sort(function(a, b) {
      var av = a.children[colIdx].textContent.trim().toLowerCase();
      var bv = b.children[colIdx].textContent.trim().toLowerCase();
      var an = parseFloat(av), bn = parseFloat(bv);
      if (!isNaN(an) && !isNaN(bn)) return dir === 'asc' ? an - bn : bn - an;
      if (av < bv) return dir === 'asc' ? -1 : 1;
      if (av > bv) return dir === 'asc' ? 1 : -1;
      return 0;
    });
    rows.forEach(function(r) { tbody.appendChild(r); });
  }

  function toggleTheme() {
    var html = document.documentElement;
    var next = html.getAttribute('data-theme') === 'dark' ? 'light' : 'dark';
    html.setAttribute('data-theme', next);
    document.getElementById('theme-icon').textContent = next === 'dark' ? '☀️' : '🌙';
  }

  filterRows();
")))

;; ---------------------------------------------------------------------------
;; Page assembly
;; ---------------------------------------------------------------------------

(defn page [reports]
  (let [types (vec (distinct (map #(get % "type") reports)))]
    (str
     "<!DOCTYPE html>\n"
     (h/html
      [:html {:lang "en" :data-theme "light"}
       [:head
        [:meta {:charset "UTF-8"}]
        [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
        [:meta {:name "color-scheme" :content "light dark"}]
        [:link {:rel "stylesheet" :href pico-cdn}]
        [:title "BARK — Reports"]
        [:style (h/raw page-css)]]
       [:body
        [:main.container
         [:nav
          [:ul [:li [:strong "BARK — Reports"]]]
          [:ul [:li [:button.theme-toggle
                     {:onclick "toggleTheme()" :aria-label "Toggle theme"}
                     [:span#theme-icon "🌙"]]]]]
         [:div.toolbar
          [:input {:type "search" :placeholder "Search…" :oninput "filterRows()"}]
          [:div.filters
           (for [t types]
             [:button {:onclick (str "toggleType('" t "',this)")}
              (get type-labels t t)])]]
         [:div#status]
         [:figure {:style "overflow-x:auto"}
          [:table.striped
           [:thead
            [:tr
             [:th {:data-sort "type" :onclick "sortTable(0,'type')"} "Type"]
             [:th "Status"]
             [:th {:data-sort "priority" :onclick "sortTable(2,'priority')"} "Priority"]
             [:th {:data-sort "subject" :onclick "sortTable(3,'subject')"} "Subject"]
             [:th {:data-sort "from" :onclick "sortTable(4,'from')"} "From"]
             [:th {:data-sort "date" :onclick "sortTable(5,'date')"} "Date"]
             [:th {:data-sort "replies" :onclick "sortTable(6,'replies')"} "↩"]]]
           [:tbody
            (for [r reports]
              (report-row r))]]]
         [:script (h/raw (page-js types))]]]]))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [args    (vec *command-line-args*)
      out-idx (.indexOf args "-o")
      out-file (if (>= out-idx 0)
                 (nth args (inc out-idx) default-output)
                 default-output)]
  ;; Step 1: generate reports.json via bark-egest
  (binding [*out* *err*]
    (println "Generating" json-file "via bark-egest…"))
  (generate-json!)

  ;; Step 2: read JSON and generate HTML
  (let [reports (json/parse-string (slurp json-file))
        html    (page reports)]
    (spit out-file html)
    (binding [*out* *err*]
      (println (str "Wrote " (count reports) " reports to " out-file)))))
