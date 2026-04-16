#!/usr/bin/env bb

;; bark-index.clj — Generate the reports index HTML page from BARK data.
;;
;; Reads reports.json (produced by bark-export) and config.edn, then
;; builds a standalone HTML page.  Most logic is in Clojure; JS is
;; limited to client-side filtering, sorting, theme toggle, and URL
;; permalink state.
;;
;; Usage:
;;   bb export html                              -> via bb task (preferred)
;;   bb scripts/bark-index.clj                   -> writes public/index.html
;;   bb scripts/bark-index.clj -o reports.html   -> writes reports.html

(require '[babashka.process :as process]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[hiccup2.core :as h]
         '[taoensso.timbre :as log]
         '[bark.common :refer [parse-cli-args load-config]]
         '[bark.html-bb :refer [pico-cdn resolved-theme set-theme!
                                bark-description bark-repo-url
                                footer-css bark-footer wrap-js spit-html
                                noscript-css noscript-banner
                                theme-toggle-js nav-bar]])

;; ---------------------------------------------------------------------------
;; Config
;; ---------------------------------------------------------------------------

(def default-json "public/reports/all-open.json")
(def default-output "public/web/index.html")

(def type-labels {"bug" "bug" "announcement" "ann" "request" "req"
                  "patch" "patch" "release" "rel" "change" "chg"})

;; ---------------------------------------------------------------------------
;; CSS (inlined)
;; ---------------------------------------------------------------------------

(def page-css (str noscript-css "
  main.container { max-width: 1800px; padding-left: max(1rem, env(safe-area-inset-left)); padding-right: max(1rem, env(safe-area-inset-right)); }
  mark[data-type=bug]          { --pico-mark-background-color: var(--bark-mark-bug-bg, #c0392b1a); --pico-mark-color: var(--bark-mark-bug, #c0392b); }
  mark[data-type=announcement] { --pico-mark-background-color: var(--bark-mark-ann-bg, #1a7a8a1a); --pico-mark-color: var(--bark-mark-ann, #1a7a8a); }
  mark[data-type=request]      { --pico-mark-background-color: var(--bark-mark-req-bg, #b8860b1a); --pico-mark-color: var(--bark-mark-req, #b8860b); }
  mark[data-type=patch]        { --pico-mark-background-color: var(--bark-mark-patch-bg, #27ae601a); --pico-mark-color: var(--bark-mark-patch, #27ae60); }
  mark[data-type=release]      { --pico-mark-background-color: var(--bark-mark-rel-bg, #8e44ad1a); --pico-mark-color: var(--bark-mark-rel, #8e44ad); }
  mark[data-type=change]       { --pico-mark-background-color: var(--bark-mark-chg-bg, #2c3e501a); --pico-mark-color: var(--bark-mark-chg, #2c3e50); }
  mark { font-size: 0.75rem; font-weight: 600; text-transform: uppercase;
         letter-spacing: 0.05em; padding: 0.15rem 0.4rem; border-radius: 2px; }
  .toolbar { display: flex; gap: 0.75rem; flex-wrap: wrap; align-items: center; margin-bottom: 1rem; }
  .filters { display: flex; gap: 0.4rem; flex-wrap: wrap; }
  .filters button { padding: 0.3rem 0.7rem; font-size: 0.8rem; }
  .filters button.outline { opacity: 0.5; }
  .filters.status-filters button.open-btn          { background: var(--bark-btn-open, var(--pico-secondary-background)); border-color: var(--bark-btn-open, var(--pico-secondary-border)); color: var(--bark-btn-open-text, var(--pico-secondary-inverse)); }
  .filters.status-filters button.open-btn.outline   { background: none; color: var(--bark-btn-open, var(--pico-secondary-background)); opacity: 0.5; }
  .filters.status-filters button.acked-btn         { background: var(--bark-btn-acked, var(--pico-primary-background)); border-color: var(--bark-btn-acked, var(--pico-primary-border)); color: var(--bark-btn-acked-text, var(--pico-primary-inverse)); }
  .filters.status-filters button.acked-btn.outline  { background: none; color: var(--bark-btn-acked, var(--pico-primary-background)); opacity: 0.5; }
  .filters.status-filters button.owned-btn         { background: var(--bark-btn-owned, var(--pico-contrast-background)); border-color: var(--bark-btn-owned, var(--pico-contrast-border)); color: var(--bark-btn-owned-text, var(--pico-contrast-inverse)); }
  .filters.status-filters button.owned-btn.outline  { background: none; color: var(--bark-btn-owned, var(--pico-contrast-background)); opacity: 0.5; }
  .filters.status-filters button.awaiting-btn         { background: var(--bark-btn-awaiting, #b8860b); border-color: var(--bark-btn-awaiting, #b8860b); color: var(--bark-btn-awaiting-text, #fff); }
  .filters.status-filters button.awaiting-btn.outline  { background: none; color: var(--bark-btn-awaiting, #b8860b); opacity: 0.5; }
  input[type=search] { max-width: 25vw; min-width: 200px; margin-bottom: 0; }
  th[data-sort] { cursor: pointer; user-select: none; white-space: nowrap; }
  th[data-sort]:hover { text-decoration: underline; }
  th[data-sort]::after { content: ' ↕'; opacity: 0.3; font-size: 0.75em; }
  th[data-sort].asc::after  { content: ' ↑'; opacity: 0.7; }
  th[data-sort].desc::after { content: ' ↓'; opacity: 0.7; }
  tr.hidden { display: none; }
  [data-theme=light] { --bark-stripe-bg: #f5f5f5; --bark-row-bg: #fff; }
  [data-theme=dark]  { --bark-stripe-bg: #1a1f2b; --bark-row-bg: #13171f; }
  tr.stripe td       { background-color: var(--bark-stripe-bg); }
  tr:not(.stripe) td { background-color: var(--bark-row-bg); }
  td:nth-child(3) { white-space: nowrap; }
  td:nth-child(5) { min-width: 740px; }
  td:nth-child(6) { max-width: 200px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
  #status { font-size: 0.8rem; margin-bottom: 0.5rem; }
  .vote-badge { display: inline-block; padding: 0.1rem 0.4rem; border-radius: 3px; font-size: 0.7rem;
                font-weight: 600; margin-left: 0.4em; margin-right: 0.4em; vertical-align: middle; }
  .vote-pos { background: var(--bark-vote-pos-bg, #27ae6033); color: var(--bark-vote-pos, #27ae60); }
  .vote-neg { background: var(--bark-vote-neg-bg, #c0392b33); color: var(--bark-vote-neg, #c0392b); }
  .vote-zero { background: var(--bark-vote-zero-bg, #95a5a622); color: var(--bark-vote-zero, #7f8c8d); }
  .theme-toggle { cursor: pointer; background: none; border: none; font-size: 1.2rem; padding: 0.3rem; }
  [data-theme=dark] .bark-logo svg { filter: invert(0.7); }

  /* Responsive: progressively hide columns — only Subject remains */
  @media (max-width: 1200px) {
    td:nth-child(4), th:nth-child(4) { display: none; } /* Flags */
  }
  @media (max-width: 1024px) {
    td:nth-child(3), th:nth-child(3) { display: none; } /* Due */
  }
  @media (max-width: 860px) {
    td:nth-child(2), th:nth-child(2) { display: none; } /* Priority */
    td:nth-child(5) { min-width: auto; }
  }
  @media (max-width: 780px) {
    td:nth-child(9), th:nth-child(9) { display: none; } /* Replies */
  }
  @media (max-width: 740px) {
    td:nth-child(8), th:nth-child(8) { display: none; } /* Date */
  }
  @media (max-width: 680px) {
    td:nth-child(7), th:nth-child(7) { display: none; } /* Owner */
  }
  @media (max-width: 540px) {
    td:nth-child(6), th:nth-child(6) { display: none; } /* Author */
    input[type=search] { max-width: none; min-width: 0; width: 100%; }
    .toolbar { flex-direction: column; align-items: stretch; }
  }
  @media (max-width: 420px) {
    td:nth-child(1), th:nth-child(1) { display: none; } /* Type */
  }
" footer-css))

;; ---------------------------------------------------------------------------
;; JS — client-side filtering, sorting, URL state, theme toggle.
;; ---------------------------------------------------------------------------

(def ^:private index-js (slurp "resources/bark-index.js"))

(defn page-js [types-json total open-count closed-count source-type page-size reports-json]
  (wrap-js (str "var barkConfig = {types:" types-json
                ",total:" total
                ",openCount:" open-count
                ",closedCount:" closed-count
                ",closedJsonUrl:'reports/all-closed.json'"
                (when source-type
                  (str ",sourceType:'" source-type "'"))
                (when page-size
                  (str ",pageSize:" page-size))
                "};\n"
                "var barkData = " reports-json ";\n"
                index-js "\n"
                theme-toggle-js)))

;; ---------------------------------------------------------------------------
;; Page assembly
;; ---------------------------------------------------------------------------

(defn index-page [reports reports-dir envelope page-size]
  (let [source-type  (get envelope "source-type")
        types        (vec (distinct (map #(get % "type") reports)))
        types-json   (json/generate-string types)
        total        (get envelope "total" (count reports))
        open-count   (get envelope "open-count" (count reports))
        closed-count (get envelope "closed-count" 0)
        has-rss?     (.exists (clojure.java.io/file reports-dir "all.xml"))
        ;; ICS lives one level up from reports-dir, in events/
        base-dir     (.getParent (clojure.java.io/file reports-dir))
        has-ical?    (and base-dir (.exists (clojure.java.io/file base-dir "events" "announcements.ics")))
        generated-at (str (java.util.Date.))
        rss-href     "reports/all.xml"
        cols         [[:th {:data-sort "type"     :onclick "sortTable(0,'type')"}     "Type"]
                      [:th {:data-sort "priority" :onclick "sortTable(1,'priority')"} "Prio"]
                      [:th {:data-sort "due"      :onclick "sortTable(2,'due')"}      "Due"]
                      [:th {:data-sort "flags"    :onclick "sortTable(3,'flags')"}    "Flags"]
                      [:th {:data-sort "subject"  :onclick "sortTable(4,'subject')" :title "Sort by last activity"} "Subject"]
                      [:th {:data-sort "from"     :onclick "sortTable(5,'from')"}     "Author"]
                      [:th {:data-sort "owner"    :onclick "sortTable(6,'owner')"}    "Owner"]
                      [:th {:data-sort "date"     :onclick "sortTable(7,'date')"}     "Date"]
                      [:th {:data-sort "replies"  :onclick "sortTable(8,'replies')"}  "↩"]]]
    (str
     "<!DOCTYPE html>\n"
     (h/html
      [:html {:lang "en" :data-theme "light"}
       [:head
        [:meta {:charset "UTF-8"}]
        [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
        [:meta {:name "color-scheme" :content "light dark"}]
        [:meta {:name "description" :content bark-description}]
        [:meta {:property "og:title" :content "BARK — Reports"}]
        [:meta {:property "og:description" :content bark-description}]
        [:meta {:property "og:type" :content "website"}]
        [:link {:rel "stylesheet" :href pico-cdn}]
        (for [{:keys [link inline]} (resolved-theme)]
          (if link
            [:link {:rel "stylesheet" :href link}]
            [:style (h/raw inline)]))
        (when has-rss?
          [:link {:rel "alternate" :type "application/rss+xml"
                  :title "BARK Reports RSS" :href rss-href}])
        [:title "BARK — Reports"]
        [:style (h/raw page-css)]]
       [:body
        [:main.container
         (nav-bar "BARK — Reports" "reports")
         (noscript-banner)
         [:p {:style "font-size:0.78rem;color:var(--pico-muted-color);margin-bottom:1rem"}
          (str "Generated " generated-at)]
         [:div.toolbar
          [:input#si {:type        "search"
                      :placeholder "Press / to search"
                      :oninput     "onSearchInput()"}]
          [:div.filters
           (for [t types]
             [:button {:data-type t
                       :onclick (str "toggleType('" t "',this)")}
              (get type-labels t t)])]
          [:div.filters.status-filters
           [:button#btn-open.open-btn
            {:onclick "toggleOpen(this)" :title "Toggle visibility of open reports only"}
            "Open"]
           [:button#btn-acked.acked-btn.outline
            {:onclick "toggleAcked(this)" :title "Toggle visibility of acked reports"}
            "Acked"]
           [:button#btn-owned.owned-btn.outline
            {:onclick "toggleOwned(this)" :title "Toggle visibility of owned reports"}
            "Owned"]
           [:button#btn-awaiting.awaiting-btn.outline
            {:onclick "toggleAwaiting(this)" :title "Show only reports awaiting reply"
             :style "margin-left:auto"}
            "⌚ Awaiting"]]]
         [:div#status]
         [:figure {:style "overflow-x:auto"}
          [:table
           [:thead [:tr (seq cols)]]
           [:tbody {:id "data"}]]]
         [:div#pagination]
         [:script (h/raw (page-js types-json total open-count closed-count source-type page-size
                                  (json/generate-string reports)))]]
        (bark-footer {:ical has-ical?})]]))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [{:keys [out-file json-file out-dir theme page-size]}
      (parse-cli-args *command-line-args*)
      _           (when theme (set-theme! theme))
      json-file   (or json-file default-json)
      out-file    (or out-file default-output)
      reports-dir (or out-dir
                      (.getParent (clojure.java.io/file json-file))
                      "public/reports")]
  (.mkdirs (clojure.java.io/file (.getParent (clojure.java.io/file out-file))))
  (let [envelope (json/parse-string (slurp json-file))
        reports  (get envelope "reports" envelope)
        html     (index-page reports reports-dir envelope page-size)]
    (spit-html out-file html)
    (binding [*out* *err*]
      (log/info "Wrote" (count reports) "reports to" out-file))))
