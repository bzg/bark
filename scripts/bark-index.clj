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
         '[hiccup2.core :as h])

;; Forward-declared for clj-kondo (provided at runtime by load-file calls below).
(declare parse-cli-args load-config
         pico-cdn resolved-theme set-theme! bark-description bark-repo-url
         footer-css bark-footer wrap-js spit-html
         theme-toggle-btn theme-toggle-js nav-bar)

(load-file "scripts/bark-common.clj")
(load-file "scripts/bark-html.clj")

;; ---------------------------------------------------------------------------
;; Config
;; ---------------------------------------------------------------------------

(def default-json "public/reports/all-open.json")
(def default-output "public/web/index.html")

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
       ""))))

;; ---------------------------------------------------------------------------
;; Hiccup helpers
;; ---------------------------------------------------------------------------

(defn- subject-el [{:strs [subject archived-at close-reason superseded-by]} closed? source-type]
  (let [canceled?    (and closed? (= close-reason "canceled"))
        expired?     (and closed? (= close-reason "expired"))
        superseded?  (and closed? (= close-reason "superseded"))
        inner     (cond (or canceled? superseded?) [:em [:s subject]]
                        closed?   [:em subject]
                        :else     subject)
        linkable? (and archived-at
                       (not (#{"alias" "mailbox"} source-type)))]
    (if linkable?
      [:a (cond-> {:href archived-at}
             canceled?    (assoc :title "Canceled")
             expired?     (assoc :title "Expired")
             superseded?  (assoc :title (str "Superseded by: "
                                             (get superseded-by "subject" "another report")))
             (and closed? (not canceled?) (not expired?) (not superseded?))
             (assoc :title "Resolved"))
       inner]
      inner)))

(defn- related-mids [related]
  (when (seq related)
    (str/join "," (keep #(get % "message-id") related))))

(defn- related-link [related]
  (when-let [mids (related-mids related)]
    [:a.secondary {:href "#"
                   :onclick (str "showRelated('m:" mids "'); return false;")
                   :title "Filter related reports"
                   :style "font-size:0.75rem"}
     (str "↳" (count related) " ")]))

(defn- patch-link [patches]
  (when (seq patches)
    (let [n    (count patches)
          href (if (= 1 n)
                 (str "patches/" (get (first patches) "file"))
                 (let [f (get (first patches) "file")]
                   ;; directory: strip filename from first patch path
                   (str "patches/" (str/replace f #"/[^/]+$" "/"))))
          label (if (= 1 n) "1 patch file" (str n " patch files"))]
      [:a {:href href :title label :aria-label label
           :style "font-size:0.75rem"} "📎 "])))

(defn- vote-badge
  "Render a small vote badge with score/total and colored background."
  [votes]
  (when votes
    (let [[score-s] (str/split votes #"/")
          score (parse-long (or score-s "0"))
          cls   (cond (pos? score) "vote-pos"
                      (neg? score) "vote-neg"
                      :else        "vote-zero")]
      [:small {:class (str "vote-badge " cls)
               :title (str "Vote: " votes)}
       votes])))

(defn- report-row [{:strs [type subject from from-name date date-raw flags priority
                           replies _archived-at message-id related role source
                           acked owned closed urgent important patches votes
                           deadline topic close-reason expired-date expiry
                           _superseded-by]
                    :as report}
                   source-type]
  (let [label    (get type-labels type type)
        closed?  (and flags (>= (count flags) 3) (= (nth flags 2 \-) \C))
        iso-date (or (parse-to-iso-date (or date-raw date "")) "")
        author   (or (when (seq from-name) from-name) from)
        flag-a   (if (seq acked) "A" "-")
        flag-o   (if (seq owned) "O" "-")
        flag-c   (cond (= close-reason "canceled")   "C"
                       (= close-reason "expired")    "E"
                       (= close-reason "superseded") "S"
                       closed?                       "R"
                       :else                         "-")
        flags-str (str flag-a flag-o flag-c)
        ;; Numeric score for sorting: acked=1, owned=2, open=4 (closed=0)
        flags-score (+ (if (seq acked) 1 0)
                       (if (seq owned) 2 0)
                       (if closed? 0 4))
        flags-title (str/join ", " (cond-> []
                                     (= flag-a "A") (conj "Acked")
                                     (= flag-o "O") (conj "Owned")
                                     (= flag-c "C") (conj "Canceled")
                                     (= flag-c "E") (conj "Expired")
                                     (= flag-c "S") (conj "Superseded")
                                     (= flag-c "R") (conj "Resolved")))]
    [:tr {:data-type        type
          :data-closed      (str closed?)
          :data-mid         (or message-id "")
          :data-from        (str/lower-case (or from ""))
          :data-subject     (str/lower-case (or subject ""))
          :data-date        iso-date
          :data-source      (or source "")
          :data-acked       (str/lower-case (or acked ""))
          :data-owned       (str/lower-case (or owned ""))
          :data-closedby    (str/lower-case (or closed ""))
          :data-urgent      (str/lower-case (or urgent ""))
          :data-important   (str/lower-case (or important ""))
          :data-priority    (str (or priority 0))
          :data-deadline    (or deadline "")
          :data-expired     (or expired-date "")
          :data-topic       (str/lower-case (or topic ""))
          :data-search      (str/lower-case (str subject " " from " " author " " iso-date " " topic))}
     [:td [:mark {:data-type type :style "cursor:pointer"
                  :onclick (str "isolateType('" type "')")}
           label]]
     [:td {:data-value (str (or priority 0)) :style "text-align:center"}
      (case (str (or priority 0)) "3" "A" "2" "B" "1" "C" " ")]
     [:td {:data-value (or deadline "") :class "due-cell"} ""]
     [:td {:data-value (str flags-score) :title flags-title
           :style "text-align:center; font-family:monospace; font-size:0.8rem; letter-spacing:0.1em"} flags-str]
     [:td (patch-link patches) (related-link related)
      (vote-badge votes)
      (subject-el report closed? source-type)]
     [:td.secondary {:title from} (if (#{"maintainer" "admin"} role) [:strong author] author)]
     [:td {:data-value iso-date}
      (if expiry
        [:small {:title (str "Expires on " expiry)} [:em (or iso-date date "")]]
        [:small (or iso-date date "")])]
     [:td {:style "text-align:center"} (or replies 0)]]))

;; ---------------------------------------------------------------------------
;; CSS (inlined)
;; ---------------------------------------------------------------------------

(def page-css (str "
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
  .filters.status-filters button.open-btn          { background: var(--bark-btn-open, #27ae60); border-color: var(--bark-btn-open, #27ae60); color: var(--bark-btn-open-text, #fff); }
  .filters.status-filters button.open-btn.outline   { background: none; color: var(--bark-btn-open, #27ae60); opacity: 0.5; }
  .filters.status-filters button.acked-btn         { background: var(--bark-btn-acked, #b8860b); border-color: var(--bark-btn-acked, #b8860b); color: var(--bark-btn-acked-text, #fff); }
  .filters.status-filters button.acked-btn.outline  { background: none; color: var(--bark-btn-acked, #b8860b); opacity: 0.5; }
  .filters.status-filters button.owned-btn         { background: var(--bark-btn-owned, #1a5a8a); border-color: var(--bark-btn-owned, #1a5a8a); color: var(--bark-btn-owned-text, #fff); }
  .filters.status-filters button.owned-btn.outline  { background: none; color: var(--bark-btn-owned, #1a5a8a); opacity: 0.5; }
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
  @media (max-width: 740px) {
    td:nth-child(8), th:nth-child(8) { display: none; } /* Replies */
  }
  @media (max-width: 680px) {
    td:nth-child(7), th:nth-child(7) { display: none; } /* Date */
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

(defn page-js [types-json total open-count closed-count source-type]
  (wrap-js (str "var barkConfig = {types:" types-json
                ",total:" total
                ",openCount:" open-count
                ",closedCount:" closed-count
                ",closedJsonUrl:'reports/all-closed.json'"
                (when source-type
                  (str ",sourceType:'" source-type "'"))
                "};\n"
                index-js "\n"
                theme-toggle-js)))

;; ---------------------------------------------------------------------------
;; Page assembly
;; ---------------------------------------------------------------------------

(defn index-page [reports reports-dir envelope]
  (let [source-type  (get envelope "source-type")
        types        (vec (distinct (map #(get % "type") reports)))
        types-json   (json/generate-string types)
        total        (get envelope "total" (count reports))
        open-count   (get envelope "open-count" (count reports))
        closed-count (get envelope "closed-count" 0)
        has-rss?     (.exists (clojure.java.io/file reports-dir "all.xml"))
        generated-at (str (java.util.Date.))
        rss-href     "reports/all.xml"
        cols         [[:th {:data-sort "type"     :onclick "sortTable(0,'type')"}     "Type"]
                      [:th {:data-sort "priority" :onclick "sortTable(1,'priority')"} "Prio"]
                      [:th {:data-sort "due"      :onclick "sortTable(2,'due')"}      "Due"]
                      [:th {:data-sort "flags"    :onclick "sortTable(3,'flags')"}    "Flags"]
                      [:th {:data-sort "subject"  :onclick "sortTable(4,'subject')"}  "Subject"]
                      [:th {:data-sort "from"     :onclick "sortTable(5,'from')"}     "Author"]
                      [:th {:data-sort "date"     :onclick "sortTable(6,'date')"}     "Date"]
                      [:th {:data-sort "replies"  :onclick "sortTable(7,'replies')"}  "↩"]]]
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
         [:p {:style "font-size:0.78rem;color:var(--pico-muted-color);margin-bottom:1rem"}
          (str "Generated " generated-at)]
         [:div.toolbar
          [:input#si {:type        "search"
                      :placeholder "Search"
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
            "Owned"]]]
         [:div#status]
         [:figure {:style "overflow-x:auto"}
          [:table
           [:thead [:tr (seq cols)]]
           [:tbody
            (for [r reports]
              (report-row r source-type))]]]
         [:script (h/raw (page-js types-json total open-count closed-count source-type))]]
        (bark-footer)]]))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [{:keys [out-file json-file out-dir theme]}
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
        html     (index-page reports reports-dir envelope)]
    (spit-html out-file html)
    (binding [*out* *err*]
      (log/info "Wrote" (count reports) "reports to" out-file))))
