#!/usr/bin/env bb

;; bark-html.clj — Generate a static HTML page from BARK reports.
;;
;; Reads reports.json (produced by bark-egest) and config.edn, then
;; builds a standalone HTML page.  Most logic is in Clojure; JS is
;; limited to client-side filtering, sorting, theme toggle, and URL
;; permalink state.
;;
;; Usage:
;;   bb html                                    → via bb task
;;   bb scripts/bark-html.clj                   → writes index.html
;;   bb scripts/bark-html.clj -o reports.html   → writes reports.html

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
(def bark-doc-url "https://codeberg.org/bzg/bark")

(def type-labels {"bug" "bug" "announcement" "ann" "request" "req"
                  "patch" "patch" "release" "rel" "change" "chg"})

;; ---------------------------------------------------------------------------
;; Roles from config.edn
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Generate reports.json via bark-egest
;; ---------------------------------------------------------------------------

(defn generate-json! [source-name min-priority min-status]
  (let [f   (clojure.java.io/file json-file)
        _   (when (.exists f) (.delete f))
        cmd (cond-> ["bb" "scripts/bark-egest.clj" "json"]
              source-name  (into ["-n" source-name])
              min-priority (into ["-p" (str min-priority)])
              min-status   (into ["-s" (str min-status)]))
        {:keys [exit]} (apply process/shell {:continue true} cmd)]
    (when-not (zero? exit)
      (binding [*out* *err*] (println "bark-egest failed (exit" exit ")"))
      (System/exit 1))
    (when-not (.exists f)
      (binding [*out* *err*] (println (str json-file " not produced")))
      (System/exit 1))))

;; ---------------------------------------------------------------------------
;; Date normalization
;; ---------------------------------------------------------------------------

(defn- parse-to-iso-date
  "Extract a YYYY-MM-DD string from various date formats.
   Tries java.time parsing, then falls back to regex extraction."
  [s]
  (when (seq s)
    (let [s (str/trim s)]
      (or
       ;; ISO-ish: starts with YYYY-MM-DD
       (when (and (>= (count s) 10)
                  (re-matches #"\d{4}-\d{2}-\d{2}.*" s))
         (subs s 0 10))
       ;; java.util.Date.toString(): "Sat Mar 01 20:40:00 UTC 2025"
       (when-let [[_ mon day year]
                  (re-find #"^\w+ (\w+) (\d+) .* (\d{4})$" s)]
         (let [months {"Jan" "01" "Feb" "02" "Mar" "03" "Apr" "04"
                       "May" "05" "Jun" "06" "Jul" "07" "Aug" "08"
                       "Sep" "09" "Oct" "10" "Nov" "11" "Dec" "12"}]
           (when-let [m (months mon)]
             (str year "-" m "-" (format "%02d" (parse-long day))))))
       ;; Truncated: "Sat Mar 01 20:40" — no year available, assumes current year.
       ;; NB: may be wrong for emails from December viewed in January.
       (when-let [[_ mon day]
                  (re-find #"^\w+ (\w+) (\d+) " s)]
         (let [months {"Jan" "01" "Feb" "02" "Mar" "03" "Apr" "04"
                       "May" "05" "Jun" "06" "Jul" "07" "Aug" "08"
                       "Sep" "09" "Oct" "10" "Nov" "11" "Dec" "12"}
               year (str (.getYear (java.time.LocalDate/now)))]
           (when-let [m (months mon)]
             (str year "-" m "-" (format "%02d" (parse-long day))))))
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

(defn- subject-el
  "Render subject with optional admin/maintainer styling and archive link."
  [subject role archived-at]
  (let [inner (case role
                "admin"      [:strong subject]
                "maintainer" [:em subject]
                subject)]
    (if archived-at
      [:a {:href archived-at} inner]
      inner)))

(defn- related-mids
  "Comma-joined message-ids of related reports, for JS search."
  [related]
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

(defn- report-row [multi-src? {:strs [type subject from date date-raw flags status priority
                            replies archived-at message-id related role source
                            acked owned closed]}]
  (let [label    (get type-labels type type)
        closed?  (and flags (>= (count flags) 3) (= (nth flags 2 \-) \C))
        iso-date (parse-to-iso-date (or date-raw date ""))]
    [:tr {:data-type    type
          :data-closed  (str closed?)
          :data-mid     (or message-id "")
          :data-from    (str/lower-case (or from ""))
          :data-subject (str/lower-case (or subject ""))
          :data-date    iso-date
          :data-source  (or source "")
          :data-acked   (str/lower-case (or acked ""))
          :data-owned   (str/lower-case (or owned ""))
          :data-closedby (str/lower-case (or closed ""))
          :data-search  (str/lower-case (str subject " " from " " iso-date))}
     [:td [:mark {:data-type type} label]]
     [:td {:data-value (str (or status 0))} (status-square flags)]
     [:td (priority-square priority)]
     (when multi-src? [:td [:small (or source "")]])
     [:td (subject-el subject role archived-at) (related-link related)]
     [:td.secondary from]
     [:td [:small (or iso-date date "")]]
     [:td {:style "text-align:center"} (or replies 0)]]))

;; ---------------------------------------------------------------------------
;; CSS (inlined)
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
  input[type=search] { max-width: 300px; margin-bottom: 0; }
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
;; All data attributes are pre-computed in Clojure as YYYY-MM-DD.
;; ---------------------------------------------------------------------------

(defn page-js [types-json all-open?]
  (str "
  var allTypes = " types-json ";
  var activeTypes = {};
  allTypes.forEach(function(t) { activeTypes[t] = true; });
  var showClosed = " (if all-open? "true" "false") ";

  function getSearchInput() {
    return document.getElementById('si');
  }

  function setSearch(val) {
    getSearchInput().value = val;
    filterRows();
  }

  /* Format a Date as YYYY-MM-DD in local time */
  function localDate(d) {
    var y = d.getFullYear();
    var m = String(d.getMonth() + 1).padStart(2, '0');
    var day = String(d.getDate()).padStart(2, '0');
    return y + '-' + m + '-' + day;
  }

  /* Resolve a date token: '3d' → 3 days ago, 'YYYY-MM-DD' → as-is */
  function resolveDate(s) {
    if (!s) return '';
    var m = s.match(/^(\\d+)d$/);
    if (m) {
      var d = new Date();
      d.setDate(d.getDate() - parseInt(m[1]));
      return localDate(d);
    }
    if (/^\\d{4}-\\d{2}-\\d{2}$/.test(s)) return s;
    return '';
  }

  /* Parse structured search query */
  function parseQuery(q) {
    var result = { text: '', mids: [], froms: [], subjects: [], ackedBy: [], ownedBy: [], closedBy: [], dateFrom: '', dateTo: '' };
    var parts = q.split(/\\s+/);
    for (var i = 0; i < parts.length; i++) {
      var p = parts[i];
      if (p.indexOf('m:') === 0) {
        result.mids = p.substring(2).toLowerCase().split(',').filter(Boolean);
      } else if (p.indexOf('f:') === 0) {
        result.froms = p.substring(2).toLowerCase().split(',').filter(Boolean);
      } else if (p.indexOf('s:') === 0) {
        result.subjects = p.substring(2).toLowerCase().split(',').filter(Boolean);
      } else if (p.indexOf('a:') === 0) {
        result.ackedBy = p.substring(2).toLowerCase().split(',').filter(Boolean);
      } else if (p.indexOf('o:') === 0) {
        result.ownedBy = p.substring(2).toLowerCase().split(',').filter(Boolean);
      } else if (p.indexOf('c:') === 0) {
        result.closedBy = p.substring(2).toLowerCase().split(',').filter(Boolean);
      } else if (p.indexOf('d:') === 0) {
        var range = p.substring(2).split('..');
        result.dateFrom = resolveDate(range[0] || '');
        result.dateTo = resolveDate(range[1] || '') || localDate(new Date());
      } else {
        result.text += (result.text ? ' ' : '') + p;
      }
    }
    return result;
  }

  /* Test whether a row matches the parsed query */
  function matchRow(tr, q) {
    var d = tr.dataset;

    if (!activeTypes[d.type]) return false;
    if (!showClosed && d.closed === 'true') return false;

    if (q.mids.length > 0) {
      var mid = (d.mid || '').toLowerCase();
      if (!q.mids.some(function(m) { return mid.indexOf(m) !== -1; })) return false;
    }

    if (q.froms.length > 0) {
      if (!q.froms.some(function(f) { return d.from.indexOf(f) !== -1; })) return false;
    }

    if (q.subjects.length > 0) {
      if (!q.subjects.some(function(s) { return d.subject.indexOf(s) !== -1; })) return false;
    }

    if (q.ackedBy.length > 0) {
      if (!q.ackedBy.some(function(a) { return d.acked.indexOf(a) !== -1; })) return false;
    }

    if (q.ownedBy.length > 0) {
      if (!q.ownedBy.some(function(o) { return d.owned.indexOf(o) !== -1; })) return false;
    }

    if (q.closedBy.length > 0) {
      if (!q.closedBy.some(function(c) { return d.closedby.indexOf(c) !== -1; })) return false;
    }

    /* data-date is always YYYY-MM-DD, set by Clojure */
    if (q.dateFrom && d.date < q.dateFrom) return false;
    if (q.dateTo && d.date > q.dateTo) return false;

    if (q.text && d.search.indexOf(q.text.toLowerCase()) === -1) return false;

    return true;
  }

  function filterRows() {
    var q = parseQuery(getSearchInput().value);
    var rows = document.querySelectorAll('tbody tr');
    var visible = 0;
    rows.forEach(function(tr) {
      var show = matchRow(tr, q);
      tr.classList.toggle('hidden', !show);
      if (show) visible++;
    });
    document.getElementById('status').textContent = visible + '/' + rows.length + ' reports';
    updateURL();
  }

  function toggleType(type, btn) {
    activeTypes[type] = !activeTypes[type];
    btn.classList.toggle('outline');
    filterRows();
  }

  function toggleClosed() {
    showClosed = document.getElementById('show-closed').checked;
    filterRows();
  }

  /* Permalink: sync state to/from URL params */
  function updateURL() {
    var params = new URLSearchParams();
    var q = getSearchInput().value;
    if (q) params.set('q', q);
    var active = allTypes.filter(function(t) { return activeTypes[t]; });
    if (active.length !== allTypes.length) {
      params.set('types', active.join(','));
    }
    if (showClosed) params.set('closed', '1');
    var qs = params.toString();
    history.replaceState(null, '', location.pathname + (qs ? '?' + qs : ''));
  }

  function restoreFromURL() {
    var params = new URLSearchParams(location.search);
    if (params.has('q')) {
      getSearchInput().value = params.get('q');
    }
    if (params.has('types')) {
      var allowed = params.get('types').split(',');
      allTypes.forEach(function(t) { activeTypes[t] = allowed.indexOf(t) !== -1; });
      document.querySelectorAll('.filters button').forEach(function(btn) {
        var t = btn.dataset.type;
        if (t) btn.classList.toggle('outline', !activeTypes[t]);
      });
    }
    if (params.get('closed') === '1') {
      showClosed = true;
      document.getElementById('show-closed').checked = true;
    }
    filterRows();
  }

  /* Column sorting */
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
      var ac = a.children[colIdx], bc = b.children[colIdx];
      var av = (ac.getAttribute('data-value') || ac.textContent).trim().toLowerCase();
      var bv = (bc.getAttribute('data-value') || bc.textContent).trim().toLowerCase();
      var an = parseFloat(av), bn = parseFloat(bv);
      if (!isNaN(an) && !isNaN(bn)) return dir === 'asc' ? an - bn : bn - an;
      return dir === 'asc' ? av.localeCompare(bv) : bv.localeCompare(av);
    });
    rows.forEach(function(r) { tbody.appendChild(r); });
  }

  function toggleTheme() {
    var html = document.documentElement;
    var next = html.getAttribute('data-theme') === 'dark' ? 'light' : 'dark';
    html.setAttribute('data-theme', next);
    document.getElementById('theme-icon').textContent = next === 'dark' ? '☀️' : '🌙';
  }

  restoreFromURL();
"))

;; ---------------------------------------------------------------------------
;; Page assembly
;; ---------------------------------------------------------------------------

(defn page [reports min-status]
  (let [types      (vec (distinct (map #(get % "type") reports)))
        types-json (json/generate-string types)
        multi-src? (some #(get % "source") reports)
        all-open?  (and min-status (>= min-status 4))
        rss-file   "reports.rss"
        org-file   "reports.org"
        has-rss?   (.exists (clojure.java.io/file rss-file))
        has-org?   (.exists (clojure.java.io/file org-file))
        ;; Column indices depend on whether Source column is present
        ;; Type Status Priority [Source] Subject From Date ↩
        src-off    (if multi-src? 1 0)
        cols       (concat
                    [[:th {:data-sort "type" :onclick "sortTable(0,'type')"} "Type"]
                     [:th {:data-sort "status" :onclick "sortTable(1,'status')"} "Status"]
                     [:th {:data-sort "priority" :onclick "sortTable(2,'priority')"} "Priority"]]
                    (when multi-src?
                      [[:th {:data-sort "source"
                             :onclick (str "sortTable(3,'source')")} "Source"]])
                    [[:th {:data-sort "subject"
                           :onclick (str "sortTable(" (+ 3 src-off) ",'subject')")} "Subject"]
                     [:th {:data-sort "from"
                           :onclick (str "sortTable(" (+ 4 src-off) ",'from')")} "From"]
                     [:th {:data-sort "date"
                           :onclick (str "sortTable(" (+ 5 src-off) ",'date')")} "Date"]
                     [:th {:data-sort "replies"
                           :onclick (str "sortTable(" (+ 6 src-off) ",'replies')")} "↩"]])]
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
                  :title "BARK Reports RSS" :href rss-file}])
        [:title "BARK — Reports"]
        [:style (h/raw page-css)]]
       [:body
        [:main.container
         [:nav
          [:ul [:li [:strong "BARK — Reports"]]]
          [:ul
           (when has-rss?
             [:li [:a {:href rss-file :title "RSS feed"} "RSS"]])
           (when has-org?
             [:li [:a {:href org-file :title "Org file"} "Org"]])
           [:li [:a {:href bark-doc-url :title "BARK documentation"} "Docs"]]
           [:li [:button.theme-toggle
                 {:onclick "toggleTheme()" :aria-label "Toggle theme"}
                 [:span#theme-icon "🌙"]]]]]
         [:div.toolbar
          [:input#si {:type "search"
                      :placeholder "Search… m: f: s: a: o: c: d:3d.. d:2025-01-01..2025-03-01"
                      :oninput "filterRows()"}]
          [:div.filters
           (for [t types]
             [:button {:data-type t
                       :onclick (str "toggleType('" t "',this)")}
              (get type-labels t t)])]
          (when-not all-open?
            [:div.controls
             [:label
              [:input#show-closed {:type "checkbox" :onchange "toggleClosed()"}]
              " Show closed"]])]
         [:div#status]
         [:figure {:style "overflow-x:auto"}
          [:table.striped
           [:thead [:tr (seq cols)]]
           [:tbody
            (for [r reports]
              (report-row multi-src? r))]]]
         [:script (h/raw (page-js types-json all-open?))]]]]))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [args (vec *command-line-args*)
      {:keys [out-file source-name min-priority min-status]}
      (loop [opts {} [a & [v & r :as more]] args]
        (cond
          (nil? a)                        opts
          (#{"-o" "--output"} a)          (if v (recur (assoc opts :out-file v) r) opts)
          (#{"-n" "--source"} a)          (if v (recur (assoc opts :source-name v) r) opts)
          (#{"-p" "--min-priority"} a)    (if v (recur (assoc opts :min-priority (parse-long v)) r) opts)
          (#{"-s" "--min-status"} a)      (if v (recur (assoc opts :min-status (parse-long v)) r) opts)
          :else                           (recur opts more)))
      out-file (or out-file default-output)]
  (binding [*out* *err*]
    (println "Generating" json-file "via bark-egest…"))
  (generate-json! source-name min-priority min-status)
  (let [reports (json/parse-string (slurp json-file))
        html    (page reports min-status)]
    (spit out-file html)
    (binding [*out* *err*]
      (println (str "Wrote " (count reports) " reports to " out-file)))))
