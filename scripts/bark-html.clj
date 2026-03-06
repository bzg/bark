;; bark-html.clj — Shared HTML utilities for bark-index.clj and bark-stats.clj.
;;
;; Usage: (load-file "scripts/bark-html.clj")

(require '[clojure.string :as str]
         '[cheshire.core :as json])

;; ---------------------------------------------------------------------------
;; Shared CDN
;; ---------------------------------------------------------------------------

(def pico-cdn "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css")

;; ---------------------------------------------------------------------------
;; Shared theme-toggle JS (inlined into each page's <script> block)
;; ---------------------------------------------------------------------------

(def theme-toggle-js
  "function toggleTheme() {
    var html = document.documentElement;
    var next = html.getAttribute('data-theme') === 'dark' ? 'light' : 'dark';
    html.setAttribute('data-theme', next);
    document.getElementById('theme-icon').textContent = next === 'dark' ? '☀️' : '🌙';
  }")

;; ---------------------------------------------------------------------------
;; Shared <head> builder
;; ---------------------------------------------------------------------------

(defn html-head
  "Render a <head> block as a string.
   opts keys:
     :title      — page <title> (required)
     :css        — inline CSS string (optional)
     :extra-head — raw HTML string inserted before </head> (optional)
     :rss-href   — href for <link rel=alternate> RSS (optional)"
  [{:keys [title css extra-head rss-href]}]
  (str "<head>\n"
       "<meta charset=\"UTF-8\">\n"
       "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
       "<meta name=\"color-scheme\" content=\"light dark\">\n"
       "<link rel=\"stylesheet\" href=\"" pico-cdn "\">\n"
       (when rss-href
         (str "<link rel=\"alternate\" type=\"application/rss+xml\" "
              "title=\"BARK Reports RSS\" href=\"" rss-href "\">\n"))
       "<title>" title "</title>\n"
       (when css (str "<style>\n" css "\n</style>\n"))
       (or extra-head "")
       "</head>\n"))

;; ---------------------------------------------------------------------------
;; Shared nav theme-toggle button (hiccup vector)
;; ---------------------------------------------------------------------------

(defn theme-toggle-btn []
  [:button.theme-toggle
   {:onclick "toggleTheme()" :aria-label "Toggle theme"}
   [:span#theme-icon "🌙"]])
