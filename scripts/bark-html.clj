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
;; Shared metadata
;; ---------------------------------------------------------------------------

(def bark-description "BARK (Bug And Report Keeper) — track bugs, patches, and requests.")
(def bark-repo-url "https://codeberg.org/bzg/bark")
(def bark-license-url "https://codeberg.org/bzg/bark/src/branch/main/LICENSES/EPL-2.0.txt")

;; ---------------------------------------------------------------------------
;; Shared JS (loaded from resources/ at build time)
;; ---------------------------------------------------------------------------

(def theme-toggle-js (slurp "resources/bark-theme.js"))

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
       "<meta name=\"description\" content=\"" bark-description "\">\n"
       "<meta property=\"og:title\" content=\"" title "\">\n"
       "<meta property=\"og:description\" content=\"" bark-description "\">\n"
       "<meta property=\"og:type\" content=\"website\">\n"
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

;; ---------------------------------------------------------------------------
;; Shared footer (hiccup vector)
;; ---------------------------------------------------------------------------

(def footer-css
  "footer.bark-footer { font-size:0.78rem; color:var(--pico-muted-color); text-align:center; padding:2rem 0 1rem; }")

(defn bark-footer []
  [:footer.bark-footer
   [:a {:href bark-repo-url} "BARK"]
   " — Licensed under "
   [:a {:href bark-license-url} "EPL-2.0"]])
