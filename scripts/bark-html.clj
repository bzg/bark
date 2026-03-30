;; bark-html.clj — Shared HTML utilities for bark-index.clj and bark-stats.clj.
;;
;; Usage: (load-file "scripts/bark-html.clj")

(require '[clojure.string :as str]
         '[cheshire.core :as json]
         '[hiccup2.core :as h])

;; ---------------------------------------------------------------------------
;; Shared CDN
;; ---------------------------------------------------------------------------

(def pico-cdn "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css")

;; ---------------------------------------------------------------------------
;; Theme CDN — set via config.edn :theme or --theme CLI flag
;; ---------------------------------------------------------------------------

(def ^:private themes-cdn
  "Base URL for bzg/pico-themes on jsDelivr."
  "https://cdn.jsdelivr.net/gh/bzg/pico-themes@latest/")

(defn resolve-css-theme
  "Resolve a --theme / :theme value to a seq of maps, or nil.
  Each map is either {:link url} or {:inline css-content}.

  Resolution order:
  1. https:// URL           → [{:link url}]
  2. file:///path           → [{:inline (slurp path)}]
  3. path ending in .css    → [{:inline (slurp path)}] (relative or absolute)
  4. bare name (no spaces)  → pico-themes CDN [{:link base} {:link bark-overlay}]
  5. \"none\" or nil          → nil"
  [theme]
  (when (and theme (not= theme "none"))
    (cond
      (str/starts-with? theme "https://")
      [{:link theme}]

      (str/starts-with? theme "file:///")
      (let [path (subs theme (count "file://"))]
        [{:inline (slurp path)}])

      (str/ends-with? theme ".css")
      (let [f (java.io.File. theme)]
        (if (.isFile f)
          [{:inline (slurp f)}]
          (do (binding [*out* *err*]
                (println (str "Warning: CSS file not found: " theme)))
              nil)))

      (not (str/includes? theme " "))
      [{:link (str themes-cdn theme ".css")}
       {:link (str themes-cdn "bark/" theme ".css")}]

      :else
      (do (binding [*out* *err*]
            (println (str "Warning: cannot resolve theme '" theme "'")))
          nil))))

(def ^:private theme-atom (atom nil))

(defn set-theme!
  "Set the resolved theme from a name, path, or URL. Called by each script at startup."
  [theme]
  (reset! theme-atom (resolve-css-theme theme)))

(defn resolved-theme
  "Current resolved theme entries as a seq of maps, or nil."
  []
  @theme-atom)

;; ---------------------------------------------------------------------------
;; Shared metadata
;; ---------------------------------------------------------------------------

(def bark-description "BARK (Bug And Report Keeper) — track bugs, patches, and requests.")
(def bark-repo-url "https://codeberg.org/bzg/bark")

;; ---------------------------------------------------------------------------
;; LibreJS license tags (JS files are MPL-2.0)
;; ---------------------------------------------------------------------------

(def js-license-start
  "// @license magnet:?xt=urn:btih:3877d6d54b3accd4bc32f8a48bf32ebc0901502a&dn=mpl-2.0.txt MPL-2.0")
(def js-license-end
  "// @license-end")

(defn wrap-js
  "Wrap JavaScript code with LibreJS license tags."
  [js]
  (str js-license-start "\n" js "\n" js-license-end))

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
       (when-let [entries (resolved-theme)]
         (str/join (map (fn [{:keys [link inline]}]
                          (if link
                            (str "<link rel=\"stylesheet\" href=\"" link "\">\n")
                            (str "<style>\n" inline "\n</style>\n")))
                        entries)))
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
;; Inline SVG logo (loaded from resources/ at build time)
;; ---------------------------------------------------------------------------

(def bark-logo-svg
  "Inline SVG logo for the nav bar, scaled to match text height."
  (slurp "resources/bark-logo.svg"))

;; ---------------------------------------------------------------------------
;; Shared nav bar (hiccup vector)
;; ---------------------------------------------------------------------------

(def nav-pages
  "Navigation pages in display order: [id label href]."
  [["reports" "Reports" "index.html"]
   ["docs"   "Docs"  "docs.html"]
   ["data"    "Data"    "data.html"]])

(defn nav-bar
  "Render a <nav> with BARK logo, title, and Reports/Docs/Data links.
  `current` is the id of the active page (bolded)."
  [title current]
  [:nav
   [:ul [:li {:style "display:flex;align-items:center;gap:0.5rem"}
         (h/raw bark-logo-svg)
         [:strong title]]]
   [:ul
    (for [[id label href] nav-pages]
      [:li (if (= id current)
             [:a {:href href} [:strong label]]
             [:a {:href href} label])])
    [:li (theme-toggle-btn)]]])

;; ---------------------------------------------------------------------------
;; Shared footer (hiccup vector)
;; ---------------------------------------------------------------------------

(def footer-css
  "footer.bark-footer { font-size:0.78rem; color:var(--pico-muted-color); text-align:center; padding:2rem 0 1rem; }")

(defn bark-footer []
  [:footer.bark-footer
   [:a {:href bark-repo-url} "BARK"]
   " is "
   [:a {:href "https://www.gnu.org/philosophy/free-sw.html"}
    "Free Software"]
   " — "
   [:a {:href "reports/all.xml"} "RSS"]
   " — "
   [:a {:href "reports/all.json"} "JSON"]
   " — "
   [:a {:href "reports/all.org"} "Org"]
   " — "
   [:a {:href "events/announcements.ics"} "iCal"]])

;; ---------------------------------------------------------------------------
;; Org-mode inline link conversion (shared by bark-docs, bark-stats)
;; ---------------------------------------------------------------------------

(defn org-inline-links
  "Convert org-mode links to HTML anchors.
  [[url][label]] -> <a href=\"url\">label</a>
  [[url]]        -> <a href=\"url\">url</a>"
  [s]
  (-> s
      (str/replace #"\[\[([^\]]+)\]\[([^\]]+)\]\]" "<a href=\"$1\">$2</a>")
      (str/replace #"\[\[([^\]]+)\]\]" "<a href=\"$1\">$1</a>")))

;; ---------------------------------------------------------------------------
;; HTML tidy (optional — graceful no-op when tidy is not installed)
;; ---------------------------------------------------------------------------

(def ^:private tidy-available?
  "True when the `tidy` binary is on PATH."
  (delay
    (try
      (-> (babashka.process/process ["tidy" "--version"]
                                    {:out :string :err :string})
          deref :exit (= 0))
      (catch Exception _ false))))

(defn tidy-html
  "Pretty-print an HTML string via tidy.  Returns the input unchanged if
  tidy is not installed or if it fails on the input."
  [html]
  (if-not @tidy-available?
    html
    (try
      (let [result @(babashka.process/process
                     ["tidy" "-qi" "--wrap" "0" "--tidy-mark" "no"]
                     {:in html :out :string :err :string})]
        ;; tidy exits 0 = ok, 1 = warnings (normal), 2 = errors
        (if (<= (:exit result) 1)
          (:out result)
          html))
      (catch Exception _ html))))

(defn spit-html
  "Write `html` to `file`, running it through tidy when available."
  [file html]
  (spit file (tidy-html html)))
