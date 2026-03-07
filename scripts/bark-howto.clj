#!/usr/bin/env bb

;; bark-howto.clj — Generate public/howto.html from docs/howto.org.
;;
;; Reads the org file as a lightweight template, substitutes instance-specific
;; placeholders from config.edn, and renders a standalone HTML page.
;;
;; Placeholders (on their own line, wrapped in = in org):
;;   =sources=        — number of sources watched
;;   =sources-table=  — table with :name and :list-archive per source
;;
;; Usage:
;;   bb scripts/bark-howto.clj              → writes public/howto.html
;;   bb scripts/bark-howto.clj -o out.html  → writes out.html

(require '[clojure.string :as str]
         '[hiccup2.core :as h])

(load-file "scripts/bark-common.clj")
(load-file "scripts/bark-html.clj")

;; ---------------------------------------------------------------------------
;; Config
;; ---------------------------------------------------------------------------

(def default-output "public/howto.html")
(def org-file "docs/howto.org")

;; ---------------------------------------------------------------------------
;; Placeholder data from config.edn
;; ---------------------------------------------------------------------------

(defn sources-table-html
  "Build an HTML table of sources from config."
  [sources]
  (str "<table>\n<thead><tr><th>Source</th><th>Archive</th></tr></thead>\n<tbody>\n"
       (str/join
        (map (fn [src]
               (let [n (:name src)
                     a (:list-archive src)]
                 (str "<tr><td>" (h/html (h/raw (str n))) "</td><td>"
                      (if a
                        (str "<a href=\"" a "\">" a "</a>")
                        "—")
                      "</td></tr>\n")))
             sources))
       "</tbody></table>"))

(defn build-placeholders
  "Build placeholder-key → HTML-string from config."
  [config]
  (let [sources (:sources config)]
    {"=sources="       (str (count sources) " source" (when (> (count sources) 1) "s"))
     "=sources-table=" (sources-table-html sources)}))

;; ---------------------------------------------------------------------------
;; Minimal org → HTML conversion
;; ---------------------------------------------------------------------------

(defn- org-inline
  "Convert inline org markup to HTML."
  [s]
  (-> s
      ;; Links: [[url][desc]] or [[url]]
      (str/replace #"\[\[([^\]]+)\]\[([^\]]+)\]\]" "<a href=\"$1\">$2</a>")
      (str/replace #"\[\[([^\]]+)\]\]" "<a href=\"$1\">$1</a>")
      ;; Code: =code=
      (str/replace #"=([^=\n]+)=" "<code>$1</code>")
      ;; Bold: *bold*  (word-bounded to avoid conflicts with list bullets)
      (str/replace #"(?<=\s|^)\*([^*\n]+)\*(?=[\s.,;:!?)]|$)" "<strong>$1</strong>")))

(defn- heading-id
  "Generate an anchor id from heading text."
  [text]
  (-> text str/lower-case str/trim
      (str/replace #"[^a-z0-9 -]" "")
      (str/replace #"\s+" "-")))

(defn- parse-table
  "Parse consecutive org table lines into an HTML table string."
  [lines]
  (let [rows (->> lines
                  (remove #(re-matches #"\s*\|[-+]+\|\s*" %))  ;; skip hlines
                  (mapv (fn [line]
                          (->> (str/split line #"\|")
                               (drop 1)       ;; leading empty
                               butlast         ;; trailing empty
                               (mapv str/trim)))))]
    (when (seq rows)
      (let [header (first rows)
            body   (rest rows)]
        (str "<table>\n<thead><tr>"
             (str/join (map #(str "<th>" (org-inline %) "</th>") header))
             "</tr></thead>\n<tbody>\n"
             (str/join (map (fn [r]
                              (str "<tr>"
                                   (str/join (map #(str "<td>" (org-inline %) "</td>") r))
                                   "</tr>\n"))
                            body))
             "</tbody></table>")))))

(defn org->html
  "Convert org text to HTML body content.
  Handles headings, paragraphs, tables, example blocks, src blocks, and
  inline markup.  Substitutes placeholders."
  [org-text placeholders]
  (let [lines (str/split-lines org-text)]
    (loop [i 0, acc (transient []), in-para? false]
      (if (>= i (count lines))
        (let [acc (if in-para? (conj! acc "</p>") acc)]
          (str/join "\n" (persistent! acc)))
        (let [line (nth lines i)
              trimmed (str/trim line)]
          (cond
            ;; Placeholder line (standalone =placeholder=)
            (contains? placeholders trimmed)
            (let [acc (if in-para? (conj! acc "</p>") acc)]
              (recur (inc i) (conj! acc (get placeholders trimmed)) false))

            ;; Heading
            (re-matches #"\*+ .+" line)
            (let [[_ stars text] (re-find #"^(\*+) (.+)" line)
                  level (min (count stars) 6)
                  ;; Strip <<custom-id>> anchors
                  text  (str/replace text #"\s*<<[^>]+>>\s*" "")
                  ;; Strip CUSTOM_ID properties (on next lines — already separate)
                  tag   (str "h" level)
                  id    (heading-id text)
                  acc   (if in-para? (conj! acc "</p>") acc)]
              (recur (inc i)
                     (conj! acc (str "<" tag " id=\"" id "\">" (org-inline text) "</" tag ">"))
                     false))

            ;; Property drawer / custom-id — skip
            (or (= trimmed ":PROPERTIES:")
                (= trimmed ":END:")
                (re-matches #":CUSTOM_ID:.*" trimmed))
            (recur (inc i) acc in-para?)

            ;; Example block
            (re-matches #"(?i)#\+begin_example" trimmed)
            (let [acc  (if in-para? (conj! acc "</p>") acc)
                  acc  (conj! acc "<pre>")
                  next (loop [j (inc i), a acc]
                         (if (>= j (count lines))
                           [j a]
                           (let [bl (str/trim (nth lines j))]
                             (if (re-matches #"(?i)#\+end_example" bl)
                               [(inc j) a]
                               (recur (inc j)
                                      (conj! a (-> (nth lines j)
                                                   (str/replace "&" "&amp;")
                                                   (str/replace "<" "&lt;"))))))))]
              (recur (first next) (conj! (second next) "</pre>") false))

            ;; Src block
            (re-matches #"(?i)#\+begin_src.*" trimmed)
            (let [acc  (if in-para? (conj! acc "</p>") acc)
                  acc  (conj! acc "<pre><code>")
                  next (loop [j (inc i), a acc]
                         (if (>= j (count lines))
                           [j a]
                           (let [bl (str/trim (nth lines j))]
                             (if (re-matches #"(?i)#\+end_src" bl)
                               [(inc j) a]
                               (recur (inc j)
                                      (conj! a (-> (nth lines j)
                                                   (str/replace "&" "&amp;")
                                                   (str/replace "<" "&lt;"))))))))]
              (recur (first next) (conj! (second next) "</code></pre>") false))

            ;; Table line — collect consecutive table lines
            (str/starts-with? trimmed "|")
            (let [acc   (if in-para? (conj! acc "</p>") acc)
                  tlines (loop [j i, tl []]
                           (if (and (< j (count lines))
                                    (str/starts-with? (str/trim (nth lines j)) "|"))
                             (recur (inc j) (conj tl (nth lines j)))
                             [j tl]))
                  [next-i table-lines] tlines]
              (recur next-i (conj! acc (parse-table table-lines)) false))

            ;; Blank line — close paragraph
            (str/blank? trimmed)
            (let [acc (if in-para? (conj! acc "</p>") acc)]
              (recur (inc i) acc false))

            ;; Comment / keyword lines — skip
            (or (str/starts-with? trimmed "#")
                (str/starts-with? trimmed "#+"))
            (recur (inc i) acc in-para?)

            ;; Regular text — paragraph
            :else
            (if in-para?
              (recur (inc i) (conj! acc (str (org-inline trimmed))) true)
              (recur (inc i) (conj! acc (str "<p>" (org-inline trimmed))) true))))))))

;; ---------------------------------------------------------------------------
;; Page assembly
;; ---------------------------------------------------------------------------

(def howto-css "
  main.container { max-width: 1600px; }
  table { font-size: 0.9rem; }
  pre { font-size: 0.85rem; padding: 1rem; }
  .theme-toggle { cursor: pointer; background: none; border: none; font-size: 1.2rem; padding: 0.3rem; }
  .meta { font-size: 0.78rem; color: var(--pico-muted-color); margin-bottom: 2rem; }
")

(defn page [body-html]
  (let [has-index? (.exists (clojure.java.io/file "public/index.html"))
        has-stats? (.exists (clojure.java.io/file "public/stats.html"))
        generated-at (str (java.util.Date.))]
    (str
     "<!DOCTYPE html>\n"
     (h/html
      [:html {:lang "en" :data-theme "light"}
       [:head
        [:meta {:charset "UTF-8"}]
        [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
        [:meta {:name "color-scheme" :content "light dark"}]
        [:link {:rel "stylesheet" :href pico-cdn}]
        [:title "BARK — How-to"]
        [:style (h/raw howto-css)]]
       [:body
        [:main.container
         [:nav
          [:ul [:li [:strong "BARK — How-to"]]]
          [:ul
           (when has-index?
             [:li [:a {:href "index.html" :title "Reports"} "Reports"]])
           (when has-stats?
             [:li [:a {:href "stats.html" :title "Statistics"} "Stats"]])
           [:li (theme-toggle-btn)]]]
         [:p.meta (str "Generated " generated-at)]
         (h/raw body-html)
         [:script (h/raw theme-toggle-js)]]]]))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [{:keys [out-file]} (parse-cli-args *command-line-args*)
      out-file     (or out-file default-output)
      config       (load-config)
      placeholders (if config (build-placeholders config) {})
      org-text     (slurp org-file)
      body-html    (org->html org-text placeholders)
      html         (page body-html)]
  (.mkdirs (clojure.java.io/file "public"))
  (spit out-file html)
  (binding [*out* *err*]
    (println (str "Wrote " out-file))))
