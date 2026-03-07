#!/usr/bin/env bb

;; bark-howto.clj — Generate public/<source>/howto.html from docs/howto-tpl.org.
;;
;; Reads the org template and substitutes source-specific labels and
;; triggers into the unified table, based on merged config
;; (defaults → global → per-source).
;;
;; Usage:
;;   bb scripts/bark-howto.clj -n my-source              → public/my-source/howto.html
;;   bb scripts/bark-howto.clj -n my-source -o out.html  → writes out.html
;;   bb scripts/bark-howto.clj                           → public/howto.html (defaults)

(require '[clojure.string :as str]
         '[hiccup2.core :as h])

(load-file "scripts/bark-common.clj")
(load-file "scripts/bark-html.clj")

;; ---------------------------------------------------------------------------
;; Defaults (mirrors bark-detect.clj / bark-triggers.clj)
;; ---------------------------------------------------------------------------

(def default-labels
  {:bug          ["BUG"]
   :patch        ["PATCH"]
   :request      ["POLL" "FR" "TODO"]
   :announcement ["ANN" "ANNOUNCEMENT"]
   :release      ["REL" "RELEASE"]
   :change       ["CHG" "CHANGE"]})

(def default-trigger-words
  {:bug          {:acked ["Approved" "Confirmed"] :owned ["Handled"] :closed ["Canceled" "Fixed"]}
   :patch        {:acked ["Approved" "Reviewed"]  :owned ["Handled"] :closed ["Canceled" "Applied"]}
   :request      {:acked ["Approved"]             :owned ["Handled"] :closed ["Canceled" "Done" "Closed"]}
   :announcement {:closed ["Canceled"]}
   :release      {:closed ["Canceled"]}
   :change       {:closed ["Canceled"]}})

;; ---------------------------------------------------------------------------
;; Resolve labels & triggers with config merge chain
;; ---------------------------------------------------------------------------

(defn resolve-labels [source-cfg]
  (let [global  (:global-labels source-cfg)
        per-src (:labels source-cfg)]
    (cond
      (and global per-src) (merge default-labels global per-src)
      global               (merge default-labels global)
      per-src              (merge default-labels per-src)
      :else                default-labels)))

(defn resolve-triggers [source-cfg]
  (let [global  (:global-triggers source-cfg)
        per-src (:triggers source-cfg)
        deep-merge (fn [base overrides]
                     (reduce-kv (fn [acc rtype ov]
                                  (assoc acc rtype (merge (get acc rtype) ov)))
                                base overrides))]
    (cond
      (and global per-src) (-> default-trigger-words (deep-merge global) (deep-merge per-src))
      global               (deep-merge default-trigger-words global)
      per-src              (deep-merge default-trigger-words per-src)
      :else                default-trigger-words)))

;; ---------------------------------------------------------------------------
;; Build the org table from resolved labels + triggers
;; ---------------------------------------------------------------------------

(defn- fmt-label-tags
  "Format label tags as org =code= entries for a given report type."
  [tags rtype]
  (let [versioned? #{:bug :patch :release :change}]
    (str/join " "
              (if (versioned? rtype)
                (mapcat (fn [t]
                          (case rtype
                            :bug   [(str "=[" t "]=") (str "=[" t " version]=")]
                            :patch [(str "=[" t "]=") (str "=[" t " n/m]=") (str "=[" t " topic n/m]=")]
                            [(str "=[" t "]=") (str "=[" t " version]=")]))
                        tags)
                (map #(str "=[" % "]=") tags)))))

(defn- fmt-trigger-words
  "Format trigger words as org =code= entries."
  [words]
  (if (seq words)
    (str/join " " (map #(str "=" % "=") words))
    ""))

(defn build-table-org
  "Build the unified labels+triggers org table."
  [labels triggers]
  (let [types-upper [:bug :patch :request]
        types-lower [:announcement :release :change]
        all-types   (concat types-upper types-lower)
        rows  (mapv (fn [rtype]
                      {:type    (name rtype)
                       :labels  (fmt-label-tags (get labels rtype) rtype)
                       :acked   (fmt-trigger-words (get-in triggers [rtype :acked]))
                       :owned   (fmt-trigger-words (get-in triggers [rtype :owned]))
                       :closed  (fmt-trigger-words (get-in triggers [rtype :closed]))})
                    all-types)
        w-type   (apply max (count "Type")           (map #(count (:type %)) rows))
        w-labels (apply max (count "Subject labels") (map #(count (:labels %)) rows))
        w-acked  (apply max (count "Acked")          (map #(count (:acked %)) rows))
        w-owned  (apply max (count "Owned")          (map #(count (:owned %)) rows))
        w-closed (apply max (count "Closed")         (map #(count (:closed %)) rows))
        pad      (fn [s w] (str s (apply str (repeat (max 0 (- w (count s))) " "))))
        hline    (str "|-" (apply str (repeat w-type "-")) "-+-"
                      (apply str (repeat w-labels "-")) "-+-"
                      (apply str (repeat w-acked "-")) "-+-"
                      (apply str (repeat w-owned "-")) "-+-"
                      (apply str (repeat w-closed "-")) "-|")
        row-str  (fn [{:keys [type labels acked owned closed]}]
                   (str "| " (pad type w-type)
                        " | " (pad labels w-labels)
                        " | " (pad acked w-acked)
                        " | " (pad owned w-owned)
                        " | " (pad closed w-closed) " |"))
        header   (row-str {:type "Type" :labels "Subject labels"
                           :acked "Acked" :owned "Owned" :closed "Closed"})
        upper    (map row-str (take 3 rows))
        lower    (map row-str (drop 3 rows))]
    (str/join "\n" (concat [header hline] upper [hline] lower))))

;; ---------------------------------------------------------------------------
;; Template substitution — detect and replace the org table block
;; ---------------------------------------------------------------------------

(defn- table-line? [s] (str/starts-with? (str/trim s) "|"))

(defn substitute-template
  "Replace the first org table block in org-text with the resolved table."
  [org-text labels triggers]
  (let [table-org (build-table-org labels triggers)
        lines     (str/split-lines org-text)
        ;; Find first and last indices of the table block
        first-tl  (first (keep-indexed (fn [i l] (when (table-line? l) i)) lines))
        last-tl   (last  (keep-indexed (fn [i l] (when (and first-tl
                                                            (>= i first-tl)
                                                            (table-line? l)
                                                            ;; stop at first gap
                                                            (every? #(or (table-line? (nth lines %))
                                                                         false)
                                                                    (range first-tl (inc i))))
                                                   i))
                                       lines))]
    (if (and first-tl last-tl)
      (str/join "\n"
                (concat (take first-tl lines)
                        [(str table-org)]
                        (drop (inc last-tl) lines)))
      ;; No table found — return as-is
      org-text)))

;; ---------------------------------------------------------------------------
;; Minimal org → HTML conversion
;; ---------------------------------------------------------------------------

(defn- org-inline [s]
  (-> s
      (str/replace #"\[\[([^\]]+)\]\[([^\]]+)\]\]" "<a href=\"$1\">$2</a>")
      (str/replace #"\[\[([^\]]+)\]\]" "<a href=\"$1\">$1</a>")
      (str/replace #"=([^=\n]+)=" "<code>$1</code>")
      (str/replace #"(?<=\s|^)\*([^*\n]+)\*(?=[\s.,;:!?)]|$)" "<strong>$1</strong>")))

(defn- heading-id [text]
  (-> text str/lower-case str/trim
      (str/replace #"[^a-z0-9 -]" "")
      (str/replace #"\s+" "-")))

(defn- parse-table [lines]
  (let [rows (->> lines
                  (remove #(re-matches #"\s*\|[-+]+\|\s*" %))
                  (mapv (fn [line]
                          (->> (str/split line #"\|" -1)
                               (drop 1) butlast
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

(defn org->html [org-text]
  (let [lines (str/split-lines org-text)]
    (loop [i 0, acc (transient []), in-para? false]
      (if (>= i (count lines))
        (let [acc (if in-para? (conj! acc "</p>") acc)]
          (str/join "\n" (persistent! acc)))
        (let [line (nth lines i)
              trimmed (str/trim line)]
          (cond
            (re-matches #"\*+ .+" line)
            (let [[_ stars text] (re-find #"^(\*+) (.+)" line)
                  level (min (count stars) 6)
                  text  (str/replace text #"\s*<<[^>]+>>\s*" "")
                  tag   (str "h" level)
                  id    (heading-id text)
                  acc   (if in-para? (conj! acc "</p>") acc)]
              (recur (inc i)
                     (conj! acc (str "<" tag " id=\"" id "\">" (org-inline text) "</" tag ">"))
                     false))

            (or (= trimmed ":PROPERTIES:")
                (= trimmed ":END:")
                (re-matches #":CUSTOM_ID:.*" trimmed))
            (recur (inc i) acc in-para?)

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

            (str/starts-with? trimmed "|")
            (let [acc    (if in-para? (conj! acc "</p>") acc)
                  tlines (loop [j i, tl []]
                           (if (and (< j (count lines))
                                    (str/starts-with? (str/trim (nth lines j)) "|"))
                             (recur (inc j) (conj tl (nth lines j)))
                             [j tl]))
                  [next-i table-lines] tlines]
              (recur next-i (conj! acc (parse-table table-lines)) false))

            (str/blank? trimmed)
            (let [acc (if in-para? (conj! acc "</p>") acc)]
              (recur (inc i) acc false))

            (or (str/starts-with? trimmed "#")
                (str/starts-with? trimmed "#+"))
            (recur (inc i) acc in-para?)

            :else
            (if in-para?
              (recur (inc i) (conj! acc (org-inline trimmed)) true)
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
  (let [title        "BARK — How-to"
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
        [:title title]
        [:style (h/raw howto-css)]]
       [:body
        [:main.container
         [:nav
          [:ul [:li [:strong title]]]
          [:ul
           [:li [:a {:href "index.html" :title "Reports"} "Reports"]]
           [:li [:a {:href "stats.html" :title "Statistics"} "Stats"]]
           [:li (theme-toggle-btn)]]]
         [:p.meta (str "Generated " generated-at)]
         (h/raw body-html)
         [:script (h/raw theme-toggle-js)]]]]))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [{:keys [out-file source-name]} (parse-cli-args *command-line-args*)
      config      (load-config)
      source-map  (when config (build-source-map config))
      source-cfg  (get source-map source-name)
      labels      (if source-cfg (resolve-labels source-cfg) default-labels)
      triggers    (if source-cfg (resolve-triggers source-cfg) default-trigger-words)
      out-file    (or out-file
                      (if source-name
                        (str "public/" source-name "/howto.html")
                        "public/howto.html"))
      org-text    (-> (slurp "docs/howto-tpl.org")
                      (substitute-template labels triggers))
      body-html   (org->html org-text)
      html        (page body-html)]
  (.mkdirs (.getParentFile (clojure.java.io/file out-file)))
  (spit out-file html)
  (binding [*out* *err*]
    (println (str "Wrote " out-file))))
