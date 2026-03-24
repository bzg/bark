#!/usr/bin/env bb

;; bark-docs.clj — Generate public/<source>/docs.html from resources/docs-tpl.org.
;;
;; Reads the org template and substitutes source-specific labels and
;; commands into the unified table, based on merged config
;; (defaults -> global -> per-source).
;;
;; Usage:
;;   bb scripts/bark-docs.clj -n my-source              -> public/my-source/docs.html
;;   bb scripts/bark-docs.clj -n my-source -o out.html  -> writes out.html
;;   bb scripts/bark-docs.clj                           -> public/docs.html (defaults)

(require '[clojure.string :as str]
         '[hiccup2.core :as h])

;; Forward-declared for clj-kondo (provided at runtime by load-file calls below).
(declare default-labels default-commands
         resolve-labels-map resolve-commands-map
         parse-cli-args load-config build-source-map
         load-datalevin-pod! bark-schema parse-maintainer-since-entries
         pico-cdn theme-cdns set-theme! bark-description footer-css bark-footer wrap-js
         spit-html theme-toggle-btn theme-toggle-js nav-bar org-inline-links)

(load-file "scripts/bark-common.clj")
(load-file "scripts/bark-html.clj")

;; ---------------------------------------------------------------------------
;; Defaults — canonical definitions in bark-common.clj
;; ---------------------------------------------------------------------------

;; default-labels and default-commands are defined in bark-common.clj

;; ---------------------------------------------------------------------------
;; Resolve labels & commands with config merge chain
;; (using shared resolve-labels-map / resolve-commands-map from bark-common)
;; ---------------------------------------------------------------------------

(defn docs-labels [source-cfg]
  (resolve-labels-map source-cfg))

(defn docs-commands [source-cfg]
  (resolve-commands-map source-cfg))

;; ---------------------------------------------------------------------------
;; Build the org table from resolved labels + commands
;; ---------------------------------------------------------------------------

(defn- fmt-label-tags
  "Format label tags as org =code= entries for a given report type."
  [tags rtype]
  (let [versioned?  #{:release :change}
        with-topic? #{:bug :request :announcement}]
    (str/join " "
              (cond
                (= rtype :patch)
                (map (fn [t] (str "=[" t " <topic> <version> <n/m>]=")) tags)

                (versioned? rtype)
                (map (fn [t] (str "=[" t " <topic> <version>]=")) tags)

                (with-topic? rtype)
                (map (fn [t] (str "=[" t " <topic>]=")) tags)

                :else
                (map #(str "=[" % "]=") tags)))))

(defn- fmt-command-words
  "Format command words as org =code= entries."
  [words]
  (if (seq words)
    (str/join " " (map #(str "=" % "=") words))
    ""))

(defn build-labels-table-org
  "Build the labels-only org table."
  [labels]
  (let [types-upper [:bug :patch :request]
        types-lower [:announcement :release :change]
        all-types   (concat types-upper types-lower)
        rows  (mapv (fn [rtype]
                      {:type   (name rtype)
                       :labels (fmt-label-tags (get labels rtype) rtype)})
                    all-types)
        w-type   (apply max (count "Type")           (map #(count (:type %)) rows))
        w-labels (apply max (count "Subject labels") (map #(count (:labels %)) rows))
        pad      (fn [s w] (str s (apply str (repeat (max 0 (- w (count s))) " "))))
        hline    (str "|-" (apply str (repeat w-type "-")) "-+-"
                      (apply str (repeat w-labels "-")) "-|")
        row-str  (fn [{:keys [type labels]}]
                   (str "| " (pad type w-type)
                        " | " (pad labels w-labels) " |"))
        header   (row-str {:type "Type" :labels "Subject labels"})
        upper    (map row-str (take 3 rows))
        lower    (map row-str (drop 3 rows))]
    (str/join "\n" (concat [header hline] upper [hline] lower))))

(defn build-commands-table-org
  "Build the merged status+priority commands org table (3 columns)."
  [cmds]
  (let [rows [["Mark as acked"             (fmt-command-words (:acked cmds))  "Status"]
              ["Mark as owned"             (fmt-command-words (:owned cmds))  "Status"]
              ["Mark as closed (canceled)" (fmt-command-words (filterv #(contains? #{"Canceled" "Cancelled"} %)
                                                                       (:closed cmds))) "Status"]
              ["Mark as closed (expired)"  (fmt-command-words (filterv #(= "Expired" %)
                                                                       (:closed cmds))) "Status"]
              ["Mark as closed (resolved)" (fmt-command-words (filterv #(not (contains? #{"Canceled" "Cancelled" "Expired"} %))
                                                                       (:closed cmds))) "Status"]
              ["Mark as urgent"            (fmt-command-words ["Urgent"])    "Priority"]
              ["Mark as important"         (fmt-command-words ["Important"]) "Priority"]]
        w-effect  (apply max (count "Effect on report")  (map #(count (nth % 0)) rows))
        w-command (apply max (count "Command keyword")   (map #(count (nth % 1)) rows))
        w-type    (apply max (count "Type")              (map #(count (nth % 2)) rows))
        pad       (fn [s w] (str s (apply str (repeat (max 0 (- w (count s))) " "))))
        hline     (str "|-" (apply str (repeat w-effect "-")) "-+-"
                       (apply str (repeat w-command "-")) "-+-"
                       (apply str (repeat w-type "-")) "-|")
        row-str   (fn [[effect command typ]]
                    (str "| " (pad effect w-effect)
                         " | " (pad command w-command)
                         " | " (pad typ w-type) " |"))
        header    (row-str ["Effect on report" "Command keyword" "Type"])]
    (str/join "\n" (concat [header hline] (map row-str rows)))))

;; ---------------------------------------------------------------------------
;; Template substitution — detect and replace org table blocks
;; ---------------------------------------------------------------------------

(defn- table-line? [s] (str/starts-with? (str/trim s) "|"))

(defn- find-table-blocks
  "Return a seq of [start-idx end-idx] for each contiguous table block."
  [lines]
  (loop [i 0, blocks [], in-table? false, start nil]
    (if (>= i (count lines))
      (if in-table? (conj blocks [start (dec i)]) blocks)
      (if (table-line? (nth lines i))
        (recur (inc i) blocks true (or start i))
        (if in-table?
          (recur (inc i) (conj blocks [start (dec i)]) false nil)
          (recur (inc i) blocks false nil))))))

(defn substitute-template
  "Replace the first two org table blocks in org-text:
  the first with the resolved labels table, the second with the commands table."
  [org-text labels cmds]
  (let [lines  (str/split-lines org-text)
        blocks (find-table-blocks lines)]
    (if (>= (count blocks) 2)
      (let [[t2-start t2-end] (nth blocks 1)
            [t1-start t1-end] (nth blocks 0)]
        ;; Replace second table first (so indices stay valid)
        (str/join "\n"
                  (concat (take t1-start lines)
                          [(build-labels-table-org labels)]
                          (subvec (vec lines) (inc t1-end) t2-start)
                          [(build-commands-table-org cmds)]
                          (drop (inc t2-end) lines))))
      ;; Fallback: only one table — replace with labels only
      (if (seq blocks)
        (let [[t1-start t1-end] (first blocks)]
          (str/join "\n"
                    (concat (take t1-start lines)
                            [(build-labels-table-org labels)]
                            (drop (inc t1-end) lines))))
        org-text))))

;; ---------------------------------------------------------------------------
;; Minimal org -> HTML conversion
;; ---------------------------------------------------------------------------

(defn- org-inline [s]
  (-> s
      org-inline-links
      (str/replace #"=([^=\n\"]+)="
                   (fn [[_ inner]]
                     (str "<code>"
                          (-> inner
                              (str/replace "&" "&amp;")
                              (str/replace "<" "&lt;")
                              (str/replace ">" "&gt;"))
                          "</code>")))
      (str/replace #"(?<=\s|^)\*([^*\n]+)\*(?=[\s.,;:!?)]|$)" "<strong>$1</strong>")
      (str/replace "\\vert" "|")))

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

(def docs-css (str "
  main.container { max-width: 1600px; }
  table { font-size: 0.9rem; }
  pre { font-size: 0.85rem; padding: 1rem; }
  .theme-toggle { cursor: pointer; background: none; border: none; font-size: 1.2rem; padding: 0.3rem; }
  .meta { font-size: 0.78rem; color: var(--pico-muted-color); margin-bottom: 2rem; }
" footer-css))

(defn docs-page [body-html]
  (let [title        "BARK — Docs"
        generated-at (str (java.util.Date.))]
    (str
     "<!DOCTYPE html>\n"
     (h/html
      [:html {:lang "en" :data-theme "light"}
       [:head
        [:meta {:charset "UTF-8"}]
        [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
        [:meta {:name "color-scheme" :content "light dark"}]
        [:meta {:name "description" :content bark-description}]
        [:meta {:property "og:title" :content title}]
        [:meta {:property "og:description" :content bark-description}]
        [:meta {:property "og:type" :content "website"}]
        [:link {:rel "stylesheet" :href pico-cdn}]
        (for [url (theme-cdns)]
          [:link {:rel "stylesheet" :href url}])
        [:title title]
        [:style (h/raw docs-css)]]
       [:body
        [:main.container
         (nav-bar title "docs")
         [:p.meta (str "Generated " generated-at)]
         (h/raw body-html)
         [:script (h/raw (wrap-js theme-toggle-js))]]
        (bark-footer)]]))))

;; ---------------------------------------------------------------------------
;; Filter feed links in "Getting the data" table
;; ---------------------------------------------------------------------------

(defn- strip-dead-links
  "Remove org links [[file][label]] whose target does not exist in out-dir.
  Bare links are replaced by their label; separators (', ') before/after
  removed links are cleaned up."
  [cell out-dir]
  (let [;; Replace each [[target][label]] with label if file exists, else ""
        replaced (str/replace
                  cell
                  #"\[\[([^\]]+)\]\[([^\]]+)\]\]"
                  (fn [[_ target label]]
                    (if (.exists (clojure.java.io/file out-dir target))
                      (str "[[" target "][" label "]]")
                      "")))
        ;; Clean up separators: collapse multiple ", " and trim
        cleaned (-> replaced
                    str/trim
                    (str/replace #",\s*,+" ",")
                    (str/replace #"^,\s*" "")
                    (str/replace #",\s*$" "")
                    str/trim)]
    cleaned))

(defn filter-feed-links
  "Process the org text: in table rows that contain feed links (*.json,
  *.xml, *.org), remove links to files that don't exist in out-dir.
  Removes entire rows where all links have been stripped.
  Cleans up adjacent hlines left by removed rows."
  [org-text out-dir]
  (if-not out-dir
    org-text
    (let [lines (str/split-lines org-text)
          hline? #(re-matches #"\s*\|[-+]+\|\s*" %)]
      (->> lines
           (map (fn [line]
                  (if (and (str/starts-with? (str/trim line) "|")
                           (re-find #"\[\[.+\.(json|xml|org)\]" line))
                    ;; This is a table row with feed links — process each cell
                    (let [cells (->> (str/split line #"\|" -1)
                                     (drop 1) butlast
                                              (mapv #(str/trim %)))
                          filtered (mapv #(strip-dead-links % out-dir) cells)
                          ;; Drop row if format column (2nd cell) is empty
                          format-cell (get filtered 1)]
                      (when-not (str/blank? format-cell)
                        (str "| " (str/join " | " filtered) " |")))
                    line)))
           (remove nil?)
           ;; Remove consecutive hlines (keep first)
           (reduce (fn [acc line]
                     (if (and (hline? line)
                              (seq acc)
                              (hline? (peek acc)))
                       acc
                       (conj acc line)))
                   [])
           (str/join "\n")))))

;; ---------------------------------------------------------------------------
;; Maintainers section
;; ---------------------------------------------------------------------------

(defn- contributor-name
  "Look up a contributor's display name by email for a given source.
  Returns the name if found and non-blank, otherwise nil."
  [db source-name email]
  (let [dq (resolve 'pod.huahaiy.datalevin/q)
        k  (str source-name ":" (str/lower-case email))]
    (when-let [n (dq '[:find ?n .
                       :in $ ?k
                       :where [?e :contributor/key ?k]
                       [?e :contributor/name ?n]]
                     db k)]
      (when-not (str/blank? n) n))))

(defn- html-escape
  "Escape HTML special characters in a string."
  [s]
  (when s
    (-> s
        (str/replace "&" "&amp;")
        (str/replace "<" "&lt;")
        (str/replace ">" "&gt;")
        (str/replace "\"" "&quot;"))))

;; parse-maintainer-since-entries is in bark-common.clj

(defn build-maintainers-html
  "Build an HTML section listing maintainers by display name,
  with since-dates when available. Only current maintainers are shown.
  Maintainers sharing the same display name (or contributor name) are
  deduplicated, keeping the earliest since-date."
  [db source-name source-cfg]
  (when source-name
    (let [dp          (resolve 'pod.huahaiy.datalevin/pull)
          roles       (dp db '[:roles/admin :roles/maintainers :roles/maintainer-since]
                          [:roles/source source-name])
          maint-v     (:roles/maintainers roles)
          maint-emails (cond (nil? maint-v) []
                             (string? maint-v) [maint-v]
                             :else maint-v)
          since-map   (parse-maintainer-since-entries roles)
          ;; Resolve each email to {display, since} then deduplicate by display name
          raw-entries (mapv (fn [email]
                              {:display (or (contributor-name db source-name email) email)
                               :since   (get since-map (str/lower-case email))})
                            maint-emails)
          ;; Group by display name, keep earliest since-date per person
          deduped     (->> raw-entries
                           (group-by :display)
                           vals
                           (mapv (fn [group]
                                   (let [display (:display (first group))
                                         sinces  (keep :since group)
                                         earliest (when (seq sinces)
                                                    (first (sort sinces)))]
                                     {:display display :since earliest}))))
          entries     (mapv (fn [{:keys [display since]}]
                              (let [escaped (html-escape display)]
                                (if since
                                  (str escaped " <small>(since " since ")</small>")
                                  escaped)))
                            deduped)]
      (when (seq entries)
        (str "<h2 id=\"maintainers\">Maintainers</h2>\n<ul>\n"
             (str/join "\n" (map #(str "<li>" % "</li>") entries))
             "\n</ul>")))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [{:keys [out-file out-dir source-name theme]} (parse-cli-args *command-line-args*)
      config      (load-config)
      _           (set-theme! (or theme (:theme config)))
      source-map  (when config (build-source-map config))
      source-cfg  (get source-map source-name)
      labels      (if source-cfg (docs-labels source-cfg) default-labels)
      cmds        (if source-cfg (docs-commands source-cfg) default-commands)
      out-file    (or out-file
                      (if source-name
                        (str "public/" source-name "/web/docs.html")
                        "public/web/docs.html"))
      ;; Infer out-dir from out-file when not given explicitly
      effective-dir (or out-dir
                        (.getParent (clojure.java.io/file out-file)))
      ;; Load DB for maintainer names
      db-path     (or (System/getenv "BARK_DB") "data/bark-db")
      _           (load-datalevin-pod!)
      conn        ((resolve 'pod.huahaiy.datalevin/get-conn) db-path bark-schema {:wal? false})
      db          ((resolve 'pod.huahaiy.datalevin/db) conn)
      maint-html  (build-maintainers-html db source-name source-cfg)
      org-text    (-> (slurp "resources/docs-tpl.org")
                      (substitute-template labels cmds)
                      (filter-feed-links effective-dir))
      body-html   (cond-> (org->html org-text)
                    maint-html (str "\n" maint-html))
      html        (docs-page body-html)]
  ((resolve 'pod.huahaiy.datalevin/close) conn)
  (.mkdirs (.getParentFile (clojure.java.io/file out-file)))
  (spit-html out-file html)
  (binding [*out* *err*]
    (log/info "Wrote" out-file)))
