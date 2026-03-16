#!/usr/bin/env bb

;; bark-howto.clj — Generate public/<source>/howto.html from resources/howto-tpl.org.
;;
;; Reads the org template and substitutes source-specific labels and
;; triggers into the unified table, based on merged config
;; (defaults -> global -> per-source).
;;
;; Usage:
;;   bb scripts/bark-howto.clj -n my-source              -> public/my-source/howto.html
;;   bb scripts/bark-howto.clj -n my-source -o out.html  -> writes out.html
;;   bb scripts/bark-howto.clj                           -> public/howto.html (defaults)

(require '[clojure.string :as str]
         '[hiccup2.core :as h])

;; Forward-declared for clj-kondo (provided at runtime by load-file calls below).
(declare default-labels default-triggers resolve-labels-map
         resolve-triggers-map parse-cli-args load-config build-source-map
         load-datalevin-pod! bark-schema
         pico-cdn bark-description footer-css bark-footer wrap-js
         theme-toggle-btn theme-toggle-js nav-bar)

(load-file "scripts/bark-common.clj")
(load-file "scripts/bark-html.clj")

;; ---------------------------------------------------------------------------
;; Defaults — canonical definitions in bark-common.clj
;; ---------------------------------------------------------------------------

;; default-labels and default-triggers are defined in bark-common.clj

;; ---------------------------------------------------------------------------
;; Resolve labels & triggers with config merge chain
;; (using shared resolve-labels-map / resolve-triggers-map from bark-common)
;; ---------------------------------------------------------------------------

(defn howto-labels [source-cfg]
  (resolve-labels-map source-cfg))

(defn howto-triggers [source-cfg]
  (resolve-triggers-map source-cfg))

;; ---------------------------------------------------------------------------
;; Build the org table from resolved labels + triggers
;; ---------------------------------------------------------------------------

(defn- fmt-label-tags
  "Format label tags as org =code= entries for a given report type."
  [tags rtype]
  (let [versioned?  #{:bug :patch :release :change}
        with-topic? #{:request :announcement}]
    (str/join " "
              (cond
                (versioned? rtype)
                (mapcat (fn [t]
                          (case rtype
                            :bug   [(str "=[" t "]=") (str "=[" t " version]=")]
                            :patch [(str "=[" t "]=") (str "=[" t " n/m]=") (str "=[" t " topic n/m]=")]
                            [(str "=[" t "]=") (str "=[" t " version]=")]))
                        tags)

                (with-topic? rtype)
                (mapcat (fn [t]
                          [(str "=[" t "]=") (str "=[" t " topic]=")])
                        tags)

                :else
                (map #(str "=[" % "]=") tags)))))

(defn- fmt-trigger-words
  "Format trigger words as org =code= entries."
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

(defn build-triggers-table-org
  "Build the flat triggers org table."
  [triggers]
  (let [rows [["acked"             (fmt-trigger-words (:acked triggers))]
              ["owned"             (fmt-trigger-words (:owned triggers))]
              ["closed (canceled)" (fmt-trigger-words (filterv #(contains? #{"Canceled" "Cancelled"} %)
                                                               (:closed triggers)))]
              ["closed (expired)"  (fmt-trigger-words (filterv #(= "Expired" %)
                                                               (:closed triggers)))]
              ["closed (resolved)" (fmt-trigger-words (filterv #(not (contains? #{"Canceled" "Cancelled" "Expired"} %))
                                                               (:closed triggers)))]]
        w-effect  (apply max (count "Effect on report")  (map #(count (first %)) rows))
        w-trigger (apply max (count "Trigger keyword")   (map #(count (second %)) rows))
        pad       (fn [s w] (str s (apply str (repeat (max 0 (- w (count s))) " "))))
        hline     (str "|-" (apply str (repeat w-effect "-")) "-+-"
                       (apply str (repeat w-trigger "-")) "-|")
        row-str   (fn [[effect trigger]]
                    (str "| " (pad effect w-effect) " | " (pad trigger w-trigger) " |"))
        header    (row-str ["Effect on report" "Trigger keyword"])]
    (str/join "\n" [header hline (row-str (nth rows 0)) (row-str (nth rows 1))
                    (row-str (nth rows 2)) (row-str (nth rows 3)) (row-str (nth rows 4))])))

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
  the first with the resolved labels table, the second with the triggers table."
  [org-text labels triggers]
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
                          [(build-triggers-table-org triggers)]
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
      (str/replace #"\[\[([^\]]+)\]\[([^\]]+)\]\]" "<a href=\"$1\">$2</a>")
      (str/replace #"\[\[([^\]]+)\]\]" "<a href=\"$1\">$1</a>")
      (str/replace #"=([^=\n\"<>]+)=" "<code>$1</code>")
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

(def howto-css (str "
  main.container { max-width: 1600px; }
  table { font-size: 0.9rem; }
  pre { font-size: 0.85rem; padding: 1rem; }
  .theme-toggle { cursor: pointer; background: none; border: none; font-size: 1.2rem; padding: 0.3rem; }
  .meta { font-size: 0.78rem; color: var(--pico-muted-color); margin-bottom: 2rem; }
" footer-css))

(defn howto-page [body-html]
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
        [:meta {:name "description" :content bark-description}]
        [:meta {:property "og:title" :content title}]
        [:meta {:property "og:description" :content bark-description}]
        [:meta {:property "og:type" :content "website"}]
        [:link {:rel "stylesheet" :href pico-cdn}]
        [:title title]
        [:style (h/raw howto-css)]]
       [:body
        [:main.container
         (nav-bar title "howto")
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

(defn- parse-since-map
  "Parse :roles/maintainer-since entries into {lower-email -> \"yyyy-MM-dd\"}."
  [roles]
  (let [entries (let [v (:roles/maintainer-since roles)]
                  (cond (nil? v) #{} (string? v) #{v} :else (set v)))]
    (into {}
          (keep (fn [entry]
                  (let [idx (str/last-index-of entry ":")]
                    (when (and idx (pos? idx))
                      [(subs entry 0 idx) (subs entry (inc idx))]))))
          entries)))

(defn build-maintainers-html
  "Build an HTML section listing admin and maintainers by display name,
  with since-dates when available. Only current maintainers are shown."
  [db source-name source-cfg]
  (when source-name
    (let [dp          (resolve 'pod.huahaiy.datalevin/pull)
          admin-email (:admin source-cfg)
          roles       (dp db '[:roles/admin :roles/maintainers :roles/maintainer-since]
                          [:roles/source source-name])
          maint-v     (:roles/maintainers roles)
          maint-emails (cond (nil? maint-v) []
                             (string? maint-v) [maint-v]
                             :else maint-v)
          since-map   (parse-since-map roles)
          entries     (mapv (fn [email]
                              (let [name  (or (contributor-name db source-name email)
                                              email)
                                    since (get since-map (str/lower-case email))]
                                (if since
                                  (str name " <small>(since " since ")</small>")
                                  name)))
                            maint-emails)]
      (when (seq entries)
        (str "<h2 id=\"maintainers\">Maintainers</h2>\n<ul>\n"
             (str/join "\n" (map #(str "<li>" % "</li>") entries))
             "\n</ul>")))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(let [{:keys [out-file out-dir source-name]} (parse-cli-args *command-line-args*)
      config      (load-config)
      source-map  (when config (build-source-map config))
      source-cfg  (get source-map source-name)
      labels      (if source-cfg (howto-labels source-cfg) default-labels)
      triggers    (if source-cfg (howto-triggers source-cfg) default-triggers)
      out-file    (or out-file
                      (if source-name
                        (str "public/" source-name "/web/howto.html")
                        "public/web/howto.html"))
      ;; Infer out-dir from out-file when not given explicitly
      effective-dir (or out-dir
                       (.getParent (clojure.java.io/file out-file)))
      ;; Load DB for maintainer names
      db-path     (or (System/getenv "BARK_DB") "data/bark-db")
      _           (load-datalevin-pod!)
      conn        ((resolve 'pod.huahaiy.datalevin/get-conn) db-path bark-schema {:wal? false})
      db          ((resolve 'pod.huahaiy.datalevin/db) conn)
      maint-html  (build-maintainers-html db source-name source-cfg)
      org-text    (-> (slurp "resources/howto-tpl.org")
                      (substitute-template labels triggers)
                      (filter-feed-links effective-dir))
      body-html   (cond-> (org->html org-text)
                    maint-html (str "\n" maint-html))
      html        (howto-page body-html)]
  ((resolve 'pod.huahaiy.datalevin/close) conn)
  (.mkdirs (.getParentFile (clojure.java.io/file out-file)))
  (spit out-file html)
  (binding [*out* *err*]
    (log/info "Wrote" out-file)))
