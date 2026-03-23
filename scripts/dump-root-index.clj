;; ---------------------------------------------------------------------------
;; Root index — public/index.html listing all sources
;; ---------------------------------------------------------------------------
;; Add after dump-stats!, before export-source!.
;; Depends on: html-head, footer-css, bark-repo-url, wrap-js, theme-toggle-js
;;             (all from bark-html.clj), plus slugify, json, io, log.

(defn- load-source-meta
  "Read reports/meta.json for a source dir, or nil on failure."
  [base-dir]
  (let [f (io/file base-dir "reports" "meta.json")]
    (when (.exists f)
      (try (json/parse-string (slurp f) true)
           (catch Exception _ nil)))))

(defn dump-root-index!
  "Generate public/index.html listing all exported sources.
  Reads each source's reports/meta.json for summary counts."
  [source-names source-map]
  (let [rows (for [src-name source-names
                   :let [slug     (slugify src-name)
                         base-dir (str "public/" slug)
                         meta     (load-source-meta base-dir)]
                   :when meta]
               {:name         src-name
                :slug         slug
                :total        (or (:total meta) 0)
                :open         (or (:open-count meta) 0)
                :closed       (or (:closed-count meta) 0)
                :list-archive (:list-archive meta)})
        row-html
        (fn [{:keys [name slug total open closed list-archive]}]
          (str "<tr>"
               "<td><a href=\"" slug "/index.html\">" name "</a>"
               (when list-archive
                 (str " <a class=\"archive\" href=\"" list-archive
                      "\" title=\"List archive\">↗</a>"))
               "</td>"
               "<td class=\"num\">" open "</td>"
               "<td class=\"num\">" closed "</td>"
               "<td class=\"num\">" total "</td>"
               "<td class=\"num feeds\">"
               "<a href=\"" slug "/reports/all.xml\">RSS</a> · "
               "<a href=\"" slug "/reports/all.json\">JSON</a>"
               "</td>"
               "</tr>\n"))
        page
        (str
         "<!DOCTYPE html>\n<html lang=\"en\">\n"
         (html-head {:title "BARK — Sources"
                     :css (str "table{margin-top:1.5rem}"
                               "td.num,th.num{text-align:right}"
                               "a.archive{font-size:0.82rem;margin-left:0.4rem;opacity:0.7}"
                               ".feeds{font-size:0.82rem;white-space:nowrap}"
                               footer-css)})
         "<body>\n<main class=\"container\">\n"
         "<nav><ul><li><strong>BARK</strong></li></ul>"
         "<ul><li><button class=\"theme-toggle\" onclick=\"toggleTheme()\" "
         "aria-label=\"Toggle theme\"><span id=\"theme-icon\">🌙</span>"
         "</button></li></ul></nav>\n"
         "<table role=\"grid\">\n"
         "<thead><tr>"
         "<th>Source</th>"
         "<th class=\"num\">Open</th>"
         "<th class=\"num\">Closed</th>"
         "<th class=\"num\">Total</th>"
         "<th class=\"num\">Feeds</th>"
         "</tr></thead>\n<tbody>\n"
         (apply str (map row-html rows))
         "</tbody></table>\n"
         "<footer class=\"bark-footer\">"
         "<a href=\"" bark-repo-url "\">BARK</a> is "
         "<a href=\"https://www.gnu.org/philosophy/free-sw.html\">Free Software</a>"
         "</footer>\n"
         "<script>\n" (wrap-js theme-toggle-js) "\n</script>\n"
         "</main>\n</body>\n</html>\n")]
    (spit "public/index.html" page)
    (log/info "Wrote public/index.html with" (count rows) "source(s)")))

;; ---------------------------------------------------------------------------
;; Integration: add one line in the main `let` block, after the `doseq`
;; that calls export-source! and before (save-last-export! ...):
;;
;;   (dump-root-index! source-names source-map)
;;
;; Also add "root" as an optional format so you can run:
;;   bb export root    — regenerate only public/index.html
;; ---------------------------------------------------------------------------
