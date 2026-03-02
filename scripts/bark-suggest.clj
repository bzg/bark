#!/usr/bin/env bb

;; bark-suggest.clj — Display BARK reports interactively.
;;
;; Standalone CLI tool: reads JSON produced by bark-egest from a file,
;; a URL, or stdin.  Displays via fzf (with detail on selection)
;; or plain text lines when fzf is not available.
;;
;; Usage:
;;   bark-suggest.clj [options]
;;
;; Options:
;;   -f, --file FILE   Read reports from a JSON file
;;   -u, --url  URL    Fetch reports from a URL
;;   -                 Read JSON from stdin
;;
;; Examples:
;;   bb suggest -f reports.json
;;   bb suggest -u https://example.com/reports.json
;;   curl … | bb suggest -

(require '[babashka.process :as process]
         '[babashka.http-client :as http]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[clojure.pprint :as pprint])

;; ---------------------------------------------------------------------------
;; Data loading
;; ---------------------------------------------------------------------------

(defn- load-json-string [s]
  (json/parse-string s keyword))

(defn- load-from-file [path]
  (when-not (.exists (clojure.java.io/file path))
    (binding [*out* *err*]
      (println (str "File not found: " path)))
    (System/exit 1))
  (load-json-string (slurp path)))

(defn- load-from-url [url]
  (let [resp (http/get url {:headers {"Accept" "application/json"}})]
    (when (not= 200 (:status resp))
      (binding [*out* *err*]
        (println (str "HTTP " (:status resp) " fetching " url)))
      (System/exit 1))
    (load-json-string (:body resp))))

(defn- load-from-stdin []
  (load-json-string (slurp *in*)))

;; ---------------------------------------------------------------------------
;; Formatting helpers
;; ---------------------------------------------------------------------------

(defn- has-mailbox? [reports]
  (some :mailbox reports))

(defn- report->row
  "Format a report as a tab-separated row for gum table."
  [report show-type? show-mb?]
  (str/join "\t"
            (concat
             (when show-type? [(:type report "")])
             (when show-mb?   [(:mailbox report "")])
             [(str (:priority report 0))
              (:flags report "---")
              (str (:replies report 0))
              (:from report "?")
              (:date report "")
              (:subject report "(no subject)")])))

(defn- extra-str [report]
  (let [parts (remove nil?
                [(:mailbox report)
                 (:version report)
                 (:topic report)
                 (:patch-seq report)
                 (when-let [sources (seq (:patch-source report))]
                   (str "src:" (str/join "," sources)))
                 (when-let [v (:votes report)] (str "votes:" v))
                 (when-let [s (:series report)]
                   (str "series:" (:received s) "/" (:expected s)
                        (when (:closed s) " closed")))
                 (when-let [related (seq (:related report))]
                   (str "→" (str/join "," (distinct (map :type related)))))])]
    (when (seq parts) (str/join " " parts))))

(defn- report->line
  "Format a report as a plain text line."
  [report show-type? show-mb?]
  (str (when show-type? (format "[%-12s] " (:type report "")))
       (when show-mb?   (format "[%-10s] " (:mailbox report "")))
       (format "%d %-3s %3d %-25s %s  %s"
               (:priority report 0)
               (:flags report "---")
               (:replies report 0)
               (:from report "?")
               (:date report "")
               (:subject report "(no subject)"))
       (when-let [e (extra-str report)] (str " " e))))

;; ---------------------------------------------------------------------------
;; Display
;; ---------------------------------------------------------------------------

(defn- fzf-available? []
  (try
    (zero? (:exit (process/shell {:out :string :err :string :continue true} "fzf" "--version")))
    (catch Exception _ false)))

(defn- tabulate
  "Align tab-separated rows into fixed-width columns using column(1)."
  [rows]
  (-> (process/shell {:in (str/join "\n" rows) :out :string}
                     "column" "-t" "-s" "\t")
      :out
      str/trim
      str/split-lines))

(defn display-reports!
  "Display reports interactively with fzf, or as plain text lines."
  [reports]
  (let [show-type? true
        show-mb?   (has-mailbox? reports)]
    (if (empty? reports)
      (println "No reports found.")
      (if (fzf-available?)
        (let [header (str/join "\t"
                               (concat
                                (when show-type? ["Type"])
                                (when show-mb?   ["Mailbox"])
                                ["P" "Flags" "#" "From" "Date" "Subject"]))
              rows   (mapv #(report->row % show-type? show-mb?) reports)
              aligned      (tabulate (cons header rows))
              header-line  (first aligned)
              aligned-rows (vec (rest aligned))
              input        (str/join "\n" aligned-rows)
              {:keys [exit out]}
              (process/shell {:in input :out :string :continue true}
                             "fzf" "--header" header-line
                             "--no-sort" "--reverse"
                             "--prompt" "report> ")]
          (when (zero? exit)
            (let [selected (str/trim out)
                  idx      (.indexOf ^java.util.List aligned-rows selected)]
              (when (>= idx 0)
                (let [report (nth reports idx)
                      url    (:archived-at report)]
                  (if url
                    (process/shell "xdg-open" url)
                    (do (println "No archived-at URL for this report.")
                        (pprint/pprint report))))))))
        ;; Plain text fallback
        (do (println (str (count reports) " report(s):\n"))
            (doseq [r reports]
              (println (str "  " (report->line r show-type? show-mb?)))))))))
;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(defn- usage []
  (println "Usage: bark-suggest.clj [options]")
  (println "")
  (println "Options:")
  (println "  -f, --file FILE       Read reports from a JSON file")
  (println "  -u, --url  URL        Fetch reports from a URL")
  (println "  -m, --mailbox NAME    Filter by mailbox name")
  (println "  -                     Read JSON from stdin"))

(defn- filter-by-mailbox [reports mb-name]
  (filter #(= (:mailbox %) mb-name) reports))

(let [args *command-line-args*]
  (if (or (empty? args) (some #{"-h" "--help"} args))
    (usage)
    (let [[opts _remaining]
          (loop [opts {} [a & more :as remaining] args]
            (cond
              (nil? a)                       [opts nil]
              (= a "-")                      [(assoc opts :source :stdin) more]
              (#{"-f" "--file"} a)            (recur (assoc opts :source :file :path (first more)) (rest more))
              (#{"-u" "--url"} a)             (recur (assoc opts :source :url  :url  (first more)) (rest more))
              (#{"-m" "--mailbox"} a)         (recur (assoc opts :mailbox (first more)) (rest more))
              :else                          [opts remaining]))
          reports     (case (:source opts)
                        :file  (load-from-file (:path opts))
                        :url   (load-from-url (:url opts))
                        :stdin (load-from-stdin)
                        (do (println "Error: specify -f FILE, -u URL, or - for stdin.")
                            (System/exit 1)))
          reports     (if-let [mb (:mailbox opts)]
                        (filter-by-mailbox reports mb)
                        reports)]
      (display-reports! reports))))
