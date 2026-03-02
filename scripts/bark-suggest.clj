#!/usr/bin/env bb

;; bark-suggest.clj — Display BARK reports interactively.
;;
;; Standalone CLI tool: reads JSON produced by bark-egest from a file,
;; a URL, or stdin.  Displays via gum table (with detail on selection)
;; or plain text lines when gum is not available.
;;
;; Usage:
;;   bark-suggest.clj [options] [type]
;;
;; Options:
;;   -f, --file FILE   Read reports from a JSON file
;;   -u, --url  URL    Fetch reports from a URL
;;   -                 Read JSON from stdin
;;
;; Type (optional filter):
;;   bugs | patches | requests | announcements | releases | changes
;;
;; Examples:
;;   bb suggest -f reports.json
;;   bb suggest -f reports.json bugs
;;   bb suggest -u https://example.com/reports.json patches
;;   curl … | bb suggest -
;;   bb reports-json && bb suggest -f reports.json

(require '[babashka.deps :as deps]
         '[babashka.process :as process]
         '[babashka.http-client :as http]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[clojure.pprint :as pprint])

(deps/add-deps '{:deps {io.github.lispyclouds/bblgum
                         {:git/sha "881a426d9df9df40eb305ceaeb3996ea1c7ae0d3"}}})

(require '[bblgum.core :as gum])

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
;; Filtering
;; ---------------------------------------------------------------------------

(def ^:private type-aliases
  {"bugs"          "bug"
   "patches"       "patch"
   "requests"      "request"
   "announcements" "announcement"
   "releases"      "release"
   "changes"       "change"})

(defn- filter-by-type [reports type-str]
  (if-let [t (type-aliases type-str)]
    (filter #(= (:type %) t) reports)
    reports))

;; ---------------------------------------------------------------------------
;; Formatting helpers
;; ---------------------------------------------------------------------------

(defn- report->row
  "Format a report as a tab-separated row for gum table."
  [report show-type?]
  (str/join "\t"
            (concat
             (when show-type? [(:type report "")])
             [(:flags report "-----")
              (str (:replies report 0))
              (:from report "?")
              (:date report "")
              (:subject report "(no subject)")])))

(defn- extra-str [report]
  (let [parts (remove nil?
                [(:version report)
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
  [report show-type?]
  (str (when show-type? (format "[%-12s] " (:type report "")))
       (format "%-5s %3d %-25s %s  %s"
               (:flags report "-----")
               (:replies report 0)
               (:from report "?")
               (:date report "")
               (:subject report "(no subject)"))
       (when-let [e (extra-str report)] (str " " e))))

;; ---------------------------------------------------------------------------
;; Display
;; ---------------------------------------------------------------------------

(defn- gum-available? []
  (try
    (zero? (:exit (process/shell {:out :string :err :string :continue true} "gum" "--version")))
    (catch Exception _ false)))

(defn display-reports!
  "Display reports interactively with gum table, or as plain text lines."
  [reports show-type?]
  (if (empty? reports)
    (println "No reports found.")
    (if (gum-available?)
      (let [columns (concat
                     (when show-type? ["Type"])
                     ["Flags" "#" "From" "Date" "Subject"])
            rows    (mapv #(report->row % show-type?) reports)
            input   (str/join "\n" rows)
            {:keys [status result]}
            (gum/gum :table
                     :in input
                     :columns (str/join "," columns)
                     :separator "\t")]
        (when (and (zero? status) (seq result))
          (let [selected (first result)
                idx      (.indexOf ^java.util.List rows selected)]
            (when (>= idx 0)
              (pprint/pprint (nth reports idx))))))
      ;; Plain text fallback
      (do (println (str (count reports) " report(s):\n"))
          (doseq [r reports]
            (println (str "  " (report->line r show-type?))))))))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(defn- usage []
  (println "Usage: bark-suggest.clj [options] [type]")
  (println "")
  (println "Options:")
  (println "  -f, --file FILE   Read reports from a JSON file")
  (println "  -u, --url  URL    Fetch reports from a URL")
  (println "  -                 Read JSON from stdin")
  (println "")
  (println "Type: bugs | patches | requests | announcements | releases | changes"))

(let [args *command-line-args*]
  (if (or (empty? args) (some #{"-h" "--help"} args))
    (usage)
    (let [[opts remaining]
          (loop [opts {} [a & more :as remaining] args]
            (cond
              (nil? a)                       [opts nil]
              (= a "-")                      [(assoc opts :source :stdin) more]
              (#{"-f" "--file"} a)            (recur (assoc opts :source :file :path (first more)) (rest more))
              (#{"-u" "--url"} a)             (recur (assoc opts :source :url  :url  (first more)) (rest more))
              :else                          [opts remaining]))
          type-filter (first remaining)
          reports     (case (:source opts)
                        :file  (load-from-file (:path opts))
                        :url   (load-from-url (:url opts))
                        :stdin (load-from-stdin)
                        (do (println "Error: specify -f FILE, -u URL, or - for stdin.")
                            (System/exit 1)))
          filtered    (if type-filter
                        (filter-by-type reports type-filter)
                        reports)
          show-type?  (nil? type-filter)]
      (display-reports! filtered show-type?))))
