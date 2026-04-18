#!/usr/bin/env bb

;; bark-rebuild-history.clj — Replay a mail archive across multiple
;; static configs, one per era.  Each config is run in batch mode with
;; its :fetch window overridden by the era's :since/:until bounds,
;; producing a historically consistent ingest for archives whose
;; command vocabulary or syntax mode changed over time.
;;
;; Usage:
;;   bb rebuild-history                  — run the plan in configs/history.edn
;;   bb rebuild-history --history PATH   — use a different history file
;;   bb rebuild-history --fresh          — wipe the DB first (interactive confirm)
;;   bb rebuild-history --dry-run        — validate only, no execution

(require '[babashka.process :as p]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.pprint :as pp]
         '[clojure.string :as str]
         '[bark.common :refer [bark-schema]]
         '[bark.common-bb :refer [load-datalevin-pod! dq]]
         '[taoensso.timbre :as log])

(log/merge-config! {:min-level :info})

;; ---------------------------------------------------------------------------
;; CLI parsing
;; ---------------------------------------------------------------------------

(defn- parse-args [args]
  (loop [opts {:history "configs/history.edn" :fresh? false :dry-run? false}
         [a & r] args]
    (cond
      (nil? a)          opts
      (= a "--fresh")   (recur (assoc opts :fresh? true) r)
      (= a "--dry-run") (recur (assoc opts :dry-run? true) r)
      (= a "--history") (if (seq r)
                          (recur (assoc opts :history (first r)) (rest r))
                          (do (log/error "--history requires a PATH argument")
                              (System/exit 1)))
      :else             (do (log/error "Unknown arg:" a)
                            (System/exit 1)))))

;; ---------------------------------------------------------------------------
;; Validation
;; ---------------------------------------------------------------------------

(defn- iso-date? [s]
  (and (string? s) (re-matches #"\d{4}-\d{2}-\d{2}" s)))

(defn- validate-entry [idx entry]
  (cond-> []
    (not (map? entry))
    (conj (str "entry " idx ": not a map — " (pr-str entry)))
    (and (map? entry) (not (:config entry)))
    (conj (str "entry " idx ": missing :config"))
    (and (:config entry) (not (.exists (io/file (:config entry)))))
    (conj (str "entry " idx ": config file not found — " (:config entry)))
    (and (:since entry) (not (iso-date? (:since entry))))
    (conj (str "entry " idx ": :since not ISO yyyy-MM-dd — " (pr-str (:since entry))))
    (and (:until entry) (not (iso-date? (:until entry))))
    (conj (str "entry " idx ": :until not ISO yyyy-MM-dd — " (pr-str (:until entry))))
    (and (iso-date? (:since entry)) (iso-date? (:until entry))
         (not (neg? (compare (:since entry) (:until entry)))))
    (conj (str "entry " idx ": :since must be strictly before :until"))))

(defn- validate-contiguity [entries]
  (->> (partition 2 1 (map-indexed vector entries))
       (reduce (fn [errs [[ai a] [bi b]]]
                 (cond
                   (nil? (:until a))
                   (conj errs (str "entry " ai ": missing :until "
                                   "(only the last entry may omit it)"))
                   (nil? (:since b))
                   (conj errs (str "entry " bi ": missing :since "
                                   "(only the first entry may omit it)"))
                   (not= (:until a) (:since b))
                   (conj errs (str "gap/overlap between entries " ai " and " bi ": "
                                   ":until " (:until a)
                                   " ≠ :since " (:since b)))
                   :else errs))
               [])))

(defn- validate-history [entries]
  (let [entry-errs (into [] (mapcat validate-entry (range) entries))
        cont-errs  (if (>= (count entries) 2)
                     (validate-contiguity entries)
                     [])]
    (seq (concat entry-errs cont-errs))))

;; ---------------------------------------------------------------------------
;; Config injection
;; ---------------------------------------------------------------------------

(defn- merge-fetch-window
  "Return `cfg` with :ingest :fetch overridden to reflect the era's
  window.  An empty window ({}) means \"no bound\" — mailseq fetches
  everything the source has."
  [cfg {:keys [since until]}]
  (let [window (cond-> {}
                 since (assoc :since since)
                 until (assoc :until until))]
    (assoc-in cfg [:ingest :fetch] window)))

(defn- write-temp-config!
  "Write `cfg` to a temp EDN file and return its absolute path.
  The file is marked for deletion on JVM exit."
  [cfg]
  (let [f (java.io.File/createTempFile "bark-history-" ".edn")]
    (.deleteOnExit f)
    (spit f (with-out-str (pp/pprint cfg)))
    (.getAbsolutePath f)))

;; ---------------------------------------------------------------------------
;; DB snapshot (email + report counts) via Datalevin pod
;; ---------------------------------------------------------------------------

(defn- db-snapshot
  "Return {:emails N :reports N}.  Opens and closes the pod connection
  around each snapshot so the subsequent subprocess can acquire the
  Datalevin lock."
  [db-path]
  (if-not (.exists (io/file db-path))
    {:emails 0 :reports 0}
    (let [d-conn  (resolve 'pod.huahaiy.datalevin/get-conn)
          d-db    (resolve 'pod.huahaiy.datalevin/db)
          d-close (resolve 'pod.huahaiy.datalevin/close)
          conn    (d-conn db-path bark-schema {:wal? false})
          db      (d-db conn)
          emails  (or (ffirst (dq '[:find (count ?e)
                                    :where [?e :email/id _]] db)) 0)
          reports (or (ffirst (dq '[:find (count ?r)
                                    :where [?r :report/type _]] db)) 0)]
      (d-close conn)
      {:emails emails :reports reports})))

;; ---------------------------------------------------------------------------
;; Fresh mode — wipe the DB
;; ---------------------------------------------------------------------------

(defn- confirm! [prompt]
  (print prompt) (flush)
  (let [line (read-line)]
    (boolean (#{"y" "Y" "yes" "YES"} (some-> line str/trim)))))

(defn- delete-recursively! [^java.io.File f]
  (when (.isDirectory f)
    (doseq [child (.listFiles f)]
      (delete-recursively! child)))
  (.delete f))

(defn- wipe-db! [db-path]
  (let [f (io/file db-path)]
    (when (.exists f)
      (log/info "Wiping" (.getAbsolutePath f))
      (delete-recursively! f))))

;; ---------------------------------------------------------------------------
;; Execution
;; ---------------------------------------------------------------------------

(defn- run-era!
  "Run one entry: load its config, inject the window, spawn
  `clojure -M:run -c <tmpfile>`.  Returns the process result."
  [{:keys [config] :as entry}]
  (let [raw    (edn/read-string (slurp config))
        merged (merge-fetch-window raw entry)
        tmp    (write-temp-config! merged)]
    (log/info "─── Running" config
              (str "window="
                   (or (:since entry) "-") ".." (or (:until entry) "-")))
    (p/shell {:continue true} "clojure" "-M:run" "-c" tmp)))

(defn- summarize! [label {:keys [emails reports] :as _snap}]
  (log/info (format "  %s: %d emails, %d reports" label emails reports)))

(defn- run-history!
  "Iterate through `entries`, snapshotting the DB between runs so a
  final per-era summary can be logged."
  [entries db-path]
  (load-datalevin-pod!)
  (let [start (db-snapshot db-path)
        acc   (loop [remaining entries
                     prev      start
                     acc       []]
                (if (empty? remaining)
                  acc
                  (let [entry  (first remaining)
                        result (run-era! entry)]
                    (when-not (zero? (:exit result))
                      (log/error "Run failed with exit" (:exit result)
                                 "— aborting history rebuild")
                      (System/exit (:exit result)))
                    (let [snap (db-snapshot db-path)]
                      (log/info (format "  +%d emails, +%d reports"
                                        (- (:emails snap) (:emails prev))
                                        (- (:reports snap) (:reports prev))))
                      (recur (rest remaining) snap (conj acc [entry snap]))))))]
    (log/info "─── Summary")
    (summarize! "baseline" start)
    (reduce (fn [prev [entry snap]]
              (log/info (format "  %s [%s..%s]: +%d emails, +%d reports"
                                (:config entry)
                                (or (:since entry) "-")
                                (or (:until entry) "-")
                                (- (:emails snap) (:emails prev))
                                (- (:reports snap) (:reports prev))))
              snap)
            start
            acc)
    (summarize! "final" (or (some-> acc last second) start))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn- print-plan [entries]
  (log/info "Planned runs:")
  (doseq [[idx e] (map-indexed vector entries)]
    (log/info (format "  %d. %s  [%s..%s]"
                      idx
                      (:config e)
                      (or (:since e) "-")
                      (or (:until e) "-")))))

(defn -main [& args]
  (let [{:keys [history fresh? dry-run?]} (parse-args args)
        history-file (io/file history)]
    (when-not (.exists history-file)
      (log/error "History file not found:" history)
      (System/exit 1))
    (let [entries (edn/read-string (slurp history-file))]
      (when-not (and (vector? entries) (seq entries))
        (log/error "History file must contain a non-empty vector of entries:" history)
        (System/exit 1))
      (when-let [errs (validate-history entries)]
        (log/error "History validation failed:")
        (doseq [e errs] (log/error " -" e))
        (System/exit 1))
      (print-plan entries)
      (when dry-run?
        (log/info "Dry run — exiting without execution.")
        (System/exit 0))
      (let [first-cfg (edn/read-string (slurp (:config (first entries))))
            db-path   (get-in first-cfg [:db :path])]
        (when-not db-path
          (log/error "First config has no :db :path")
          (System/exit 1))
        (when fresh?
          (if (confirm! (str "Wipe DB at " db-path "? [y/N] "))
            (wipe-db! db-path)
            (do (log/info "Aborted.") (System/exit 0))))
        (run-history! entries db-path)))))

(apply -main *command-line-args*)
