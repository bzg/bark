;; bark-common.clj — Shared utilities for bark bb scripts.
;;
;; Usage: (load-file "scripts/bark-common.clj")

(require '[clojure.string :as str]
         '[clojure.edn :as edn])

(defn load-config
  "Load config.edn if it exists, or nil."
  []
  (let [f (clojure.java.io/file "config.edn")]
    (when (.exists f) (edn/read-string (slurp f)))))

(defn get-header
  "Case-insensitive header lookup. headers-edn can be an EDN string or
  an already-parsed map. Returns nil on parse failure."
  [headers-edn header-name]
  (when headers-edn
    (try
      (let [headers (if (string? headers-edn) (edn/read-string headers-edn) headers-edn)
            lname   (str/lower-case header-name)]
        (some (fn [[k v]] (when (= (str/lower-case k) lname) v)) headers))
      (catch Exception _ nil))))

(defn- match-source?
  "Check if headers match a source's :match spec (substring, case-insensitive)."
  [headers-edn match-spec]
  (every? (fn [[k v]]
            (let [header-name (case k
                                :list-id      "List-Id"
                                :delivered-to "Delivered-To"
                                :to           "To"
                                (name k))
                  header-val  (get-header headers-edn header-name)]
              (and header-val
                   (str/includes? (str/lower-case (str header-val))
                                  (str/lower-case v)))))
          match-spec))

(defn classify-source
  "Return the :name of the first matching source, or nil.
  A source with no :match acts as a catch-all."
  [headers-edn sources]
  (some (fn [{:keys [name match]}]
          (when (or (nil? match) (empty? match)
                    (match-source? headers-edn match))
            name))
        sources))

(defn build-source-map
  "Build source-name → {:admin :mailing-list-email :triggers} from config."
  [config]
  (let [default-admin (:admin config)]
    (into {}
          (map (fn [src]
                 [(:name src)
                  (cond-> {:admin (or (:admin src) default-admin)}
                    (:mailing-list-email src)
                    (assoc :mailing-list-email (:mailing-list-email src))
                    (:triggers src)
                    (assoc :triggers (:triggers src)))]))
          (:sources config))))

;; ---------------------------------------------------------------------------
;; Report scoring (shared by bark-egest and bark-notify)
;; ---------------------------------------------------------------------------

(defn report-priority [report]
  (+ (if (:report/urgent report) 2 0)
     (if (:report/important report) 1 0)))

(defn report-status
  "Compute a numeric status score for filtering.
  Higher = more active: open (4) > closed (0), +2 if owned, +1 if acked.
  E.g. --min-status 4 filters to open reports only."
  [report]
  (+ (if-not (:report/closed report) 4 0)
     (if (:report/owned report) 2 0)
     (if (:report/acked report) 1 0)))

(defn report-descendant-count [report]
  (let [d (:report/descendants report)]
    (cond (sequential? d) (count d)
          (map? d)        1
          :else           0)))
