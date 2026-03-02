;; bark-common.clj — Shared utilities for bark bb scripts.
;;
;; Usage: (load-file "scripts/bark-common.clj")

(require '[clojure.string :as str]
         '[clojure.edn :as edn])

(defn mailbox-id
  "Compute a stable mailbox identifier from an IMAP config map."
  [mb]
  (str (:host mb) ":" (:user mb)))

(defn load-config
  "Load config.edn if it exists, or nil."
  []
  (let [f (clojure.java.io/file "config.edn")]
    (when (.exists f) (edn/read-string (slurp f)))))

(defn build-mailbox-map
  "Build mailbox-id → {:email :admin :name :mailing-list-email} from config."
  [config]
  (let [default-admin (:admin config)]
    (into {}
          (map (fn [mb]
                 [(mailbox-id mb)
                  (cond-> {:email (:email mb)
                           :admin (or (:admin mb) default-admin)}
                    (:name mb)               (assoc :name (:name mb))
                    (:mailing-list-email mb)  (assoc :mailing-list-email (:mailing-list-email mb)))]))
          (:mailboxes config))))

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
