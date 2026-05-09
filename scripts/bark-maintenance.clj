#!/usr/bin/env bb

;; bark-maintenance.clj — Purge orphan emails from the BARK database.
;;
;; An orphan email is one that is not referenced by any report (directly
;; or as a descendant/command), and was not sent by a maintainer.
;;
;; IMPORTANT: Run this only when the JVM daemon is NOT running, to avoid
;; write collisions on the Datalevin database.
;;
;; Usage:
;;   bb maintenance                  — dry run, show orphan counts
;;   bb maintenance --delete         — actually delete orphan emails
;;   bb maintenance -n my-source     — scope to a single source
;;   bb maintenance --verbose        — list individual orphan message-ids
;;   bb maintenance --failures       — list recent command failures
;;   bb maintenance --retention DUR  — orphan retention (default: 90d).
;;                                     Accepts "30d", "6m", "1y" or ISO date.
;;
;; Environment:
;;   BARK_DB — path to db (default: ./data/bark-db)

(require '[clojure.string :as str]
         '[taoensso.timbre :as log]
         '[bark.common :refer [parse-delay parse-cutoff-date
                               load-config db-path build-source-map
                               bark-schema format-date
                               failures-file-path read-failures-file
                               reason-labels]]
         '[bark.common-bb :refer [load-datalevin-pod! dq get-tenures]])

;; ---------------------------------------------------------------------------
;; CLI parsing
;; ---------------------------------------------------------------------------

(defn- parse-args [args]
  (loop [opts {:delete? false :verbose? false :failures? false}
         [a & [v & r :as more]] args]
    (cond
      (nil? a)               opts
      (= a "--delete")       (recur (assoc opts :delete? true) more)
      (= a "--verbose")      (recur (assoc opts :verbose? true) more)
      (= a "--failures")     (recur (assoc opts :failures? true) more)
      (#{"-n" "--source"} a) (if v (recur (assoc opts :source-name v) r) opts)
      (= a "--retention")    (if v (recur (assoc opts :retention v) r) opts)
      :else                  (recur opts more))))

;; ---------------------------------------------------------------------------
;; Protected email collection
;; ---------------------------------------------------------------------------

(defn- report-referenced-eids
  "All email entity IDs reachable from any report via any ref attribute.
  This covers :report/email, :report/descendants, :report/acked,
  :report/closed, :report/related, etc.
  If it's an email entity pointed to by a report, it's protected."
  [db]
  (set (dq '[:find [?e ...]
             :where
             [?r :report/type _]
             [?r _ ?e]
             [?e :email/message-id _]]
           db)))

(defn- maintainer-addresses
  "Union of all addresses that ever held maintainer status on any source,
  including closed tenures — these are still 'privileged' for the purpose
  of orphan detection (we don't want to delete emails from a former
  maintainer just because their tenure was closed)."
  [db _config source-map]
  (->> source-map
       (mapcat (fn [[src-name _]]
                 (keep :email (get-tenures db src-name))))
       (map str/lower-case)
       (into #{})))

(defn- all-emails
  "All email entities as [eid source author-address date-sent message-id]."
  [db]
  (dq '[:find ?e ?src ?from ?date ?mid
         :where
         [?e :email/source ?src]
         [?e :email/author-address ?from]
         [?e :email/date-sent ?date]
         [?e :email/message-id ?mid]]
       db))

;; ---------------------------------------------------------------------------
;; Orphan detection
;; ---------------------------------------------------------------------------

(defn- find-orphans
  "Returns a seq of {:eid :source :from :date :mid} for orphan emails."
  [db config source-map {:keys [source-name retention]}]
  (let [protected   (report-referenced-eids db)
        maintainers (maintainer-addresses db config source-map)
        parsed      (when retention (parse-cutoff-date retention))
        _           (when (and retention (nil? parsed))
                      (log/error "Invalid --retention value:" (pr-str retention)
                                 "(expected duration like \"90d\", \"6m\", \"1y\""
                                 "or ISO date \"yyyy-MM-dd\")")
                      (System/exit 1))
        cutoff-date (or parsed
                        (java.util.Date. (- (System/currentTimeMillis) (* 90 24 60 60 1000))))
        emails      (all-emails db)]
    (log/info "Total emails in DB:" (count emails))
    (log/info "Protected by reports:" (count protected))
    (log/info "Maintainer addresses:" (count maintainers))
    (log/info "Orphan retention cutoff:" cutoff-date)
    (->> emails
         (remove (fn [[eid _ _ _ _]] (contains? protected eid)))
         (remove (fn [[_ _ from _ _]]
                   (contains? maintainers (str/lower-case (or from "")))))
         (filter (fn [[_ src _ _ _]]
                   (if source-name (= src source-name) true)))
         (filter (fn [[_ _ _ date _]]
                   (and date (.before ^java.util.Date date cutoff-date))))
         (mapv (fn [[eid src from date mid]]
                 {:eid eid :source src :from from :date date :mid mid})))))

;; ---------------------------------------------------------------------------
;; Command failures
;; ---------------------------------------------------------------------------

(defn- show-failures
  "Display command failures from the failures file, optionally filtered by source."
  [source-name]
  (let [all      (read-failures-file failures-file-path)
        failures (->> all
                      (filter #(or (nil? source-name) (= source-name (:source %))))
                      (sort-by :date #(compare %2 %1)))]
    (if (empty? failures)
      (log/info "No command failures found.")
      (do
        (log/info (count failures) "command failure(s)")
        (doseq [{:keys [from source date reason command]} failures]
          (println (str "  " (format-date date)
                        " | " source
                        " | " from
                        " | " command
                        " — " (get reason-labels reason (name reason)))))))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(load-datalevin-pod!)

(let [{:keys [delete? verbose? failures? source-name] :as opts}
      (parse-args *command-line-args*)
      config  (load-config)
      _       (when-not config
                (log/error "No config.edn found")
                (System/exit 1))
      dbp     (db-path config)
      source-map (build-source-map config)
      _       (when (and source-name (not (contains? source-map source-name)))
                (log/error "Unknown source:" source-name)
                (log/error "Available:" (str/join ", " (keys source-map)))
                (System/exit 1))
      ;; Open with WAL for potential writes
      conn    (d/get-conn dbp bark-schema {})]
  (try
    (let [db (d/db conn)]
      (if failures?
        (show-failures source-name)
        (let [orphans (find-orphans db config source-map opts)]
          (if (empty? orphans)
            (log/info "No orphan emails found.")
            (let [by-source (group-by :source orphans)]
              (log/info "Found" (count orphans) "orphan email(s)")
              (doseq [[src os] (sort-by key by-source)]
                (log/info (str "  [" src "] " (count os) " orphan(s)")))
              (when verbose?
                (doseq [{:keys [source mid from date]} (sort-by :date orphans)]
                  (println (str "  " source " | " mid " | " from " | " date))))
              (if delete?
                (do
                  (log/info "Deleting" (count orphans) "orphan email(s)…")
                  (let [tx-data (mapv (fn [{:keys [eid]}]
                                       [:db/retractEntity eid])
                                     orphans)]
                    (d/transact! conn tx-data)
                    (log/info "Done. Deleted" (count orphans) "email(s).")))
                (do
                  (log/info "Dry run — no changes made. Pass --delete to remove.")
                  (log/info "Tip: use --verbose to list individual orphan message-ids."))))))))
    (finally
      (d/close conn))))
