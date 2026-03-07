#!/usr/bin/env bb

;; test/bark-email-test.clj — Test SMTP configuration by sending a test email.
;;
;; Reads :notifications :smtp from config.edn, sends a short test message
;; to the global :admin address (or to a custom address via --to).
;;
;; Usage:
;;   bb test-smtp                    — send test email to :admin
;;   bb test-smtp --to me@example.com — send to a specific address
;;   bb test-smtp --dry-run          — validate config without sending

(require '[babashka.pods :as pods]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(load-file "scripts/bark-common.clj")

;; ---------------------------------------------------------------------------
;; Config validation
;; ---------------------------------------------------------------------------

(defn- check-key [m k label]
  (when-not (get m k)
    (println (str "  ✗ Missing " label " (" (name k) ")"))
    true))

(defn validate-smtp-config
  "Validate that all required SMTP keys are present. Returns true if valid."
  [smtp]
  (let [required [[:host "SMTP host"]
                  [:port "SMTP port"]
                  [:user "SMTP user"]
                  [:password "SMTP password"]
                  [:from "From address"]]
        errors (keep (fn [[k label]] (check-key smtp k label)) required)]
    (if (seq errors)
      (do (println (str "\n" (count errors) " missing SMTP field(s)."))
          false)
      (do (println "  ✓ All required SMTP fields present")
          (println (str "    host: " (:host smtp)))
          (println (str "    port: " (:port smtp)))
          (println (str "    tls:  " (boolean (:tls smtp))))
          (println (str "    user: " (:user smtp)))
          (println (str "    from: " (:from smtp)))
          true))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(when (= (System/getProperty "babashka.file") *file*)
  (let [args     *command-line-args*
        dry-run? (some #{"--dry-run"} args)
        to-idx   (some #(when (= "--to" (nth args % nil)) %) (range (count args)))
        to-arg   (when to-idx (nth args (inc to-idx) nil))
        config   (load-config)]

    (println "== BARK SMTP test ==\n")

    ;; --- Check config.edn ---
    (when-not config
      (println "✗ config.edn not found.")
      (System/exit 1))

    (let [notif (:notifications config)]
      (when-not notif
        (println "✗ No :notifications key in config.edn.")
        (System/exit 1))
      (when-not (:enabled notif)
        (println "⚠ Notifications are disabled (:enabled false). Testing anyway."))

      ;; --- Validate SMTP fields ---
      (println "Checking SMTP config...")
      (let [smtp (:smtp notif)]
        (when-not smtp
          (println "✗ No :smtp key under :notifications.")
          (System/exit 1))
        (when-not (validate-smtp-config smtp)
          (System/exit 1))

        ;; --- Resolve recipient ---
        (let [to-addr (or to-arg (:admin config))]
          (when-not to-addr
            (println "\n✗ No recipient: set :admin in config.edn or use --to <addr>.")
            (System/exit 1))

          (println (str "\nRecipient: " to-addr))

          (if dry-run?
            (println "\n✓ Dry run — config looks good, no email sent.")

            ;; --- Send test email ---
            (do
              (println "Sending test email...")
              (pods/load-pod 'tzzh/mail "0.0.3")
              (require '[pod.tzzh.mail :as mail])

              (let [body (str "This is a test email from BARK.\n\n"
                              "If you received this, your SMTP configuration is working.\n\n"
                              "Timestamp: " (java.util.Date.) "\n"
                              "Host: " (:host smtp) "\n"
                              "From: " (:from smtp) "\n")]
                (try
                  ((resolve 'pod.tzzh.mail/send-mail)
                   {:host     (:host smtp)
                    :port     (:port smtp)
                    :tls      (boolean (:tls smtp))
                    :username (:user smtp)
                    :password (:password smtp)
                    :from     (:from smtp)
                    :to       [to-addr]
                    :subject  "[BARK] SMTP test"
                    :text     body})
                  (println (str "\n✓ Test email sent to " to-addr "."))
                  (catch Exception e
                    (println (str "\n✗ Failed to send: " (or (.getMessage e) (pr-str e))))
                    (System/exit 1)))))))))))
