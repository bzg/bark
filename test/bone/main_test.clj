(ns bone.main-test
  (:require [clojure.test :refer [deftest is testing]]
            [bone.main :as main])
  (:import [java.util Date]))

(def parse-fetch (var-get #'main/parse-fetch))
(def cli-fetch->map (var-get #'main/cli-fetch->map))
(def run-opts (var-get #'main/run-opts))
(def load-context (var-get #'main/load-context))

(deftest parse-fetch-limit
  (testing "{:limit N} with positive integer"
    (is (= {:limit 50} (parse-fetch {:limit 50})))
    (is (= {:limit 1}  (parse-fetch {:limit 1}))))
  (testing ":limit rejects zero and negatives"
    (is (thrown? Exception (parse-fetch {:limit 0})))
    (is (thrown? Exception (parse-fetch {:limit -5}))))
  (testing ":limit rejects non-integer"
    (is (thrown? Exception (parse-fetch {:limit "50"}))))
  (testing ":limit cannot be combined with other keys"
    (is (thrown? Exception (parse-fetch {:limit 50 :since "30d"})))
    (is (thrown? Exception (parse-fetch {:limit 50 :start "2020-01-01"})))))

(deftest parse-fetch-since
  (testing "{:since \"Nd\"} returns {:since Date}"
    (let [{:keys [since]} (parse-fetch {:since "30d"})]
      (is (instance? Date since))))
  (testing ":since accepts all duration units"
    (doseq [u ["5d" "3w" "2m" "1y"]]
      (is (some? (:since (parse-fetch {:since u}))) u)))
  (testing ":since rejects ISO date (must be duration)"
    (is (thrown? Exception (parse-fetch {:since "2020-01-01"}))))
  (testing ":since rejects bare numbers"
    (is (thrown? Exception (parse-fetch {:since "50"})))
    (is (thrown? Exception (parse-fetch {:since 50}))))
  (testing ":since cannot combine with other keys"
    (is (thrown? Exception (parse-fetch {:since "30d" :end "2020-01-01"})))))

(deftest parse-fetch-start-end
  (testing "{:start ISO} translates to mailseq :since"
    (let [{:keys [since before]} (parse-fetch {:start "2020-01-01"})]
      (is (instance? Date since))
      (is (nil? before))))
  (testing "{:end ISO} translates to mailseq :before (alone allowed)"
    (let [{:keys [since before]} (parse-fetch {:end "2022-01-01"})]
      (is (nil? since))
      (is (instance? Date before))))
  (testing "{:start :end} translates to mailseq {:since :before}"
    (let [{:keys [since before]} (parse-fetch {:start "2020-01-01" :end "2022-01-01"})]
      (is (instance? Date since))
      (is (instance? Date before))
      (is (.before since before))))
  (testing ":start/:end reject duration strings"
    (is (thrown? Exception (parse-fetch {:start "30d"})))
    (is (thrown? Exception (parse-fetch {:end "30d"}))))
  (testing ":start must be strictly before :end"
    (is (thrown? Exception (parse-fetch {:start "2022-01-01" :end "2020-01-01"})))
    (is (thrown? Exception (parse-fetch {:start "2020-01-01" :end "2020-01-01"}))))
  (testing "extraneous keys rejected"
    (is (thrown? Exception (parse-fetch {:start "2020-01-01" :foo 1})))))

(deftest parse-fetch-rejects
  (testing "non-map values rejected"
    (is (thrown? Exception (parse-fetch 50)))
    (is (thrown? Exception (parse-fetch "30d")))
    (is (thrown? Exception (parse-fetch "2020-01-01")))
    (is (thrown? Exception (parse-fetch nil)))
    (is (thrown? Exception (parse-fetch [:limit 50]))))
  (testing "empty map rejected"
    (is (thrown? Exception (parse-fetch {})))))

(deftest cli-fetch->map-lifts
  (testing "bare integer → :limit"
    (is (= {:limit 50} (cli-fetch->map "50"))))
  (testing "duration → :since"
    (is (= {:since "30d"} (cli-fetch->map "30d")))
    (is (= {:since "6w"}  (cli-fetch->map "6w"))))
  (testing "ISO date → :start"
    (is (= {:start "2020-01-01"} (cli-fetch->map "2020-01-01"))))
  (testing "invalid rejected"
    (is (thrown? Exception (cli-fetch->map "foo")))
    (is (thrown? Exception (cli-fetch->map "30 days")))))

(deftest cli-fetch-round-trips-through-parse-fetch
  (testing "every CLI shape lifts into a map that parse-fetch accepts"
    (is (= {:limit 50} (parse-fetch (cli-fetch->map "50"))))
    (let [{:keys [since before]} (parse-fetch (cli-fetch->map "30d"))]
      (is (instance? Date since))
      (is (nil? before)))
    (let [{:keys [since before]} (parse-fetch (cli-fetch->map "2020-01-01"))]
      (is (instance? Date since))
      (is (nil? before)))))

(def ^:private imap-mb
  {:name "primary" :type :imap :host "imap.example.com"
   :user "me@example.com" :password "secret"})

(def ^:private maildir-mb
  {:name "forge" :type :maildir :path "/tmp/mail/forge"})

(deftest check-mailboxes-accepts-valid-vector
  (testing "single mailbox"
    (let [r (main/check-mailboxes {:mailboxes [imap-mb]})]
      (is (= [imap-mb] (:ok r)))
      (is (nil? (:error r)))))
  (testing "mixed IMAP and Maildir"
    (let [r (main/check-mailboxes {:mailboxes [imap-mb maildir-mb]})]
      (is (= [imap-mb maildir-mb] (:ok r))))))

(deftest check-mailboxes-rejects-singleton
  (testing ":mailbox singleton is no longer accepted"
    (let [r (main/check-mailboxes {:mailbox imap-mb})]
      (is (nil? (:ok r)))
      (is (re-find #":mailbox is no longer accepted" (:error r)))))
  (testing ":mailbox is rejected even if :mailboxes is also present"
    (let [r (main/check-mailboxes {:mailbox imap-mb :mailboxes [imap-mb]})]
      (is (re-find #":mailbox is no longer accepted" (:error r))))))

(deftest check-mailboxes-rejects-bad-shape
  (testing "missing :mailboxes"
    (is (re-find #"non-empty vector" (:error (main/check-mailboxes {})))))
  (testing "empty :mailboxes vector"
    (is (re-find #"non-empty vector" (:error (main/check-mailboxes {:mailboxes []})))))
  (testing ":mailboxes is not a vector"
    (is (re-find #"non-empty vector" (:error (main/check-mailboxes {:mailboxes imap-mb}))))))

(deftest check-mailboxes-rejects-missing-or-bad-name
  (testing "missing :name"
    (let [r (main/check-mailboxes {:mailboxes [(dissoc imap-mb :name)]})]
      (is (re-find #"invalid :name" (:error r)))))
  (testing "blank :name"
    (let [r (main/check-mailboxes {:mailboxes [(assoc imap-mb :name "   ")]})]
      (is (re-find #"invalid :name" (:error r)))))
  (testing "non-string :name"
    (let [r (main/check-mailboxes {:mailboxes [(assoc imap-mb :name :foo)]})]
      (is (re-find #"invalid :name" (:error r)))))
  (testing "name with reserved characters (slash, colon) rejected"
    (is (re-find #"invalid :name"
                 (:error (main/check-mailboxes {:mailboxes [(assoc imap-mb :name "foo/bar")]}))))
    (is (re-find #"invalid :name"
                 (:error (main/check-mailboxes {:mailboxes [(assoc imap-mb :name "foo:bar")]})))))
  (testing "name with embedded spaces accepted (aligned with :source/name)"
    (is (= [(assoc imap-mb :name "primary mail")]
           (:ok (main/check-mailboxes {:mailboxes [(assoc imap-mb :name "primary mail")]}))))))

(deftest check-mailboxes-rejects-bad-type
  (let [r (main/check-mailboxes {:mailboxes [(assoc imap-mb :type :pop3)]})]
    (is (re-find #"invalid :type" (:error r)))))

(deftest check-mailboxes-rejects-duplicate-names
  (let [r (main/check-mailboxes {:mailboxes [imap-mb (assoc maildir-mb :name "primary")]})]
    (is (re-find #":name values must be unique" (:error r)))))

;; ---------------------------------------------------------------------------
;; :ingest override per mailbox -- priority: CLI > local > global > defaults
;; ---------------------------------------------------------------------------

(deftest run-opts-uses-global-ingest-when-mailbox-has-none
  (let [opts (run-opts maildir-mb
                       {:fetch {:limit 200} :max-size 999}
                       nil)]
    (is (= {:limit 200} (select-keys (:fetch-opts opts) [:limit])))
    (is (= 999 (-> opts :ingest-opts :max-size)))))

(deftest run-opts-falls-back-to-default-fetch-when-nothing-set
  (let [opts (run-opts maildir-mb {} nil)]
    (is (= {:limit 50} (select-keys (:fetch-opts opts) [:limit])))))

(deftest run-opts-local-ingest-overrides-global
  (let [mb (assoc maildir-mb :ingest {:fetch {:limit 5}})
        opts (run-opts mb {:fetch {:limit 200} :max-size 999} nil)]
    (testing ":fetch from mailbox-local wins"
      (is (= {:limit 5} (select-keys (:fetch-opts opts) [:limit]))))
    (testing "keys not set locally fall through to global"
      (is (= 999 (-> opts :ingest-opts :max-size))))))

(deftest run-opts-local-fetch-fully-replaces-global-fetch
  (testing "local :fetch wholly replaces global :fetch (shallow merge)"
    (let [mb (assoc maildir-mb :ingest {:fetch {:limit 5}})
          opts (run-opts mb {:fetch {:since "30d"}} nil)]
      (is (= 5  (-> opts :fetch-opts :limit)))
      (is (nil? (-> opts :fetch-opts :since))))))

(deftest run-opts-local-max-size-overrides-global
  (let [mb (assoc maildir-mb :ingest {:max-size 100})
        opts (run-opts mb {:max-size 999 :max-attachment-size 555} nil)]
    (testing ":max-size from mailbox-local wins"
      (is (= 100 (-> opts :ingest-opts :max-size))))
    (testing "other ingest keys fall through to global"
      (is (= 555 (-> opts :ingest-opts :max-attachment-size))))))

(deftest run-opts-cli-fetch-beats-everything
  (let [mb (assoc maildir-mb :ingest {:fetch {:since "30d"}})
        opts (run-opts mb {:fetch {:limit 200}} {:limit 7})]
    (testing "CLI --fetch is the sole survivor in :fetch-opts"
      (is (= {:limit 7} (select-keys (:fetch-opts opts) [:limit])))
      (is (nil? (:since (:fetch-opts opts)))))))

(deftest load-context-rejects-missing-config
  (let [missing-path (str (System/getProperty "java.io.tmpdir")
                          "/bone-missing-config-" (System/nanoTime) ".edn")]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Config file not found"
                          (load-context nil missing-path)))))

;; ---------------------------------------------------------------------------
;; Mid lock -- prevents concurrent digest of the same email when two
;; mailboxes subscribe to the same source in watch mode.
;; ---------------------------------------------------------------------------

(deftest mid-ownership-first-caller-wins
  ;; Use a unique mid to avoid colliding with other tests via the
  ;; shared atom.
  (let [mid "<lock-test-1@example.org>"]
    (try
      (testing "first take-mid-ownership! claims the mid"
        (is (true? (main/take-mid-ownership! mid))))
      (testing "second take-mid-ownership! observes the claim and returns false"
        (is (false? (main/take-mid-ownership! mid))))
      (testing "after release another caller can take it again"
        (main/release-mid-ownership! mid)
        (is (true? (main/take-mid-ownership! mid))))
      (finally
        (main/release-mid-ownership! mid)))))

(deftest mid-ownership-independent-mids-do-not-block
  (let [m1 "<lock-test-2a@example.org>"
        m2 "<lock-test-2b@example.org>"]
    (try
      (is (true? (main/take-mid-ownership! m1)))
      (is (true? (main/take-mid-ownership! m2)))
      (is (false? (main/take-mid-ownership! m1)))
      (is (false? (main/take-mid-ownership! m2)))
      (finally
        (main/release-mid-ownership! m1)
        (main/release-mid-ownership! m2)))))
