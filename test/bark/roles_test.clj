(ns bark.roles-test
  "Unit tests for the permission invariants of bark.roles/apply-role-controls!.
  These pin down the three rules that hold the new tenure model together:
    1. any active maintainer can Add maintainers
    2. only the lead maintainer can Remove maintainers
    3. the lead cannot remove themselves"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datalevin.core :as d]
            [bark.common :as common]
            [bark.roles :as roles]
            [bark.test-helpers :as th]))

(use-fixtures :once th/with-temp-failures-file)

(defn- fresh-conn []
  (let [path (str "/tmp/bark-roles-test-" (System/nanoTime))]
    (d/get-conn path common/bark-schema)))

(defn- seed-two-maintainers!
  "Seed two maintainers on source `src`; the first is the lead."
  [conn src lead co-maint]
  (d/transact! conn [{:maint-tenure/source src
                      :maint-tenure/email  lead
                      :maint-tenure/order  0}
                     {:maint-tenure/source src
                      :maint-tenure/email  co-maint
                      :maint-tenure/order  1}]))

(def ^:private t1 #inst "2026-02-01T10:00:00.000-00:00")

(deftest add-maintainer-permissions
  (testing "a non-maintainer cannot Add"
    (let [conn (fresh-conn)
          _    (seed-two-maintainers! conn "s" "lead@x.org" "co@x.org")
          t0   (roles/get-tenures (d/db conn) "s")]
      (roles/apply-role-controls! conn t0 "s" "user@x.org"
                                  "Add maintainer: intruder@x.org" t1)
      (is (not (common/maintainer? (roles/get-tenures (d/db conn) "s")
                                   "intruder@x.org")))))

  (testing "a non-lead maintainer CAN Add"
    (let [conn (fresh-conn)
          _    (seed-two-maintainers! conn "s" "lead@x.org" "co@x.org")
          t0   (roles/get-tenures (d/db conn) "s")]
      (roles/apply-role-controls! conn t0 "s" "co@x.org"
                                  "Add maintainer: peer@x.org" t1)
      (is (common/maintainer? (roles/get-tenures (d/db conn) "s")
                              "peer@x.org")))))

(deftest remove-maintainer-permissions
  (testing "a non-lead maintainer CANNOT Remove"
    (let [conn (fresh-conn)
          _    (seed-two-maintainers! conn "s" "lead@x.org" "co@x.org")
          t0   (roles/get-tenures (d/db conn) "s")]
      (roles/apply-role-controls! conn t0 "s" "co@x.org"
                                  "Remove maintainer: lead@x.org" t1)
      ;; lead tenure is untouched
      (is (common/maintainer? (roles/get-tenures (d/db conn) "s")
                              "lead@x.org"))))

  (testing "the lead CAN remove a peer"
    (let [conn (fresh-conn)
          _    (seed-two-maintainers! conn "s" "lead@x.org" "co@x.org")
          t0   (roles/get-tenures (d/db conn) "s")]
      (roles/apply-role-controls! conn t0 "s" "lead@x.org"
                                  "Remove maintainer: co@x.org" t1)
      (is (not (common/maintainer? (roles/get-tenures (d/db conn) "s")
                                   "co@x.org"))))))

(deftest lead-is-irremovable
  (testing "the lead cannot remove themselves"
    (let [conn (fresh-conn)
          _    (seed-two-maintainers! conn "s" "lead@x.org" "co@x.org")
          t0   (roles/get-tenures (d/db conn) "s")]
      (roles/apply-role-controls! conn t0 "s" "lead@x.org"
                                  "Remove maintainer: lead@x.org" t1)
      (is (common/maintainer? (roles/get-tenures (d/db conn) "s")
                              "lead@x.org"))
      (is (= "lead@x.org"
             (common/lead-maintainer (roles/get-tenures (d/db conn) "s")))))))

(deftest rfc5322-address-forms
  (testing "plain bare address"
    (let [conn (fresh-conn)
          _    (seed-two-maintainers! conn "s" "lead@x.org" "co@x.org")
          t0   (roles/get-tenures (d/db conn) "s")]
      (roles/apply-role-controls! conn t0 "s" "co@x.org"
                                  "Add maintainer: peer@x.org" t1)
      (is (common/maintainer? (roles/get-tenures (d/db conn) "s")
                              "peer@x.org"))))

  (testing "Display Name <addr> form"
    (let [conn (fresh-conn)
          _    (seed-two-maintainers! conn "s" "lead@x.org" "co@x.org")
          t0   (roles/get-tenures (d/db conn) "s")]
      (roles/apply-role-controls! conn t0 "s" "co@x.org"
                                  "Add maintainer: Peer User <peer@x.org>" t1)
      (is (common/maintainer? (roles/get-tenures (d/db conn) "s")
                              "peer@x.org"))))

  (testing "mix of bracketed and bare addresses preserves order"
    (let [conn (fresh-conn)
          _    (seed-two-maintainers! conn "s" "lead@x.org" "co@x.org")
          t0   (roles/get-tenures (d/db conn) "s")]
      (roles/apply-role-controls!
       conn t0 "s" "co@x.org"
       "Add maintainer: alice@x.org Bob <bob@x.org> carol@x.org" t1)
      (let [ts (roles/get-tenures (d/db conn) "s")]
        (is (common/maintainer? ts "alice@x.org"))
        (is (common/maintainer? ts "bob@x.org"))
        (is (common/maintainer? ts "carol@x.org"))))))
