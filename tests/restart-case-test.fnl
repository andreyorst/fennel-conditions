(require-macros :fennel-test.test)
(local {: invoke-restart : continue} (require :init))
(require-macros :macros)

(deftest calling-restarts-without-handlers
  (testing "arbitrary restart"
    (assert-eq :ok (restart-case (invoke-restart :r)
                     (:r [] :ok)))
    (assert-eq :ok (restart-case (invoke-restart :r :ok)
                     (:r [a] a))))
  (testing "continue restart"
    (assert-eq :ok (restart-case (continue)
                     (:fennel-conditions/continue [] :ok)))))