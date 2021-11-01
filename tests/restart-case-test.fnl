(require-macros :fennel-test)
(local {: invoke-restart : continue : find-restart : error : Condition : warn} (require :init))
(require-macros :init-macros)

(deftest calling-restarts-without-handlers
  (testing "no-scope"
    (assert-not (warn Condition)))

  (testing "arbitrary restart"
    (assert-eq :ok (restart-case (invoke-restart :r)
                     (:r [] :ok)))
    (assert-eq :ok (restart-case (invoke-restart :r :ok)
                     (:r [a] a))))
  (testing "continue restart"
    (assert-eq :ok (restart-case (continue)
                     (:continue [] :ok))))

  (testing "first restart is called"
    (assert-eq :ok (restart-case (invoke-restart :r)
                     (:r [] :ok)
                     (:r [] :bad)))

    (assert-eq :ok (restart-case (invoke-restart :r)
                     (:f [] :bad)
                     (:r [] :ok)
                     (:r [] :bad)))))

(deftest finding-restarts
  (testing "find restarts from the handler"
    (assert-eq :ok (handler-bind [:err (fn [] (when (find-restart :restart)
                                                (invoke-restart :restart)))]
                     (restart-case (error :err)
                       (:restart [] :ok)))))

  (testing "Find restart from within restart-case"
    (assert-eq :restart (restart-case (find-restart :restart)
                          (:restart []  :ok)))))
