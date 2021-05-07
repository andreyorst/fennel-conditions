(require-macros :fennel-test.test)
(local {: error : warn : signal : invoke-restart : continue} (require :init))
(require-macros :macros)

(deftest cerror-test
  (testing "continue"
    (handler-bind [:condition (fn [] (continue))]
      (assert-not (restart-case
                      (restart-case (cerror "nil" :condition)
                        (:inner [] :bad))
                    (:outer [] :bad2)))
      (assert-eq :ok (restart-case
                         (restart-case (do (cerror "nil" :condition)
                                           :ok)
                           (:inner [] :bad))
                       (:outer [] :bad2)))))
  (testing "discard"
    (handler-bind [:condition (fn [] (invoke-restart :inner))]
      (assert-eq :ok (restart-case
                         (restart-case (cerror "nil" :condition)
                           (:inner [] :ok))
                       (:outer [] :bad))))
    (handler-bind [:condition (fn [] (invoke-restart :outer))]
      (assert-eq :ok (restart-case
                         (restart-case (do (cerror "nil" :condition)
                                           :bad)
                           (:inner [] :bad2))
                       (:outer [] :ok))))))
