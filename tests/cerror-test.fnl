(require-macros :fennel-test.test)
(local {: error : warn : signal : invoke-restart : continue} (require :init))
(require-macros :macros)

(deftest cerror-test
  (testing "continue"
    (handler-bind [:condition (fn [] (continue))]
      (assert-not (restart-case
                      (restart-case (cerror "nil" :condition)
                        (:inner [] 42))
                    (:outer [] 27)))
      (assert-eq 72 (restart-case
                        (restart-case (do (cerror "nil" :condition)
                                          72)
                          (:inner [] 42))
                      (:outer [] 27)))))
    (testing "discard"
      (handler-bind [:condition (fn [] (invoke-restart :inner))]
        (assert-eq 42 (restart-case
                          (restart-case (cerror "nil" :condition)
                            (:inner [] 42))
                        (:outer [] 27))))
      (handler-bind [:condition (fn [] (invoke-restart :outer))]
        (assert-eq 27 (restart-case
                          (restart-case (do (cerror "nil" :condition)
                                            72)
                            (:inner [] 42))
                        (:outer [] 27))))))
