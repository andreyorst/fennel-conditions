(require-macros :fennel-test.test)
(local {: error : warn : signal : invoke-restart} (require :init))
(require-macros :macros)

(deftest invoking-restarts
  (testing "control transfered to correct restart-case"
    (handler-bind [:condition (fn [] (invoke-restart :restart))]
      (assert-eq 42 (restart-case (error :condition)
                      (:restart [] 42))))
    (handler-bind [:condition (fn [] (invoke-restart :outer))]
      (assert-eq 27 (restart-case
                        (restart-case (error :condition)
                          (:inner [] 42))
                      (:outer [] 27))))
    (handler-bind [:condition (fn [] (invoke-restart :inner))]
      (assert-eq 42 (restart-case
                        (restart-case (error :condition)
                          (:inner [] 42))
                      (:outer [] 27)))))

  (testing "decline to handle the condition"
    (assert-not (pcall #(handler-bind [:condition (fn [] :nope)]
                          (error :condition)))))

  (testing "nested handler binds"
    (let [res []]
      (assert-eq :ok (handler-bind [:error (fn [] (table.insert res "outer") (invoke-restart :r))]
                       (handler-bind [:error (fn [] (table.insert res "inner"))]
                         (restart-case (error :error)
                           (:r [] (table.insert res "restart") :ok)))))
      (assert-eq res ["inner" "outer" "restart"]))))
