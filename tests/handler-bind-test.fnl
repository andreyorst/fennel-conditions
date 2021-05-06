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
      (assert-eq res ["inner" "outer" "restart"])))

  (testing "decline to handle the condition, handle in the next handler"
    (let [res []]
      (assert-eq :ok (handler-bind [:condition (fn [] (table.insert res "first decline"))
                                    :condition (fn [] (table.insert res "second decline"))
                                    :fennel-conditions/error (fn [] (table.insert res "error catchall decline"))
                                    :fennel-conditions/condition (fn [] (table.insert res "catchall decline"))
                                    :condition (fn [] (table.insert res "handle") (invoke-restart :r))]
                       (restart-case (error :condition)
                         (:r [] (table.insert res "restart") :ok))))
      (assert-eq ["first decline"
                  "second decline"
                  "error catchall decline"
                  "catchall decline"
                  "handle"
                  "restart"]
                 res)))

  (testing "trying to return to already handled restart-case"
    (let [res []
          (ok? msg) (pcall #(handler-bind [:simple-error (fn [] (table.insert res "se") (invoke-restart :r1))
                                           :error (fn [] (table.insert res "e") (invoke-restart :r2))]
                              (restart-case (error :simple-error)
                                (:r1 [] (table.insert res "r1") (error :error))
                                (:r2 [] (table.insert res "r2")))))]
      (assert-not ok?)
      (assert-is (msg:gmatch "restart \"r2\" is not found$"))
      (assert-eq ["se" "r1" "e"] res)))

  (testing "re-signale the error"
    (let [res []]
      (assert-eq :ok (handler-bind [:condition (fn [] (table.insert res "third") (invoke-restart :r))]
                       (handler-bind [:condition (fn [c] (table.insert res "first") (error c))
                                      :condition (fn [] (table.insert res "second"))]
                         (restart-case (error :condition)
                           (:r [] (table.insert res "restart") :ok)))))
      (assert-eq ["first"
                  "third"
                  "restart"]
                 res)))
  )
