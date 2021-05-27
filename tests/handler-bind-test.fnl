(require-macros :fennel-test.test)
(local {: error : warn : signal : invoke-restart &as cs} (require :init))
(require-macros :macros)

(deftest invoking-restarts
  (testing "no scope"
    (assert-not (pcall invoke-restart :nope)))

  (testing "invalid handler-bind"
    (let [(ok? msg) (pcall #(handler-bind [nil (fn [] :bad)]
                              :ok))]
      (assert-not ok?)
      (assert-is (msg:match "condition object must not be nil"))))

  (testing "restart not found"
    (let [(ok? msg) (pcall #(handler-bind [:error (fn [] (invoke-restart :bar))]
                              (restart-case (error :error)
                                (:baz [] :bad))))]
      (assert-not ok?)
      (assert-is (msg:match "restart \"bar\" is not found$")))

    (let [(ok? msg) (pcall #(handler-bind [:error (fn [] (invoke-restart :bar))]
                              (error :error)))]
      (assert-not ok?)
      (assert-is (msg:match "restart \"bar\" is not found$")))

    (let [(ok? msg) (pcall invoke-restart :bar)]
      (assert-not ok?)
      (assert-is (msg:match "restart \"bar\" is not found$"))))

  (testing "control transfered to correct restart-case"
    (handler-bind [:condition (fn [] (invoke-restart :restart))]
      (assert-eq :ok (restart-case (error :condition)
                       (:restart [] :ok))))
    (handler-bind [:condition (fn [] (invoke-restart :outer))]
      (assert-eq :ok (restart-case
                         (restart-case (error :condition)
                           (:inner [] :not-ok))
                       (:outer [] :ok))))
    (handler-bind [:condition (fn [] (invoke-restart :inner))]
      (assert-eq :ok (restart-case
                         (restart-case (error :condition)
                           (:inner [] :ok))
                       (:outer [] :not-ok)))))

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
                                    cs.Error (fn [] (table.insert res "error catchall decline"))
                                    cs.Condition (fn [] (table.insert res "catchall decline"))
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
      (assert-is (msg:match "restart \"r2\" is not found$"))
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
                 res))

    (let [res []]
      (assert-eq :ok (handler-bind [:condition (fn [] (table.insert res 9) (invoke-restart :r))]
                       (handler-bind [:condition (fn [] (table.insert res 6))
                                      :condition (fn [c] (table.insert res 7) (error c))
                                      :condition (fn [] (table.insert res 8))]
                         (handler-bind [:condition (fn [] (table.insert res 1))
                                        :condition (fn [] (table.insert res 2))
                                        :condition (fn [] (table.insert res 3))
                                        :condition (fn [] (table.insert res 4))
                                        :condition (fn [] (table.insert res 5))]
                           (restart-case (error :condition)
                             (:r [] (table.insert res 10) :ok))))))
      (assert-eq [1 2 3 4 5 6 7 9 10]
                 res)))

  (testing "handling Lua error"
    (assert-eq :ok (handler-bind [cs.Error
                                  (fn [] (invoke-restart :r))]
                     (restart-case (/ 1 nil)
                       (:r [] :ok))))
    (let [(ok? msg) (pcall #(handler-bind [cs.Error
                                           (fn [] :decline)]
                              (restart-case (/ 1 nil)
                                (:r [] :ok))))]
      (assert-not ok?)
      (assert-is (msg:match "attempt to perform arithmetic on a nil value$")))))
