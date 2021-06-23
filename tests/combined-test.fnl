(require-macros :fennel-test.test)
(local {: error : warn : signal : invoke-restart : continue &as cs} (require :init))
(require-macros :init-macros)


(deftest handler-bind-in-handler-case
  (testing "declined condition"
    (let [res []]
      (assert-eq :ok (handler-case
                         (handler-bind [:error (fn [] (table.insert res "bind"))]
                           (error :error))
                       (cs.Error [] (table.insert res "case") :ok)))
      (assert-eq ["bind" "case"] res)))

  (testing "restart throws error"
    (let [res []]
      (assert-eq :ok (handler-case
                         (handler-bind [:error (fn [] (table.insert res "bind") (invoke-restart :r))]
                           (restart-case (error :error)
                             (:r [] (table.insert res "restart") (error :new-error))))
                       (:new-error [] (table.insert res "case") :ok)))
      (assert-eq ["bind" "restart" "case"] res))))
