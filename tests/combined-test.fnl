(require-macros :fennel-test)
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
      (assert-eq ["bind" "restart" "case"] res)))
  (testing "handling errors inside the handler"
    (assert-eq :ok (ignore-errors
                     (handler-bind [:handler-bind
                                    (fn []
                                      (handler-case (error :handler-case)
                                        (:handler-case [] (invoke-restart :restart))))]
                       (restart-case (error :handler-bind)
                         (:restart [] :ok)))))
    (assert-eq :ok (ignore-errors
                     (handler-bind [:handler-bind
                                    (fn []
                                      (assert-eq :inner-ok (handler-case (error :handler-case)
                                                             (:handler-case [] :inner-ok)))
                                      (invoke-restart :restart))]
                       (restart-case (error :handler-bind)
                         (:restart [] :ok)))))
    (assert-eq :ok (ignore-errors
                     (handler-bind [:handler-bind
                                    (fn []
                                      (ignore-errors (error :ignored-error))
                                      (invoke-restart :restart))]
                       (restart-case (error :handler-bind)
                         (:restart [] :ok)))))
    (assert-eq :ok (ignore-errors
                     (handler-bind [:handler-bind
                                    (fn []
                                      (unwind-protect
                                          (error :protected-error)
                                        (invoke-restart :restart)))]
                       (restart-case (error :handler-bind)
                         (:restart [] :ok)))))
    (assert-eq :ok (ignore-errors
                     (handler-bind [:handler-bind
                                    (fn []
                                      (unwind-protect :no-error
                                        (invoke-restart :restart)))]
                       (restart-case (error :handler-bind)
                         (:restart [] :ok)))))))
