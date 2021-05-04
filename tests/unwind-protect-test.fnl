(require-macros :fennel-test.test)
(local {: invoke-restart : error} (require :init))
(require-macros :macros)

(deftest unwind-protect-order
  (testing "restart first"
    (let [res []]
      (assert-eq :ok (handler-bind [:error (fn []
                                             (table.insert res "handler")
                                             (invoke-restart :r))]
                       (unwind-protect
                           (unwind-protect
                               (restart-case
                                   (error :error)
                                 (:r [] (table.insert res "restart") :ok))
                             (table.insert res "inner"))
                         (table.insert res "outer"))))
      (assert-eq res ["handler" "restart" "inner" "outer"])))

  (testing "handler-case last"
    (let [res []]
      (assert-eq :ok (handler-case
                         (unwind-protect
                             (unwind-protect
                                 (error :error)
                               (table.insert res "inner"))
                           (table.insert res "outer"))
                       (:error [] (table.insert res "handler") :ok)))
      (assert-eq res ["inner" "outer" "handler"]))))
