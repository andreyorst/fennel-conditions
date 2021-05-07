(require-macros :fennel-test.test)
(local {: invoke-restart : error} (require :init))
(require-macros :macros)

(deftest coroutines
  (testing "coroutine without yields"
    (let [res []
          c (coroutine.create
             #(restart-case (error :error)
                (:r [] (table.insert res "restart") :ok)))]
      (handler-bind [:error (fn []
                              (table.insert res "handler")
                              (invoke-restart :r))]
        (assert-eq [true :ok] [(coroutine.resume c)]))
      (assert-eq res [:handler :restart])))

  (testing "coroutine yields outside restart-case"
    (let [res []
          c (coroutine.create
             (fn []
               (coroutine.yield :yielded)
               (restart-case (error :error)
                 (:r [] (table.insert res "restart") :ok))))]
      (handler-bind [:error (fn []
                              (table.insert res "handler")
                              (invoke-restart :r))]
        (assert-eq [true :yielded] [(coroutine.resume c)])
        (assert-eq [true :ok] [(coroutine.resume c)]))
      (assert-eq res [:handler :restart])))

  (testing "coroutine yields inside restart-case"
    (let [res []
          c (coroutine.create
             #(restart-case (do (coroutine.yield :yielded)
                                (error :error))
                (:r [] (table.insert res "restart") :ok)))]
      (handler-bind [:error (fn []
                              (table.insert res "handler")
                              (invoke-restart :r))]
        (assert-eq [true :yielded] [(coroutine.resume c)])
        (assert-eq [true :ok] [(coroutine.resume c)]))
      (assert-eq res [:handler :restart])))

  (testing "No restart executed"
    (let [res []
          c (coroutine.create
             #(restart-case (do (coroutine.yield :yielded)
                                (error :error))
                (:r [] (table.insert res "restart") :ok)))]
      (handler-bind [:error (fn []
                              (table.insert res "handler"))]
        (assert-eq [true :yielded] [(coroutine.resume c)])
        (assert-eq false (coroutine.resume c)))
      (assert-eq res [:handler])))

  (testing "Attempt to resume after exception"
    (let [c (coroutine.create
             #(restart-case (do (coroutine.yield :yielded1)
                                (error :error)
                                (coroutine.yield :yielded2))
                (:r [] :ok)))]
      (assert-eq [true :yielded1] [(coroutine.resume c)])
      (assert-eq false (coroutine.resume c))
      (assert-eq false (coroutine.resume c)))))
