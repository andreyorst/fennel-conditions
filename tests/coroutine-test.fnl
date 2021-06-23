(require-macros :fennel-test.test)
(local {: invoke-restart : error} (require :init))
(require-macros :init-macros)

(deftest coroutines
  (testing "coroutine without yields"
    (let [res []
          c (coroutine.create
             #(handler-bind [:error (fn []
                              (table.insert res "handler")
                              (invoke-restart :r))]
                (restart-case (error :error)
                  (:r [] (table.insert res "restart") :ok))))]
      (assert-eq [true :ok] [(coroutine.resume c)])
      (assert-eq res [:handler :restart])))

  (testing "coroutine yields outside restart-case"
    (let [res []
          c (coroutine.create
             (fn []
               (handler-bind [:error (fn []
                                       (table.insert res "handler")
                                       (invoke-restart :r))]
                 (coroutine.yield :yielded)
                 (restart-case (error :error)
                   (:r [] (table.insert res "restart") :ok)))))]
      (assert-eq [true :yielded] [(coroutine.resume c)])
      (assert-eq [true :ok] [(coroutine.resume c)])
      (assert-eq res [:handler :restart])))

  (testing "coroutine yields inside restart-case"
    (let [res []
          c (coroutine.create
             #(handler-bind [:error (fn []
                                      (table.insert res "handler")
                                      (invoke-restart :r))]
                (restart-case (do (coroutine.yield :yielded)
                                  (error :error))
                  (:r [] (table.insert res "restart") :ok))))]
      (assert-eq [true :yielded] [(coroutine.resume c)])
      (assert-eq [true :ok] [(coroutine.resume c)])
      (assert-eq res [:handler :restart])))

  (testing "No restart executed"
    (let [res []
          c (coroutine.create
             #(handler-bind [:error (fn [] (table.insert res "handler"))]
                (restart-case (do (coroutine.yield :yielded)
                                  (error :error))
                  (:r [] (table.insert res "restart") :ok))))]
      (assert-eq [true :yielded] [(coroutine.resume c)])
      (assert-eq false (coroutine.resume c))
      (assert-eq res [:handler])))

  (testing "Attempt to resume after exception"
    (let [c (coroutine.create
             #(restart-case (do (coroutine.yield :yielded1)
                                (error :error)
                                (coroutine.yield :yielded2))
                (:r [] :ok)))]
      (assert-eq [true :yielded1] [(coroutine.resume c)])
      (assert-eq false (coroutine.resume c))
      (assert-eq false (coroutine.resume c))))

  (testing "Multiple coroutines"
    (fn fn-builder [res suffix]
      #(handler-bind [:error (fn []
                               (table.insert res (.. :handler suffix))
                               (coroutine.yield (.. :yielded suffix :-3))
                               (invoke-restart :r))]
         (coroutine.yield (.. :yielded suffix :-1))
         (restart-case (do (coroutine.yield (.. :yielded suffix :-2))
                           (error :error))
           (:r []
               (table.insert res (.. :restart suffix))
               (coroutine.yield (.. :yielded suffix :-4))
               (.. :ok- suffix)))))
    (let [res []
          c1 (coroutine.create (fn-builder res :1))
          c2 (coroutine.create (fn-builder res :2))]
      (assert-eq [true :yielded1-1] [(coroutine.resume c1)])
      (assert-eq [true :yielded2-1] [(coroutine.resume c2)])
      (assert-eq [true :yielded1-2] [(coroutine.resume c1)])
      (assert-eq [true :yielded2-2] [(coroutine.resume c2)])
      (assert-eq [true :yielded1-3] [(coroutine.resume c1)])
      (assert-eq [true :yielded2-3] [(coroutine.resume c2)])
      (assert-eq [true :yielded1-4] [(coroutine.resume c1)])
      (assert-eq [true :yielded2-4] [(coroutine.resume c2)])
      (assert-eq [true :ok-1] [(coroutine.resume c1)])
      (assert-eq [true :ok-2] [(coroutine.resume c2)])
      (assert-eq [:handler1 :handler2 :restart1 :restart2] res))

    (let [res []
          c1 (coroutine.create (fn-builder res :1))
          c2 (coroutine.create (fn-builder res :2))
          c3 (coroutine.create (fn-builder res :3))
          c4 (coroutine.create (fn-builder res :4))]
      (assert-eq [true :yielded1-1] [(coroutine.resume c1)])
      (assert-eq [true :yielded3-1] [(coroutine.resume c3)])
      (assert-eq [true :yielded1-2] [(coroutine.resume c1)])
      (assert-eq [true :yielded1-3] [(coroutine.resume c1)])
      (assert-eq [true :yielded2-1] [(coroutine.resume c2)])
      (assert-eq [true :yielded3-2] [(coroutine.resume c3)])
      (assert-eq [true :yielded4-1] [(coroutine.resume c4)])
      (assert-eq [true :yielded2-2] [(coroutine.resume c2)])
      (assert-eq [true :yielded4-2] [(coroutine.resume c4)])
      (assert-eq [true :yielded2-3] [(coroutine.resume c2)])
      (assert-eq [true :yielded2-4] [(coroutine.resume c2)])
      (assert-eq [true :ok-2] [(coroutine.resume c2)])
      (assert-eq [true :yielded4-3] [(coroutine.resume c4)])
      (assert-eq [true :yielded1-4] [(coroutine.resume c1)])
      (assert-eq [true :ok-1] [(coroutine.resume c1)])
      (assert-eq [true :yielded4-4] [(coroutine.resume c4)])
      (assert-eq [true :ok-4] [(coroutine.resume c4)])
      (assert-eq [true :yielded3-3] [(coroutine.resume c3)])
      (assert-eq [true :yielded3-4] [(coroutine.resume c3)])
      (assert-eq [true :ok-3] [(coroutine.resume c3)])
      (assert-eq [:handler1 :handler2 :restart2 :handler4 :restart1 :restart4 :handler3 :restart3] res))))
