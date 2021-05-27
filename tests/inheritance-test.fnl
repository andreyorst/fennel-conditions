(require-macros :fennel-test.test)
(local {: make-condition : signal : invoke-restart &as cs} (require :init))
(require-macros :macros)

(deftest inheritance
  (testing "default inheritance"
    (define-condition simple-error)
    (assert-eq :ok (handler-case (signal simple-error)
                     (cs.Error [] :bad1)
                     (cs.Warning [] :bad2)
                     (cs.Condition [] :ok)))

    (assert-eq :ok (handler-bind [cs.Error (fn [] (invoke-restart :r))
                                  cs.Condition (fn [] (invoke-restart :r2))]
                     (restart-case (signal simple-error)
                       (:r [] :bad)
                       (:r2 [] :ok)))))

  (testing "custom inheritance"
    (define-condition simple-error :parent cs.Error)
    (assert-eq :ok (handler-case (signal simple-error)
                     (cs.Error [] :ok)
                     (cs.Warning [] :bad1)
                     (cs.Condition [] :bad2)))

    (assert-eq :ok (handler-bind [cs.Error (fn [] (invoke-restart :r))
                                  cs.Condition (fn [] (invoke-restart :r2))]
                     (restart-case (signal simple-error)
                       (:r [] :ok)
                       (:r2 [] :bad)))))

  (testing "custom parent has higher priority"
    (define-condition simple-error :parent cs.Error)
    (define-condition simple-error-2 :parent simple-error)
    (assert-eq :ok (handler-case (signal simple-error-2)
                     (simple-error [] :ok)
                     (simple-error-2 [] :bad1)
                     (cs.Error [] :bad2)
                     (cs.Warning [] :bad3)))

    (assert-eq :ok (handler-bind [simple-error (fn [] (invoke-restart :r))
                                  simple-error-2 (fn [] (invoke-restart :r2))
                                  cs.Error (fn [] (invoke-restart :r3))]
                     (restart-case (signal simple-error-2)
                       (:r [] :ok)
                       (:r2 [] :bad1)
                       (:r3 [] :bad2)))))

  (testing "Handling parent, matching on condition"
    (define-condition simple-error :parent cs.Error)
    (assert-eq :ok (handler-case (signal simple-error)
                     (cs.Error [c]
                               (match c
                                 simple-error :ok
                                 _ :bad))))))
