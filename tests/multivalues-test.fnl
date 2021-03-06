(require-macros :fennel-test)
(local {: invoke-restart : error} (require :init))
(require-macros :init-macros)

(deftest multivalues
  (testing "returning correct multivalues on success"
    (assert-eq 6 (select :# (handler-case (values 1 2 nil 3 nil nil))))
    (assert-is (match (handler-case (values 1 2 nil 3 nil nil))
                 (1 2 nil 3 nil nil) true
                 _ false)))

  (testing "passing correct multivalues to restart"
    (handler-bind [:e (fn [] (invoke-restart :r 1 2 nil 3 nil nil))]
      (assert-eq 6 (restart-case (error :e)
                     (:r [...] (select :# ...)))))
    (assert-is (handler-bind [:e (fn [] (invoke-restart :r 1 2 nil 3 nil nil))]
                 (restart-case (error :e)
                   (:r [...] (match ...
                               (1 2 nil 3 nil nil) true
                               _ false))))))

  (testing "returning correct multivalues from restart"
    (assert-eq 6 (select :# (handler-bind [:e (fn [] (invoke-restart :r 1 2 nil 3 nil nil))]
                              (restart-case (error :e)
                                (:r [...] (values ...))))))
    (assert-is (match (handler-bind [:e (fn [] (invoke-restart :r 1 2 nil 3 nil nil))]
                        (restart-case (error :e)
                          (:r [...] (values ...))))
                 (1 2 nil 3 nil nil) true
                 _ false)))

  (testing "returning correct multivalues from handler"
    (assert-eq 6 (select :# (handler-case (error :e)
                              (:e [] (values 1 2 nil 3 nil nil)))))
    (assert-is (match (handler-case (error :e)
                        (:e [] (values 1 2 nil 3 nil nil)))
                 (1 2 nil 3 nil nil) true
                 _ false)))

  (testing "returning correct multivalues from restart-case"
    (assert-eq 6 (select :# (restart-case (values 1 2 nil 3 nil nil))))
    (assert-is (match (restart-case (values 1 2 nil 3 nil nil))
                 (1 2 nil 3 nil nil) true
                 _ false))))
