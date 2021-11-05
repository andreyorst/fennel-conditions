(require-macros :fennel-test)
(local {: invoke-restart : error : make-condition : Error} (require :init))
(require-macros :init-macros)

(deftest vararg-test
  (testing "varargs in handler-bind and restart-case"
    ((fn [...]
       (handler-bind [Error (fn [_ ...] (invoke-restart :r ...))]
         (restart-case (error (make-condition Error ...))
           (:r [...] (assert-eq 6 (select :# ...)))))) 1 2 nil 3 nil nil)
    ((fn [...]
       (handler-bind [Error (fn [_ ...] (invoke-restart :r ...))]
         (assert-eq 6 (restart-case (error (make-condition Error ...))
                        (:r [...] (select :# ...)))))) 1 2 nil 3 nil nil)
    (assert-eq 6 ((fn [...]
                    (handler-bind [Error (fn [_ ...] (invoke-restart :r ...))]
                      (restart-case (error (make-condition Error ...))
                        (:r [...] (select :# ...))))) 1 2 nil 3 nil nil)))
  (testing "varargs in unwind-protect"
    (var result nil)
    ((fn [...]
       (ignore-errors
         (unwind-protect (/ 1 nil)
           (set result (select "#" ...))))) 1 2 nil 3 nil nil nil)
    (assert-eq 7 result))
  (testing "varargs in handler-case"
    (assert-eq 8 ((fn [...] (handler-case (error (make-condition Error ...))
                              (Error [_ ...] (select "#" ...)))) 1 2 nil nil nil 3 nil nil))
    (assert-eq {1 1 2 2 6 3 :n 8}
               ((fn [...] (handler-case (error (make-condition Error ...))
                            (Error [_ ...] (doto [...] (tset :n (select "#" ...)))))) 1 2 nil nil nil 3 nil nil))))
