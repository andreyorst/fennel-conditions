(require-macros :fennel-test.test)
(local {: signal : error} (require :init))
(require-macros :macros)

(deftest ignore-errors-test
  (testing "ignore-errors returns normally"
    (assert-eq nil (ignore-errors))
    (assert-eq 42 (ignore-errors 42))
    (assert-eq 42 (ignore-errors 27 42)))

  (testing "signal thrown through the ignore-case"
    (assert-eq
     42
     (handler-case
         (ignore-errors (signal :signal))
       (:signal [] 42))))

  (testing "error condition returned"
    (assert-eq
     [nil :error]
     [(handler-case
          (ignore-errors (error :error))
        (:error [] 42))]))

  (testing "ignoring Lua errors"
    (assert-eq nil (ignore-errors (* 1 nil)))))
