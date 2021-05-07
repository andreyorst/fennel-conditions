(require-macros :fennel-test.test)
(local {: signal : error} (require :init))
(require-macros :macros)

(deftest ignore-errors-test
  (testing "ignore-errors returns normally"
    (assert-eq nil (ignore-errors))
    (assert-eq :ok (ignore-errors :ok))
    (assert-eq :ok (ignore-errors :bad :ok))
    (assert-eq 5 (select :# (ignore-errors (values 1 1 1 1 1))))
    (assert-is (match (ignore-errors (values 5 1 4 2 3))
                 (5 1 4 2 3) true)))

  (testing "signal thrown through the ignore-case"
    (assert-eq
     :ok
     (handler-case
         (ignore-errors (signal :signal))
       (:signal [] :ok))))

  (testing "error condition returned"
    (assert-eq
     [nil :error]
     [(handler-case (ignore-errors (error :error))
        (:error [] :bad))]))

  (testing "ignoring Lua errors"
    (assert-eq nil (ignore-errors (* 1 nil))))

  (testing "ignoring declined condition"
    (let [res []]
      (assert-eq
       [nil :error]
       [(ignore-errors (handler-bind [:error (fn [] (table.insert res "decline"))]
                         (error :error)))])
      (assert-eq ["decline"] res))))
