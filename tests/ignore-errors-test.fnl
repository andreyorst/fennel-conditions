(require-macros :fennel-test.test)
(local {: signal : error} (require :init))
(require-macros :macros)

(deftest ignore-errors-test
  (assert-not (ignore-errors))
  (assert-eq 42 (ignore-errors 42))
  (assert-eq 42 (ignore-errors 27 42))
  (assert-eq
   42
   (handler-case
       (ignore-errors (signal :signal))
     (:signal [] 42)))
  (assert-eq
   [nil :error]
   [(handler-case
        (ignore-errors (error :error))
      (:error [] 42))]))
