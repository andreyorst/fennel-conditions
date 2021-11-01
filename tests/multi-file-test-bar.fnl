(require-macros :fennel-test)
(import-macros {: restart-case : handler-bind : handler-case} :init-macros)
(local {: error : signal : invoke-restart} (require :init))
(local {: foo} (require :tests.multi-file-test-foo))

(deftest handling-from-enother-module
  (testing "calling restart from another module"
    (handler-bind [:error (fn [] (invoke-restart :r))]
      (assert-eq :ok (foo)))))
