(set table.pack nil)

(require-macros :fennel-test.test)
(local {: signal : Condition : make-condition &as cs} (require :init))
(require-macros :macros)

(deftest pack-test
  (testing "packed make condition"
    (assert-eq :ok (match (handler-case (signal (make-condition Condition 1 nil 2 nil nil))
                            (Condition [_ a b c d e]
                              (assert-eq a 1)
                              (assert-eq b nil)
                              (assert-eq c 2)
                              (assert-eq d nil)
                              (assert-eq e nil)
                              (values b a d c e)))
                     (nil 1 nil 2 nil) :ok))))
