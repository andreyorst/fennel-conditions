(require-macros :fennel-test.test)
(local {: invoke-debugger : make-condition} (require :init))
(require-macros :macros)
(set _G.fennel-conditions/use-debugger? true)

(macro with-no-stderr [expr]
  "Suppress output to stderr."
  `(let [stderr-mt# (. (getmetatable io.stderr) :__index)
         write# stderr-mt#.write]
     (tset stderr-mt# :write (fn [] ""))
     (let [res# (table.pack ,expr)]
       (tset stderr-mt# :write write#)
       (table.unpack res# 1 res#.n))))

(local stdin-meta (. (getmetatable io.stdin) :__index))

(deftest debugger
  (testing "default restart throws"
    (set stdin-meta.read (fn [] "throw"))
    (let [(_ msg) (with-no-stderr (pcall invoke-debugger :foo))]
      (assert-is (msg:match "condition foo was raised") msg))

    (define-condition bar)
    (let [(_ msg) (with-no-stderr (pcall invoke-debugger (make-condition bar 1 nil)))]
      (assert-is (msg:match "condition bar was raised with the following arguments: 1, nil") msg)))

  (testing "user bound restart"
    (set stdin-meta.read (fn [] "restart"))
    (assert-eq :ok (with-no-stderr
                    (restart-case (invoke-debugger :foo)
                      (:restart [] :ok)))))

  (testing "first non-unique restart chosen"
    (set stdin-meta.read (fn [] "restart"))
    (assert-eq :ok (with-no-stderr
                    (restart-case
                        (restart-case (invoke-debugger :foo)
                          (:restart [] :ok)
                          (:restart [] :err))
                      (:restart [] :err)))))

  (testing "restart-order"
    (let [res []
          foo (fn []
                (with-no-stderr
                 (restart-case
                     (restart-case (invoke-debugger :foo)
                       (:restart [] (table.insert res :ok1) :ok1)
                       (:restart [] (table.insert res :ok2) :ok2))
                   (:restart [] (table.insert res :ok3) :ok3))))]
      (set stdin-meta.read (fn [] "1"))
      (assert-eq :ok1 (foo))
      (set stdin-meta.read (fn [] "2"))
      (assert-eq :ok2 (foo))
      (set stdin-meta.read (fn [] "3"))
      (assert-eq :ok3 (foo))
      (assert-eq res [:ok1 :ok2 :ok3])))

  (testing "interactive restarts"
    (set stdin-meta.read (fn [] "1"))
    (assert-eq [1 :ok] (with-no-stderr
                        (restart-case (invoke-debugger :foo)
                          (:restart [a] [a :ok])))))

  (testing "error during interactive restart"
    (var i 0)
    (set stdin-meta.read (fn []
                           (set i (+ i 1))
                           (. ["1" "a" "2" "1" "42"] i)))
    (assert-eq [42 :ok] (with-no-stderr
                         (restart-case (invoke-debugger :foo)
                           (:restart [a] [a :ok])))))

  (testing "throw from second level"
    (var i 0)
    (set stdin-meta.read (fn []
                           (set i (+ i 1))
                           (. ["1" "a" "3"] i)))
    (assert-eq false (pcall #(with-no-stderr
                              (restart-case (invoke-debugger :foo)
                                (:restart [a] [a :ok])))))))
