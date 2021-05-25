(require-macros :fennel-test.test)
(local {: error : invoke-debugger : make-condition} (require :init))
(require-macros :macros)
(set _G.fennel-conditions/use-debugger? true)

(macro with-no-stderr [expr]
  "Suppress output to stderr."
  `(let [stderr-mt# (. (getmetatable io.stderr) :__index)
         write# stderr-mt#.write]
     (tset stderr-mt# :write (fn [fd# ...]
                               (when (not= fd# io.stderr)
                                 (write# fd# ...))))
     (let [res# (table.pack ,expr)]
       (tset stderr-mt# :write write#)
       (table.unpack res# 1 res#.n))))

(macro with-stderr-to-str [expr]
  "Captures stderr output into string."
  `(let [stderr-mt# (. (getmetatable io.stderr) :__index)
         write# stderr-mt#.write]
     (var out# "")
     (tset stderr-mt# :write (fn [fd# ...]
                               (if (= fd# io.stderr)
                                   (set out# (.. out# (table.concat [...] "")))
                                   (write# fd# ...))))
     (let [res# (table.pack ,expr)]
       (tset stderr-mt# :write write#)
       out#)))

(local stdin-meta (. (getmetatable io.stdin) :__index))

(deftest debugger
  (testing "default restart throws"
    (set stdin-meta.read (fn [] "throw"))
    (let [(_ msg) (with-no-stderr (pcall error :foo))]
      (assert-is (msg:match "condition \"foo\" was raised") msg))

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
                           (:restart [a] [a :ok]))))
    (set i 0)
    (assert-eq
     "Debugger was invoked on unhandled condition: \"foo\"
restarts (invokable by number or by name):
  1: [restart] restart-doc
  2: [throw  ] Throw condition as a Lua error
debugger>> Provide inputs for restart (args: [a]) (^D to cancel)
debugger:restart>> Level 2 debugger was invoked on unhandled condition: \"Compile error in unknown:1
  unknown global in strict mode: a

(values a)
^
* Try looking to see if there's a typo.
* Try using the _G table instead, eg. _G.a if you really want a global.
* Try moving this code to somewhere that a is in scope.
* Try binding a as a local in the scope of this code.\"
restarts (invokable by number or by name):
  1: [restart] restart-doc
  2: [cancel ] Return to level 1 debugger
  3: [throw  ] Throw condition as a Lua error
debugger>> restarts (invokable by number or by name):
  1: [restart] restart-doc
  2: [throw  ] Throw condition as a Lua error
debugger>> Provide inputs for restart (args: [a]) (^D to cancel)
debugger:restart>> "
     (with-stderr-to-str
      (restart-case (invoke-debugger :foo)
        (:restart [a] "restart-doc" [a :ok])))))

  (testing "throw from second level"
    (var i 0)
    (set stdin-meta.read (fn []
                           (set i (+ i 1))
                           (. ["1" "a" "3" "2"] i)))
    (assert-eq false (pcall #(with-no-stderr
                              (restart-case (invoke-debugger :foo)
                                (:restart [a] [a :ok]))))))

  (testing "throw from nested scopes"
    (set stdin-meta.read #"4")
    (assert-eq false (pcall #(with-no-stderr
                              (restart-case
                                  (handler-case
                                      (handler-bind [:foo (fn [] :err)]
                                        (handler-case
                                            (restart-case
                                                (restart-case (invoke-debugger :foo)
                                                  (:restart [] :err))
                                              (:restart [] :err))
                                          (:bar [] :err)))
                                    (:bar [] :err))
                                (:restart [] :err))))))

  (testing "OK from second level"
           (var i 0)
           (set stdin-meta.read (fn []
                                  (set i (+ i 1))
                                  (. ["1" "a" "2"] i)))
           (assert-eq [:ok :ok] (with-no-stderr
                                 (restart-case (invoke-debugger :foo)
                                   (:restart [a] [a :ok])
                                   (:restart2 [] [:ok :ok])))))

  (testing "Wrong action"
    (var i 0)
    (set stdin-meta.read (fn []
                           (set i (+ i 1))
                           (. ["10" "1" ":ok"] i)))
    (assert-eq [:ok :ok] (with-no-stderr
                          (restart-case (invoke-debugger :foo)
                            (:restart [a] [a :ok])))))

  (testing "^D"
    (var i 0)
    (set stdin-meta.read (fn []
                           (set i (+ i 1))
                           (. [nil "1" nil "1" ":ok"] i)))
    (assert-eq [:ok :ok] (with-no-stderr
                          (restart-case (invoke-debugger :foo)
                            (:restart [a] [a :ok]))))))
