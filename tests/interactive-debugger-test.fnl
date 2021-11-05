(require-macros :fennel-test)
(local {: error : invoke-debugger : make-condition} (require :init))
(require-macros :init-macros)
(set _G.condition-system-use-debugger? true)

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

(deftest no-scope-test
  (set stdin-meta.read (fn [] :1))
  (with-no-stderr
   (assert-not (pcall invoke-debugger))))

(deftest default-restart-test
  (set stdin-meta.read (fn [] "throw"))
  (let [(_ msg) (with-no-stderr (pcall error :foo))]
    (assert-is (msg:match "condition \"foo\" was raised") msg))

  (define-condition bar)
  (let [(_ msg) (with-no-stderr (pcall invoke-debugger (make-condition bar 1 nil)))]
    (assert-is (msg:match "condition bar was raised with the following arguments: 1, nil") msg)))

(deftest user-bound-restart-test
  (set stdin-meta.read (fn [] "restart"))
  (assert-eq :ok (with-no-stderr
                  (restart-case (invoke-debugger :foo)
                    (:restart [] :ok)))))

(deftest first-non-unique-restart-test
  (set stdin-meta.read (fn [] "restart"))
  (assert-eq :ok (with-no-stderr
                  (restart-case
                      (restart-case (invoke-debugger :foo)
                        (:restart [] :ok)
                        (:restart [] :err))
                    (:restart [] :err)))))

(deftest restart-order-test
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

(deftest interactive-restart-test
  (set stdin-meta.read (fn [] "1"))
  (assert-eq [1 :ok] (with-no-stderr
                      (restart-case (invoke-debugger :foo)
                        (:restart [a] [a :ok])))))

(deftest error-during-interactive-restart-test
  (fn escape [p]
    (p:gsub "([][().%+-*?$^])" "%%%1"))
  (var i 0)
  (set stdin-meta.read (fn []
                         (set i (+ i 1))
                         (. ["1" "a" "2" "1" "42"] i)))
  (assert-eq [42 :ok] (with-no-stderr
                       (restart-case (invoke-debugger :foo)
                         (:restart [a] [a :ok]))))
  (set i 0)
  (assert-is (string.match
              (with-stderr-to-str
               (restart-case (invoke-debugger :foo)
                 (:restart [a] "restart-doc" [a :ok])))
              (.. (escape "Debugger was invoked on unhandled condition: \"foo\"
restarts (invokable by number or by name):
  1: [restart] restart-doc
  2: [throw  ] Throw condition as a Lua error
debugger>> Provide inputs for restart (args: [a]) (^D to cancel)
debugger:restart>> Level 2 debugger was invoked on unhandled condition:")
                  ".*"
                  (escape "restarts (invokable by number or by name):
  1: [restart] restart-doc
  2: [cancel ] Return to level 1 debugger
  3: [throw  ] Throw condition as a Lua error
debugger>> restarts (invokable by number or by name):
  1: [restart] restart-doc
  2: [throw  ] Throw condition as a Lua error
debugger>> Provide inputs for restart (args: [a]) (^D to cancel)
debugger:restart>> "))))
  (when _G.utf8
    (tset _G.utf8 :len nil)
    (set i 0)
    (assert-is (string.match
                (with-stderr-to-str
                 (restart-case (invoke-debugger :bar)
                   (:restart [a] "restart-doc" [a :ok])))
                (.. (escape "Debugger was invoked on unhandled condition: \"bar\"
restarts (invokable by number or by name):
  1: [restart] restart-doc
  2: [throw  ] Throw condition as a Lua error
debugger>> Provide inputs for restart (args: [a]) (^D to cancel)
debugger:restart>> Level 2 debugger was invoked on unhandled condition:")
                    ".*"
                    (escape "restarts (invokable by number or by name):
  1: [restart] restart-doc
  2: [cancel ] Return to level 1 debugger
  3: [throw  ] Throw condition as a Lua error
debugger>> restarts (invokable by number or by name):
  1: [restart] restart-doc
  2: [throw  ] Throw condition as a Lua error
debugger>> Provide inputs for restart (args: [a]) (^D to cancel)
debugger:restart>> "))))))

(deftest throw-from-second-level-test
  (var i 0)
  (set stdin-meta.read (fn []
                         (set i (+ i 1))
                         (. ["1" "a" "3" "2"] i)))
  (assert-eq false (pcall #(with-no-stderr
                            (restart-case (invoke-debugger :foo)
                              (:restart [a] [a :ok]))))))

(deftest nested-scopes-test
  (testing "throwing from nested scope"
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
