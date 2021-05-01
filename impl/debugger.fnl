(local {: eval} (require :fennel))
(local {: get-data : get-name : compose-error-message : build-arg-str : pack :unpack _unpack}
  (require (.. (or (: (or ... "") :gsub "impl%.debugger$" "") "")
               :impl.utils)))

(fn flatten-restarts [restarts scope]
  (if scope
      (let [ordered []]
        (each [name {: n &as restart} (pairs scope.restarts)]
          (tset ordered n (doto restart (tset :target scope.target))))
        (each [_ restart (ipairs ordered)]
          (table.insert restarts restart))
        (flatten-restarts restarts scope.parent))
      restarts))

(fn take-action [{: name : restart : interactive? : docstring : args : target}]
  (let [data (if interactive?
                 (do (io.stderr:write
                      "Provide inputs for "
                      (if args
                          (.. name " (args: [" (table.concat args " ") "])")
                          name)
                      " (^D to cancel)\n"
                      "debugger:" name ">> ")
                     (match (io.stdin:read "*l")
                       input (pack (restart (eval (.. "(values " input ")"))))
                       _ (do (io.stderr:write "\n")
                             (error :cancel))))
                 (pack (restart)))]
    {:state :restarted
     : data
     : target}))

(fn display-restart-prompt [restarts]
  (let [slength (or (?. _G :utf8 :len)
                    #(length $))
        seen {}]
    (var longest 0)
    (each [_ {: name} (ipairs restarts)]
      (let [len (slength name)]
        (when (> len longest)
          (set longest len))))
    (each [i {: name : description} (ipairs restarts)]
      (let [uniq-name (when (not (. seen name))
                        (tset seen name true)
                        name)
            pad (string.rep " " (- longest (slength (or uniq-name ""))))]
        (io.stderr:write
         i
         (if uniq-name (.. ": [" uniq-name pad "] ") (.. ":  " pad "  "))
         (if description (description:gsub "\n" " ") name) "\n")))
    (io.stderr:write "debugger>> ")))

(fn restart-menu [invoke-debugger restarts scope level]
;;; Reads user input, asserts if it is a number.  If not, asks input
;;; again.  The last restart is always non-interactive internal
;;; restart defined by the debugger.  Other restarts are considered
;;; interactive.
  (display-restart-prompt restarts)
  (let [named {}
        _ (each [_ {: name &as restart} (ipairs restarts)]
            (when (not (. named name))
              (tset named name restart)))
        n (length restarts)
        input (io.stdin:read "*l")]
    (match (tonumber input)
      ;; "throw" restart
      n (let [{: restart} (. restarts n)]
          (restart))
      ;; user-defined restarts
      (where action (<= 1 action (- n 1)))
      (match (pcall take-action (. restarts action))
        (true res) (error res)
        (false :cancel) (restart-menu invoke-debugger restarts scope level)
        (false res) (match (invoke-debugger res scope (+ (or level 1) 1))
                      {:cancel true} (restart-menu invoke-debugger restarts scope level)
                      _ _))
      nil
      (match (. named input)
        restart (match (pcall take-action restart)
                  (true res) (error res)
                  (false :cancel) (restart-menu invoke-debugger restarts scope level)
                  (false res) (match (invoke-debugger res scope (+ (or level 1) 1))
                                {:cancel true} (restart-menu invoke-debugger restarts scope level)
                                _ _))
        _ (do (when (= nil input)
                (io.stderr:write "\n"))
              (io.stderr:write "Wrong action. Use number from 1 to " n
                               " or restart name.\n")
              (restart-menu invoke-debugger restarts scope level))))))

(fn invoke-debugger [condition-object scope level]
  "Invokes interactive debugger for given `condition-object'.  Accepts
`scope' with bound restarts, and optional `level', indicating current
debugger depth.

Restarts in the menu are ordered by their definition order and dynamic
scope depth.  Restarts can be called by their number in the menu or
the name in square brackets.  For example, if `restart-case` defines
two restarts `:a` and `:b` and outer `restart-case` bounds restarts
`:a` and `:c` the following menu will be printed:

```
Debugger was invoked on unhandled condition:
1: [a    ] a
2: [b    ] b
3:         a
4: [c    ] c
5: [throw] Throw condition as a Lua error
debugger>>
```

If restart function has a docstring, it is printed after the square
brackets.  If no docstring is found, restart name is printed.

If restart accepts any arguments, a second prompt will be entered when
restart is chosen from the menu.  Above the prompt a hint with
argument names will be displayed.  In this prompt arguments to the
restart are provided as expressions separated by space:

```
Provide inputs for some-restart (args: [some-arg other-arg]) (^D to cancel)
debugger:some-restart>> (+ 1 2 3) {:some :table}
```

Debugger doesn't know anything about the environment, or variables, so
in this prompt only fully realized values can be used.

If an error happens during restart call, debug level increases, and new
`cancel` restart is added to the menu, that allows returning to
previous debug level."
  (let [restarts (flatten-restarts [] scope)]
    (when level
      (table.insert restarts
                    {:name "cancel"
                     :restart #{:cancel true}
                     :description (.. "Return to level " (- level 1) " debugger")}))
    (table.insert restarts
                  {:name "throw"
                   :restart #(error (compose-error-message condition-object) 2)
                   :description "Throw condition as a Lua error"})
    (io.stderr:write
     (if level
         (.. "Level " level " debugger")
         "Debugger")
     " was invoked on unhandled condition: "
     (get-name condition-object)
     (match (get-data condition-object)
       (where args (> args.n 0))
       (.. ", raised with the following arguments: "
           (build-arg-str ", " args))
       _ "")
     "\n")
    (restart-menu invoke-debugger restarts scope level)))

{: invoke-debugger}
