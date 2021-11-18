(local {: eval} (require :fennel))
(local {: dynamic-scope
        : current-thread
        : get-data
        : get-name
        : compose-error-message
        : unpack
        : pack
        : build-arg-str}
  (require (: (or ... "") :gsub "(impl%.)debugger$" "%1utils")))

(fn flatten-restarts [restarts scope]
  (if scope
      (let [ordered []]
        (match scope.restart
          restart (table.insert restarts
                                (doto restart (tset :target scope.target))))
        (flatten-restarts restarts scope.parent))
      restarts))

(fn take-action [{: name : restart : args : target : builtin?}]
  (if builtin?
      (error {:builtin? true :data (restart)})
      args
      (do (io.stderr:write
           "Provide inputs for " name
           " (args: [" (table.concat args " ") "]) (^D to cancel)\n"
           "debugger:" name ">> ")
          (match (io.stdin:read "*l")
            input (let [args (pack (eval (.. "(values " input ")")))]
                    {:state :restarted
                     :restart #(restart (unpack args))
                     :target target})
            _ (do (io.stderr:write "\n") nil)))
      {:state :restarted
       : restart
       : target}))

(fn longest-name-lenght [length-fn restarts]
  ;; Computes the longest restart name lenght
  (var longest 0)
  (each [_ {: name} (ipairs restarts)]
    (let [len (length-fn name)]
      (when (> len longest)
        (set longest len))))
  longest)

(fn display-restart-prompt [restarts]
  ;; Prints restart prompt.  Unique restart names are shown inside
  ;; square brackets, and can be called by name.
  (let [slength (or (?. _G :utf8 :len) #(length $))
        max-name-width (longest-name-lenght slength restarts)
        max-number-width (-> restarts length tostring length)
        seen {}]
    (io.stderr:write "restarts (invokable by number or by name):\n")
    (each [i {: name : description} (ipairs restarts)]
      (let [uniq-name (when (not (. seen name))
                        (tset seen name true)
                        name)
            pad (string.rep " " (- max-name-width (slength (or uniq-name ""))))
            restart-name (if uniq-name
                             (.. "[" uniq-name pad "] ")
                             (.. pad "   "))
            number-pad (string.rep " " (+ 1 (- max-number-width (length (tostring i)))))]
        (io.stderr:write
         "  " i ":" number-pad restart-name
         (if description
             (description:gsub "\n" " ")
             name)
         "\n")))))

;; forward declaration
(var invoke-debugger nil)

(fn restart-menu [restarts prompt? level scope]
  ;; Interactive restart menu.  Restarts are displayed with the
  ;; following format: `number: [name] name-or-docstring`, where
  ;; `name` is a unique restart name in current menu.  Restarts can be
  ;; called either by their number or unique name.  If restart accepts
  ;; arguments, the second prompt will be displayed with a hint on
  ;; what arguments are accepted by the restart.  If error occurs
  ;; during input phase, second level of debugger is entered.
  (when prompt?
    (display-restart-prompt restarts))
  (io.stderr:write "debugger>> ")
  (let [named {}
        _ (each [_ {: name &as restart} (ipairs restarts)]
            (when (not (. named name))
              (tset named name restart)))
        input (io.stdin:read "*l")
        action (. restarts (tonumber input))
        named-action (. named input)]
    (if (or action named-action)
        (match (pcall take-action (or action named-action))
          (true nil) (restart-menu restarts nil level) ; no user input provided
          ;; Restart was found and no errors happened up to the restart call
          (true restart) (error restart)
          ;; some builtin restart was called
          (false {:builtin? true : data})
          (match data
            ;; exiting the nested debugger session
            {:cancel true} data
            ;; debugger invoked when no handlers were found, throwing error will land on the top level
            {:throw true : message} (if (?. dynamic-scope (current-thread) :restarts :parent)
                                        (error {:state :error : message})
                                        (error message 3)))
          ;; error happened during argument providing (likely). Entering nested debug session
          (false res) (match (invoke-debugger res (+ (or level 1) 1))
                        {:cancel true} (restart-menu restarts true level)))
        (do (if (= nil input)
                (io.stderr:write "\n")
                (io.stderr:write "Wrong action. Use number from 1 to " (length restarts)
                                 " or restart name.\n"))
            (restart-menu restarts nil level)))))

(fn invoke-debugger* [condition-object level]
  "Invokes interactive debugger for given `condition-object'.  Accepts
optional `level', indicating current debugger depth.

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

If an error happens during restart call, debug level increases, and
new `cancel` restart is added to the menu, that allows returning to
previous debug level."
  (let [thread (current-thread)]
    (when (= nil (. dynamic-scope thread))
      (tset dynamic-scope thread {:handlers {}
                                  :restarts {}}))
    (let [restarts (flatten-restarts [] (. dynamic-scope thread :restarts))]
      (when level
        (table.insert restarts
                      {:name :cancel
                       :restart #{:cancel true}
                       :builtin? true
                       :description (.. "Return to level " (- level 1) " debugger")}))
      (table.insert restarts
                    {:name :throw
                     :builtin? true
                     :restart #{:throw true :message (compose-error-message condition-object)}
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
      (restart-menu restarts true level))))

(set invoke-debugger invoke-debugger*)

{: invoke-debugger}
