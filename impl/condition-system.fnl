(local module (: (or ... "") :gsub "impl%.condition%-system$" ""))
(local {: metadata : view : eval} (require :fennel))

;;; Utils

(fn get-name [condition-object]
  "Extracts name string from `condition-object'.

# Examples
Condition objects return base condition name:

``` fennel
(define-condition simple-error)
(assert-eq
 :simple-error
 (get-name (make-condition simple-error -1)))
```

Primitive objects are transformed with `tostring`:

``` fennel
(assert-eq
 \"-1\"
 (get-name -1))
```"
  (if (and (= :table (type condition-object))
           (= condition-object.type :condition))
      (tostring condition-object.id.name)
      (tostring condition-object)))

(fn get-data [condition-object]
  "Extracts data from `condition-object'.

# Examples
Extracting data set with `make-condition' function:

``` fennel
(define-condition simple-error)
(assert-eq
 {1 :a 2 :b :n 2}
 (get-data (make-condition simple-error :a :b)))
```

Primitive objects return table with `:n` set to 0:

``` fennel
(assert-eq
 {:n 0}
 (get-data :simple-condition))
```"
  (match (and (= :table (type condition-object))
              (= condition-object.type :condition)
              condition-object.data)
    (where data data) data
    _ {:n 0}))

(fn build-arg-str [sep args]
  "Constructs the string of arguments pretty-printed values stored in
`args', separated by `sep'."
  (let [res []]
    (for [i 1 args.n]
      (table.insert res (view (. args i) {:one-line? true})))
    (table.concat res sep)))

(fn compose-error-message [condition-object]
  "Composes message for `condition-object' based on it's name and data
stored within the object.

# Examples
Conditions without data produce short messages:

``` fennel
(define-condition simple-error)
(assert-eq
 \"condition simple-error was raised\"
 (compose-error-message simple-error))
```

Conditions with data produce extended messages:

``` fennel
(define-condition simple-error)
(assert-eq
 \"condition simple-error was raised with the following arguments: 1, 2, 3\"
 (compose-error-message (make-condition simple-error 1 2 3)))
```"
  (.. "condition " (get-name condition-object)
      " was raised"
      (match (build-arg-str ", " (get-data condition-object))
        "" ""
        s (.. " with the following arguments: " s)
        _ "")))

(fn _unpack [tbl]
  "Automatically try to query `tbl` for it's size `n` and unpack whole
thing."
  (let [len (or tbl.n (length tbl))
        unpack-fn (or table.unpack _G.unpack)]
    (unpack-fn tbl 1 len)))

(local pack (or table.pack #(doto [$...] (tset :n (select :# $...)))))

;;; Debugger

(fn flatten-restarts [restarts scope]
  (if scope
      (let [ordered []]
        (each [name {: n &as restart} (pairs scope.restarts)]
          (tset ordered n (doto restart (tset :target scope.target))))
        (each [_ restart (ipairs ordered)]
          (table.insert restarts restart))
        (flatten-restarts restarts scope.parent))
      restarts))

(fn take-action [{: name : restart : interactive? : args : target : builtin?}]
  (if builtin?
      (error {:builtin? true :data (restart)})
      interactive?
      (do (io.stderr:write
           "Provide inputs for "
           (if args
               (.. name " (args: [" (table.concat args " ") "])")
               name)
           " (^D to cancel)\n"
           "debugger:" name ">> ")
          (match (io.stdin:read "*l")
            input (let [args (pack (eval (.. "(values " input ")")))]
                    {:state :restarted
                     :restart #(restart (_unpack args))
                     :target target})
            _ (do (io.stderr:write "\n") nil)))
      {:state :restarted
       : restart
       : target}))

(fn builtin-names-transform [name]
  (let [names {:fennel-conditions/continue :continue
               :fennel-conditions/throw :throw
               :fennel-conditions/cancel :cancel}]
    (match (. names name)
      nname nname
      _ name)))

(fn longest-name-lenght [length-fn restarts]
;;; Computes the longest restart name lenght
  (var longest 0)
  (each [_ {: name} (ipairs restarts)]
    (let [len (length-fn (builtin-names-transform name))]
      (when (> len longest)
        (set longest len))))
  longest)

(fn display-restart-prompt [restarts]
;;; Prints prestart prompt.  Unique restart names are shown inside
;;; square brackets, and can be called by name.
  (let [slength (or (?. _G :utf8 :len) #(length $))
        max-name-width (longest-name-lenght slength restarts)
        max-number-width (-> restarts length tostring length)
        seen {}]
    (io.stderr:write "restarts (invokable by number or by name):\n")
    (each [i {: name : description} (ipairs restarts)]
      (let [name (builtin-names-transform name)
            uniq-name (when (not (. seen name))
                        (tset seen name true)
                        name)
            pad (string.rep " " (- max-name-width (slength (or uniq-name ""))))
            number-pad (string.rep " " (+ 1 (- max-number-width (length (tostring i)))))]
        (io.stderr:write
         i
         (if uniq-name
             (.. ":" number-pad "[" uniq-name pad "] ")
             (.. ":" number-pad pad "   "))
         (if description
             (description:gsub "\n" " ")
             name)
         "\n")))))

(fn restart-menu [invoke-debugger restarts scope prompt? level]
;;; Interactive restart menu.  Restarts are displayed with the
;;; following format: `number: [name] name-or-docstring`, where `name`
;;; is a unique restart name in current menu.  Restarts can be called
;;; either by their number or unique name.  If restart accepts
;;; arguments, the second prompt will be displayed with a hint on what
;;; arguments are accepted by the restart.  If error occurs during
;;; input phase, second level of debugger is entered.
  (when prompt?
    (display-restart-prompt restarts))
  (io.stderr:write "debugger>> ")
  (let [named {}
        _ (each [_ {: name &as restart} (ipairs restarts)]
            (let [name (builtin-names-transform name)]
              (when (not (. named name))
                (tset named name restart))))
        input (io.stdin:read "*l")
        action (. restarts (tonumber input))
        named-action (. named input)]
    (if (or action named-action)
        (match (pcall take-action (or action named-action))
          (true nil) (restart-menu invoke-debugger restarts scope nil level) ; no user input provided
          ;; Restart was found and no errors happened up to the restart call
          (true restart) (error restart)
          ;; some builtin restart was called
          (false {:builtin? true : data})
          (match data
            ;; exiting the nested debugger session
            {:cancel true} data
            ;; debugger invoked when no handlers were found, throwing error will land on the top level
            {:throw true : message} (_G.error message 3))
          ;; error happened during argument providing (likely). Entering nested debug session
          (false res) (match (invoke-debugger res scope (+ (or level 1) 1))
                        {:cancel true} (restart-menu invoke-debugger restarts scope true level)
                        _ _))
        (do (if (= nil input)
                (io.stderr:write "\n")
                (io.stderr:write "Wrong action. Use number from 1 to " (length restarts)
                                 " or restart name.\n"))
            (restart-menu invoke-debugger restarts scope nil level)))))

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
                    {:name :fennel-conditions/cancel
                     :restart #{:cancel true}
                     :builtin? true
                     :description (.. "Return to level " (- level 1) " debugger")}))
    (table.insert restarts
                  {:name :fennel-conditions/throw
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
    (restart-menu invoke-debugger restarts scope true level)))


;;; Private library API

(local condition-system
  {:compose-error-message compose-error-message
   :pack (metadata:set pack :fnl/docstring "Portable `table.pack` implementation.")
   :unpack _unpack})


;;; Handlers

(set condition-system.handlers
     (metadata:set {:handlers {}
                    :parent nil}
                   :fnl/docstring "Dynamic scope for condition handlers."))

(fn find-parent-handler [condition-object scope]
;;; Searches handler for `condition-object` parent in current scope
;;; `scope` only.
  (when condition-object
    (match (. scope.handlers (?. condition-object :id :parent :id))
      handler {: handler :target scope.target}
      nil (find-parent-handler condition-object.parent scope))))

(fn find-object-handler [condition-object type* scope]
;;; Searches the handler for the `condition-object` in dynamic scope
;;; `scope`.  If no handler is found in the current scope, searches
;;; for handlers of all condition object parents.  If no parent
;;; handler found goes to upper scope.
  (when scope
    (match (or (. scope.handlers condition-object.id)
               (. scope.handlers (.. :fennel-conditions/ type*))
               (. scope.handlers :fennel-conditions/condition))
      handler {: handler :target scope.target}
      nil (match (find-parent-handler condition-object scope)
            parent-handler parent-handler
            nil (find-object-handler condition-object type* scope.parent)))))

(fn find-primitive-handler [condition-object type* scope]
;;; Checks is object is present in dynamic scope and a handler is
;;; bound to it.
  (when scope
    (match (or (. scope.handlers condition-object)
               (. scope.handlers (.. :fennel-conditions/ type*))
               (. scope.handlers :fennel-conditions/condition))
      handler {: handler :target scope.target}
      nil (find-primitive-handler condition-object type* scope.parent))))

(fn find-handler [condition-object type* scope]
;;; Finds `condition-object' handler in dynamic scope `scope`.  If
;;; `condition-object' is a table with `type` key equal to
;;; `:condition` searches handler based on condition object's
;;; inheritance.  If anything else, searches handler by object
;;; reference.
  (if (and (= :table (type condition-object))
           (= :condition condition-object.type))
      (find-object-handler condition-object type* scope)
      (find-primitive-handler condition-object type* scope)))

(fn condition-system.handle [condition-object type*]
  "Handle the `condition-object' of `type*'.

Finds the `condition-object' handler in the dynamic scope.  If found,
calls the handler, and returns a table with `:state` set to
`:handled`, and `:data` bound to a packed table of handler's return
values."
  (match (find-handler condition-object type* condition-system.handlers)
    {: handler : target}
    {:state :handled
     :data (pack (handler condition-object (_unpack (get-data condition-object))))
     :target target
     :condition-object condition-object
     :type type*}
    _ {:state :error
       :message (.. "no handler bound for condition: "
                    (get-name condition-object))
       :condition condition-object}))


;;; Restarts

(set condition-system.restarts
     (metadata:set {:restarts []
                    :parent nil}
                   :fnl/docstring "Dynamic scope for restarts."))

(fn find-restart [restart-name scope]
;;; Searches `restart-name' in dynamic scope `scope`.  Modifies the
;;; restart object, by setting it's `:target` field to the `scope`
;;; target.
  (when scope
    (match (?. scope :restarts restart-name)
      restart (doto restart (tset :target scope.target))
      nil (find-restart restart-name scope.parent))))

(fn condition-system.invoke-restart [restart-name ...]
  "Searches for `restart-name' in the dynamic scope and invokes the
restart with given arguments.  Always throws error, as
`invoke-restart' must transfer control flow out of the handler.  If
restart is found, calls the restart function and returns a table with
`:state` set to `:restarted`, and `:restart` bound to the restart
function."
  (let [args (pack ...)]
    (error (match (find-restart restart-name condition-system.restarts)
             {: restart : target} {:state :restarted
                                   :restart #(restart (_unpack args))
                                   :target target}
             _ {:state :error
                :message (.. "restart " (view restart-name) " is not found")}) 2)))


;;; Conditions

(fn raise-condition [condition-object type*]
;;; Raises `condition-object' as a condition of given `type*`, or
;;; `:signal` if `type*` is not specified.  Conditions of types
;;; `:signal` and `:warn` do not interrupt program flow, but still can
;;; be handled.
  (match (condition-system.handle condition-object (or type* :condition))
    (where (or {:state :handled &as res}
               {:state :restarted &as res})) (error res)
    _ nil))

(fn raise-warning [condition-object]
;;; Raises `condition-object' as a warning.  If condition was not
;;; handled, prints the warning message to stderr, and continues.
  (match (raise-condition condition-object :warning)
    nil (do (io.stderr:write "WARNING: "
                             (compose-error-message condition-object)
                             "\n")
            nil)))

(fn raise-error [condition-object]
;;; The only raise that always throws it's result as an error when
;;; condition was not handled, unless `use-debugger?' is not set to
;;; logical true.  If `use-debugger?' is `true`, invokes the
;;; interactive debugger.
  (match (raise-condition condition-object :error)
    nil (if _G.fennel-conditions/use-debugger?
            (invoke-debugger condition-object condition-system.restarts)
            (error (compose-error-message condition-object) 2))))

(fn condition-system.raise [condition-type condition-object]
  "Raises `condition-object' as a condition of `condition-type'.
`condition-object' must not be `nil'."
  (assert (not= nil condition-object)
          "condition must not be nil")
  (match condition-type
    :condition (raise-condition condition-object)
    :warning (raise-warning condition-object)
    :error (raise-error condition-object)))

condition-system
