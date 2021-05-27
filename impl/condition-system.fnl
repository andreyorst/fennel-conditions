(local module (: (or ... "") :gsub "impl%.condition%-system$" ""))
(local {: metadata : view : eval} (require :fennel))

(local dynamic-scope
  (metadata:set
   {}
   :fnl/docstring
   "Dynamic scope for the condition system.

Dynamic scope is a maintained table where handlers and restarts are
stored thread-locally.  Thread name is obtained with
`coroutine.running` call and each thread holds a table with the
following keys `:handlers`, `:restarts`, and `:current-context`.
Handlers and restarts itselves are tables."))

;;; Utils

(macro current-thread []
  ;; Returns the name of current thread when possible
  `(or (and coroutine
            coroutine.running
            (tostring (coroutine.running)))
       :main))

(fn get-name [condition-object]
  ;; Extracts name string from `condition-object'.
  ;;
  ;; # Examples
  ;;
  ;; Condition objects return base condition name:
  ;;
  ;; ``` fennel
  ;; (define-condition simple-error)
  ;; (assert-eq
  ;;  :simple-error
  ;;  (get-name (make-condition simple-error -1)))
  ;; ```
  ;;
  ;; Primitive objects are transformed with `tostring`:
  ;;
  ;; ``` fennel
  ;; (assert-eq
  ;;  \"-1\"
  ;;  (get-name -1))
  (if (and (= :table (type condition-object))
           (= condition-object.type :condition))
      (tostring condition-object.id.name)
      (view condition-object)))

(fn get-data [condition-object]
  ;; Extracts data from `condition-object'.
  ;;
  ;; # Examples
  ;;
  ;; Extracting data set with `make-condition' function:
  ;;
  ;; ``` fennel
  ;; (define-condition simple-error)
  ;; (assert-eq
  ;;  {1 :a 2 :b :n 2}
  ;;  (get-data (make-condition simple-error :a :b)))
  ;; ```
  ;;
  ;; Primitive objects return table with `:n` set to 0:
  ;;
  ;; ``` fennel
  ;; (assert-eq
  ;;  {:n 0}
  ;;  (get-data :simple-condition))
  ;; ```
  (match (and (= :table (type condition-object))
              (= condition-object.type :condition)
              condition-object.data)
    (where data data) data
    _ {:n 0}))

(fn build-arg-str [sep args]
  ;; Constructs the string of arguments pretty-printed values stored
  ;; in `args', separated by `sep'.
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
        s (.. " with the following arguments: " s))))

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
        (match scope.restart
          restart (table.insert restarts
                                (doto restart (tset :target scope.target))))
        (flatten-restarts restarts scope.parent))
      restarts))

(fn take-action [{: name : restart : args : target : builtin?}]
  (if builtin?
      (_G.error {:builtin? true :data (restart)})
      args
      (do (io.stderr:write
           "Provide inputs for " name
           " (args: [" (table.concat args " ") "]) (^D to cancel)\n"
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

(fn restart-menu [restarts prompt? level]
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
          (true restart) (_G.error restart)
          ;; some builtin restart was called
          (false {:builtin? true : data})
          (match data
            ;; exiting the nested debugger session
            {:cancel true} data
            ;; debugger invoked when no handlers were found, throwing error will land on the top level
            {:throw true : message} (if (?. dynamic-scope (current-thread) :restarts :parent)
                                        (_G.error {:state :error : message})
                                        (_G.error message 3)))
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

;;; Default condition objects

(local Condition
  {:name "condition"
   :type "condition"})
(tset Condition :id Condition)
(metadata:set Condition
              :fnl/docstring
              "Condition object that acts as a base for all conditions.")

(local Warning
  {:name "warning"
   :parent Condition
   :type "condition"})
(tset Warning :id Warning)
(metadata:set Warning
              :fnl/docstring
              "Condition object that acts as a base for all warning conditions.")

(local Error
  {:name "error"
   :parent Condition
   :type "condition"})
(tset Error :id Error)
(metadata:set Error
              :fnl/docstring
              "Condition object that acts as a base for all error conditions.")

;;; Handlers

(fn find-parent-handler [condition-object scope]
  ;; Searches handler for `condition-object` parent in current scope
  ;; `scope` only.
  (when condition-object
    (match (?. scope.handler (?. condition-object :id :parent :id))
      handler {: handler : scope}
      nil (find-parent-handler condition-object.parent scope))))

(fn find-object-handler [condition-object type* scope]
  ;; Searches the handler for the `condition-object` in dynamic scope
  ;; `scope`.  If no handler is found in the current scope, searches
  ;; for handlers of all condition object parents.  If no parent
  ;; handler found goes to upper scope.
  (when scope
    (let [h scope.handler]
      (match (or (?. h condition-object.id)
                 (?. h (match type*
                         :error Error
                         :warning Warning))
                 (?. h Condition))
        handler {: handler : scope}
        nil (match (find-parent-handler condition-object scope)
              parent-handler parent-handler
              nil (find-object-handler condition-object type* scope.parent))))))

(fn find-primitive-handler [condition-object type* scope]
  ;; Checks is object is present in dynamic scope and a handler is
  ;; bound to it.
  (when scope
    (let [h scope.handler]
      (match (or (?. h condition-object)
                 (?. h (match type*
                         :error Error
                         :warning Warning))
                 (?. h Condition))
        handler {: handler : scope}
        nil (find-primitive-handler condition-object type* scope.parent)))))

(fn find-handler [condition-object type* scope]
  "Finds the `condition-object' handler of `type*` in dynamic scope
`scope`.  If `condition-object' is a table with `type` key equal to
`:condition` searches handler based on condition object's inheritance.
If anything else, searches handler by object reference."
  (if (and (= :table (type condition-object))
           (= :condition condition-object.type))
      (find-object-handler condition-object type* scope)
      (find-primitive-handler condition-object type* scope)))

(fn handle [condition-object type* ?scope]
  "Handle the `condition-object' of `type*' and optional `?scope`.

Finds the `condition-object' handler in the dynamic scope.  If found,
calls the handler, and returns a table with `:state` set to
`:handled`, and `:handler` bound to an anonymous function that calls
the restart."
  (let [thread (current-thread)
        thread-scope (. dynamic-scope thread)]
    (match (find-handler
            condition-object
            type*
            (or ?scope thread-scope.handlers))
      {: handler : scope}
      (do (tset thread-scope :current-context scope)
          (match scope.handler-type
            :handler-case {:state :handled
                           :handler #(handler condition-object (_unpack (get-data condition-object)))
                           :target scope.target
                           :condition-object condition-object
                           :type type*}
            :handler-bind (do (handler condition-object (_unpack (get-data condition-object)))
                              (handle condition-object type* scope.parent))
            _ {:state :error
               :message (.. "wrong handler-type: " (view _))
               :condition condition-object}))
      _ {:state :error
         :message (.. "no handler bound for condition: "
                      (get-name condition-object))
         :condition condition-object})))


;;; Restarts

(fn find-restart [restart-name scope]
  "Searches `restart-name' in dynamic scope `scope`."
  (when scope
    (match (?. scope :restart)
      (where restart (= restart.name restart-name))
      (values restart.restart scope.target)
      _ (find-restart restart-name scope.parent))))

(fn invoke-restart [restart-name ...]
  "Searches for `restart-name' in the dynamic scope and invokes the
restart with given arguments.  Always throws error, as
`invoke-restart' must transfer control flow out of the handler.  If
restart is found, calls the restart function and returns a table with
`:state` set to `:restarted`, and `:restart` bound to the restart
function."
  (let [args (pack ...)
        thread-scope (. dynamic-scope (current-thread))]
    (_G.error (match (find-restart restart-name thread-scope.restarts)
                (restart target) {:state :restarted
                                  :restart #(restart (_unpack args))
                                  :target target}
                _ (let [msg (.. "restart " (view restart-name) " is not found")]
                    (if thread-scope.current-context
                        {:state :error
                         :message msg}
                        msg))) 2)))

;;; Conditions

(fn raise-condition [condition-object type*]
  ;; Raises `condition-object' as a condition of given `type*`, or
  ;; `:condition` if `type*` is not specified.  Conditions of types
  ;; `:condition` and `:warn` do not interrupt program flow, but still
  ;; can be handled.
  (match (handle condition-object (or type* :condition))
    (where (or {:state :handled &as res}
               {:state :restarted &as res}))
    (_G.error res 2)
    _ nil))

(fn raise-warning [condition-object]
  ;; Raises `condition-object' as a warning.  If condition was not
  ;; handled, prints the warning message to stderr, and continues.
  (match (raise-condition condition-object :warning)
    nil (do (io.stderr:write "WARNING: "
                             (compose-error-message condition-object)
                             "\n")
            nil)))

(fn raise-error [condition-object]
  ;; The only raise that always throws it's result as an error when
  ;; condition was not handled, unless
  ;; `condition-system-use-debugger?' is not set to logical true.  If
  ;; `condition-system-use-debugger?' is `true`, invokes the
  ;; interactive debugger.
  (match (raise-condition condition-object :error)
    nil (if _G.condition-system-use-debugger?
            (invoke-debugger condition-object)
            (_G.error (compose-error-message condition-object) 2))))

(fn raise [condition-type condition-object]
  "Raises `condition-object' as a condition of `condition-type'.
`condition-object' must not be `nil'."
  (assert (not= nil condition-object)
          "condition must not be nil")
  ;; If condition was raided inside handler we need to unwind the
  ;; stack to the point where we were in the handler.  Each `handle`
  ;; invocation sets the `current-context` field, and this field is
  ;; cleared when we exit the handler.
  (let [thread (current-thread)
        thread-scope (do (when (not (. dynamic-scope thread))
                           (tset dynamic-scope thread {:handlers {} :restarts {}}))
                         (. dynamic-scope thread))]
    (match thread-scope.current-context
      scope (let [target scope.target]
              (var scope scope.parent)
              (while (and scope (= target scope.target))
                (set scope scope.parent))
              (tset thread-scope :handlers scope)))
    (match condition-type
      :condition (raise-condition condition-object)
      :warning (raise-warning condition-object)
      :error (raise-error condition-object))))

(fn condition= [c1 c2]
  "Compare `c1` and `c2` condition objects by their `id` field."
  (and (= c1.type :condition)
       (= c2.type :condition)
       (rawequal c1.id c2.id)))

(setmetatable
 {: raise
  : invoke-restart
  : handle
  : find-handler
  : find-restart
  : invoke-debugger
  : compose-error-message
  :pack (metadata:set pack :fnl/docstring "Portable `table.pack` implementation.")
  :unpack _unpack
  : dynamic-scope
  : Condition
  : Error
  : Warning
  : condition=}
 {:__index {:_DESCRIPTION "Condition system for Fennel language.

This module is library's private API that provides functions meant for
internal use only.  For public API docs see
[fennel-conditions.md](../fennel-conditions.md) and
[macros.md](../macros.md)"}})
