(local {: raise
        : invoke-restart
        : pack
        : invoke-debugger
        : find-restart
        : Condition
        : Warning
        : Error
        : dynamic-scope}
  ;; Constructing relative path
  (require (.. (or (and ... (not= ... :init) (.. ... ".")) "")
               :impl.condition-system)))

(fn error* [condition-object]
  "Raise `condition-object' as an error.

This function is a drop-in replacement for the inbuilt `error`
function.  Similarly to Lua's `error` accepting message as its first
argument, this function accepts condition object as it's first
argument, although it ignores the second argument, because the
throwing semantics are different.  Like Lua's `error', this function
will interrupt function execution where it was called, and no code
after `error' will be executed.  If no handler bound for raised
condition, it is promoted to a Lua error with detailed message about
the unhandled condition and it's arguments, if any.

```
>> (error :condition-object)
runtime error: condition condition-object was raised
stack traceback...
```

Conditions support inheritance, and all conditions that are raised
with the `error' function automatically derive from
`Error` condition, and can be catched with handler
bound to this condition handlers.

And all conditions automatically derive from
`Condition` condition.

Any Lua object can be a condition, and such conditions are handled by
reference.  If more complex inheritance rules are required,
`define-condition' and `make-condition' can be used.

# Examples

Condition is thrown as a Lua error if not handled, and can be caught
with `pcall`:

``` fennel
(assert-not (pcall error :error-condition))
```

Conditions can be handled with `handler-case':

``` fennel
(define-condition some-error)

(fn handle-condition []
  (handler-case (error (make-condition some-error 42))
    (some-error [_ x] x)))

(assert-eq 42 (handle-condition))
```

Conditions also can be recovered with `handler-bind' and
`restart-case' by using `invoke-restart'.  Error recovery code doesn't
necessary need to be a part of the same lexical scope:

``` fennel
(define-condition some-error)

(fn recover-from-error []
  (restart-case (error (make-condition some-error 32))
    (:use-value [x] x)))

(handler-bind [some-error
               (fn [_ x]
                 (invoke-restart :use-value (+ x 10)))]
  (assert-eq 42 (recover-from-error)))
```

Error conditions, and Lua errors can be handled by binding a handler
to `Error` and `Condition`
conditions:

``` fennel
(assert-eq 27 (handler-case (error :some-error-condition)
                (Condition [] 27)))

(assert-eq 42 (handler-case (/ 1 nil)
                (Error [] 42)))
```"
  (raise :error condition-object))

(fn signal [condition-object]
  "Raise `condition-object' as a signal.

Raises given condition as a signal.  Signals can be handled with the
same ways as `error' conditions, but don't promote to errors if no
handler was found.  This function transfers control flow to the
handler at the point where it was called but will continue execution
if handler doesn't transfer control flow.

Signals derive from `Condition`, and can be catched
with this handler.

# Examples

Signal is ignored if not handled:

``` fennel
(assert-eq nil (signal :signal-condition))
```

Handler doesn't transfer control flow, and code evaluation continues
after `signal' call:

``` fennel
(local result [])

(handler-bind [:some-signal
               (fn [] (table.insert result 2))]
  (table.insert result 1)
  (signal :some-signal)
  (table.insert result 3))

(assert-eq [1 2 3] result)
```

See `error' for more examples on how to handle conditions."
  (raise :condition condition-object))

(fn warn [condition-object]
  "Raise `condition-object' as a warning.

Warnings are not thrown as errors when no handler is bound but their
message is printed to standard error out.  Same to `signal', the
control is temporarily transferred to handler, but code evaluation
continues if handler did not transferred control flow.

Warnings derive from both `Warning` and
`Condition`, and can be catched with any of these
handlers.

# Examples

Warning is ignored if not handled:

``` fennel
(assert-eq nil (warn :warn-condition))
```

See `error' for examples how to handle conditions."
  (raise :warning condition-object))

(fn make-condition [condition-object arg1 ...]
  "Derives condition from base `condition-object'.  Accepts any amount
of additional arguments that will be passed as arguments to handlers
when handling this condition instance.

Condition created with `define-condition` and derived condition are
different objects, but the condition system sees those as the same
condition.

# Examples

Defining a condition, and making instance of this condition with two
arguments, and registering the handler for the original condition
object:

``` fennel
(define-condition some-condition)

(handler-case
    (error (make-condition some-condition {:foo \"bar\"} 42))
  (some-condition [cond foo-bar forty-two leet]
    (assert-ne some-condition cond)
    (assert-eq {:foo \"bar\"} foo-bar)
    (assert-eq 42 forty-two)))
```"
  (assert (and (= :table (type condition-object))
               (= :condition condition-object.type)
               (not= nil condition-object.id))
          "condition must derive from existing condition object")
  {:data (if arg1 (pack arg1 ...))
   :id condition-object.id
   :parent condition-object.parent
   :type :condition})

(fn invoke-restart* [restart-name ...]
  "Invoke restart `restart-name' to handle a condition.

Must be used only within the dynamic scope of `restart-case'.
Transfers control flow to handler function when executed.

# Examples

Handle the `error' with the `:use-value' restart:

``` fennel
(define-condition error-condition)

(fn handle-error []
  (handler-bind [error-condition
                 (fn [_c x]
                   (invoke-restart :use-value (+ x 10))
                   (print \"never prints\"))]
    (restart-case (do (error (make-condition error-condition 32))
                      (print \"also never prints\"))
      (:use-value [x] x))))

(assert-eq 42 (handle-error))
```

See `error' for examples how to handle conditions."
  (invoke-restart restart-name ...))

(fn invoke-debugger* [condition-object]
  "Invokes debugger for given `condition-object` to call restarts from
the interactive menu."
  (invoke-debugger condition-object))

(fn continue []
  "Invoke the `continue' restart, which is bound automatically by `cerror' macro.

Must be used only within the dynamic scope of `restart-case'.
Transfers control flow to handler function when executed."
  (invoke-restart :continue))

(fn find-restart* [restart-name]
  "Searches `restart-name' in the dynamic scope.

If restart is found, returns its name."
  (when (find-restart
         restart-name
         (?. dynamic-scope
             (or (and coroutine
                      coroutine.running
                      (tostring (coroutine.running)))
                 :main)
             :restarts))
    restart-name))

(setmetatable
 {:error error*
  : signal
  : warn
  : make-condition
  :find-restart find-restart*
  :invoke-restart invoke-restart*
  :invoke-debugger invoke-debugger*
  : continue
  : Condition
  : Warning
  : Error}
 {:__index
  {:_DESCRIPTION "Condition system for the Fennel language.

This module provides a set of functions for control transfer, that
implement Common Lisp-inspired condition system for the Fennel
language."
   :_MODULE_NAME "fennel-conditions"}})

; LocalWords:  unhandled
