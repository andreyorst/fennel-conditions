(local {: raise
        : invoke-restart
        : find-restart
        : Condition
        : Warning
        : Error
        : condition=}
  ;; Constructing relative path
  (require (.. (or (and ... (not= ... :init) (.. ... ".")) "")
               :impl.condition-system)))

(local {: pack : dynamic-scope}
  (require (.. (or (and ... (not= ... :init) (.. ... ".")) "")
               :impl.utils)))

(local invoke-debugger
  (require (.. (or (and ... (not= ... :init) (.. ... ".")) "")
               :impl.debugger)))

(fn error* [condition-object]
  "Raise `condition-object' as an error.

This function is a drop-in replacement for the inbuilt `error`
function.  Similarly to Lua's `error` accepting message as its first
argument, this function accepts condition object as it's first
argument, although it ignores the second argument, because the
throwing semantics are different.  Like Lua's `error`, this function
will interrupt function execution where it was called, and no code
after `error` will be executed.  If no handler bound for raised
condition, the condition will be promoted to a Lua error with detailed
message about the unhandled condition and it's arguments, if any.

```
>> (error :condition-object)
runtime error: condition condition-object was raised
stack traceback...
```

Condition objects support inheritance, and all conditions that are
raised with the `error' function automatically derive from `Error'
condition, and can be catched with handler bound to this condition
object.

Likewise all conditions automatically derive from `Condition'
condition, which is a base type for all condition objects.

Any Lua object can be a condition, and such conditions are handled by
reference, but still can be handled by binding handler for `Condition'
and `Error' (in case Lua object was raised with the `error` function).
If more complex inheritance rules are required, `define-condition' and
`make-condition' can be used.

# Examples

Condition is thrown as a Lua error if not handled, and can be caught
with `pcall`:

``` fennel
(assert-not (pcall error :error-condition))
```

Error conditions, and Lua errors can be handled by binding a handler
to `Error` and `Condition` conditions via `handler-case':

``` fennel
(assert-eq 27 (handler-case (error :some-error-condition)
                (Condition [] 27)))

(assert-eq 42 (handler-case (/ 1 nil)
                (Error [] 42)))
```

User-defined conditions can be handled by their base type:

``` fennel
(define-condition some-error)

(fn handle-condition []
  (handler-case (error (make-condition some-error 42))
    (some-error [_ x] x)))

(assert-eq 42 (handle-condition))
```

Conditions also can be recovered with `handler-bind' and
`restart-case' by using `invoke-restart'.  Error handling code doesn't
necessary has to be a part of the same lexical scope of where
condition was raised:

``` fennel
(define-condition some-error)

(fn some-function []
  (restart-case (error (make-condition some-error 32))
    (:use-value [x] x)))

(handler-bind [some-error
               (fn [_ x]
                 (invoke-restart :use-value (+ x 10)))]
  (assert-eq 42 (some-function)))
```

In this case, `restart-case' is in the lexical scope of
`some-function`, and `handler-bind' is outside of it's lexical
scope. When `some-function` raises `some-error` condition a handler
bound to this condition is executed. Handler invokes restart named
`:use-value`, which recovers function from error state and function
returns the value provided by the restart."
  (raise :error condition-object))

(fn signal [condition-object]
  "Raise `condition-object' as a signal.

Signals can be handled the same way as `error' conditions, but don't
promote to errors if no handler was found.  This function transfers
control flow to the handler at the point where it was called but will
continue execution if handler itself doesn't transfer control flow.

Signals derive from `Condition`, and can be catched with handler bound
to this type.

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

Warnings are not thrown as errors when no handler is bound but the
message is printed to standard error out when warning condition is not
handled.  Similarly to `signal', the control is temporarily
transferred to a handler, but code evaluation continues if handler did
not transferred control flow.

Warnings derive from both `Warning` and `Condition`, and can be
catched by binding handler to any of these types.

# Examples

Warning is ignored if not handled:

```
(warn :warn-condition)
```

See `error' for examples how to handle conditions."
  (raise :warning condition-object))

(fn make-condition [condition-object arg1 ...]
  "Creates an instance of `condition-object'.  Accepts any amount of
additional arguments that will be passed as arguments to a handler
when handling this condition instance.

Condition created with `define-condition` and derived condition are
different objects, but the condition system sees those as the same
type.  Comparison semantics are such that derived condition is equal
to its base condition object.

# Examples

Defining a condition, making instance of this condition with two
arguments, and registering the handler for the original condition
object:

``` fennel
(define-condition some-condition)

(handler-case
    (error (make-condition some-condition {:foo \"bar\"} 42))
  (some-condition [c foo-bar forty-two]
    (assert-is (= some-condition c)) ; condition instance is equal to its base type
    (assert-eq {:foo \"bar\"} foo-bar)
    (assert-eq 42 forty-two)))
```"
  (assert (and (= :table (type condition-object))
               (= :condition condition-object.type)
               (not= nil condition-object.id))
          "condition must derive from existing condition object")
  (setmetatable
   {:data (if arg1 (pack arg1 ...))
    :id condition-object.id
    :parent condition-object.parent
    :type :condition}
   (getmetatable condition-object)))

(fn invoke-restart* [restart-name ...]
  "Invoke restart `restart-name' to handle a condition.

Additional arguments are passed to restart function as arguments.

Must be used only within the dynamic scope of `restart-case'.
Transfers control flow to restart function when executed.

# Examples

Handle the `error' with the `:use-value` restart:

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
  "Invoke the `continue' restart, which is automatically bound by `cerror' macro.

Must be used only within the dynamic scope of `restart-case'.
Transfers control flow to handler function when executed."
  (invoke-restart :continue))

(fn find-restart* [restart-name]
  "Searches `restart-name' in the dynamic scope, and if found, returns
its name."
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
