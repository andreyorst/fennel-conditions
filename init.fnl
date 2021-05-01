(local {: raise : pack : invoke-restart}                               ; Constructing relative path
  (require (if (and ... (not= ... :init))
               (.. ... :.impl.condition-system)
               :impl.condition-system)))

(fn error* [condition-object]
  "Raise `condition-object' as an error.

This macro is meant to replace inbuilt `error' function.  It has a bit
different interface than conventional Lua `error' function, as it
accepts condition as it's first argument and arguments of that
condition.  Similarly to `signal' and Lua's `error', this macro will
interrupt function execution where it was called, and no code after
`error' will be executed.  If no handler bound for raised condition,
`error' is promoted to Lua error with detailed message about
condition.

```
>> (error :condition-object 42)
runtime error: condition \"condition-object\" was thrown with the following arguments: 42
stack traceback...
```

# Examples
Error is thrown a Lua if not handled, thus can be caught with
`pcall` (note that `error' is wrapped into anonymous function, because
it is a macro):

``` fennel
(assert-not (pcall error :error-condition))
```

Errors can be handled with `handler-case':

``` fennel
(define-condition error-condition)
(fn handle-error []
  (handler-case (error (make-condition error-condition 42))
    (error-condition [_ x] x)))

(assert-eq 42 (handle-error))
```

Errors, signal, and warnings can be recovered with `handler-bind' and
`restart-case' by using `invoke-restart':

``` fennel
(define-condition error-condition)
(assert-eq 42 (handler-bind [error-condition
                             (fn [_ x]
                               (invoke-restart :use-value (+ x 10)))]
                (restart-case (error (make-condition error-condition 32))
                  (:use-value [x] x))))
```"
  (raise :error condition-object))

(fn signal [condition-object]
  "Raise `condition-object' as a signal.

Raises given condition as a signal.  Signals can be handled
with `handler-case' or `handler-bind', and don't promote to errors if
no handler found.  This macro will interrupt function execution at the
point where it was called, and no code after `signal' will be
executed.


# Examples
Signal is ignored if not handled:

``` fennel
(assert-eq nil (signal :signal-condition 10))
```

Signals can be handled like any other conditions.
See `error' for examples of how to handle signals."
  (raise :signal condition-object))

(fn warn [condition-object]
  "Raise `condition-object' as a warning.
Warnings are not thrown as errors when no handler is bound but their
message is printed to stderr.

# Examples
Warning is ignored if not handled:

``` fennel
(assert-eq nil (warn :warn-condition))
```

Warnings can be handled like any other conditions.
See `error' for examples of how to handle warnings."
  (raise :warn condition-object))

(fn make-condition [condition-object arg1 ...]
  "Derives condition from base `condition-object'.  Accepts any amount
of additional arguments that are passed as arguments to handlers."
  (let []
    (assert (and (= :table (type condition-object))
                 (= :condition condition-object.type)
                 (not= nil condition-object.id))
            "condition must derive from existing condition object")
    {:data (if arg1 (pack arg1 ...) condition-object.data)
     :id condition-object.id
     :parent condition-object.parent
     :type :condition}))

(fn invoke-restart* [restart-name ...]
  "Invoke restart `restart-name' to handle a condition.
Must be used only in handler functions defined with `handler-bind'.
Transfers control flow to handler function when executed.

# Examples
Handle the `error' with `:use-value' restart:

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
```"
  (invoke-restart restart-name ...))


(fn continue []
  "Invoke the `continue' restart bound by `cerror' macro."
  (invoke-restart :continue))

{:error error*
 : signal
 : warn
 : make-condition
 :invoke-restart invoke-restart*
 : continue}
