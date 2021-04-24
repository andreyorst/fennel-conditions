(local condition-system ; Constructing relative path for runtime require
       (if (and ... (string.match ... "conditions$"))
           (.. (string.gsub ... "conditions$" "") :impl.condition-system)
           :impl.condition-system))

;; Helper functions

(fn first [tbl]
  (. tbl 1))

(fn second [tbl]
  (. tbl 2))

(fn rest [tbl]
  ((or table.unpack _G.unpack) tbl 2))

(fn seq-to-table [seq]
;;; Transform sequential bindings into associative table.
;;; [:a 1 :b 2] => {:a 1 :b 2}
  (let [tbl {}]
    (for [i 1 (length seq) 2]
      (tset tbl (. seq i) (. seq (+ i 1))))
    tbl))

;; Condition library functions

(fn handler-bind [binding-vec ...]
  "Bind handlers to conditions.

`binding-vec' is a sequential table of conditions and their respecting
handlers followed by the body expression.  Each handler is a function
of at least one argument - the signal being handled.  Other arguments
are optional, and can be used inside the handler.

If body expression signals a condition, a bound handler is invoked.
If no handler were bound for condition, condition is thrown as an
exception.

# Examples
Handlers executed but their return values are not used:

``` fennel
(handler-bind [:signal-condition (fn [] (print \"caught signal condition\") 10)
               :error-condition (fn [] (print \"caught error condition\") 20)]
  (error :error-condition))
;; caught signal condition
;; => nil
```

To provide a return value use either `handler-case' or `restart-case'
and `invoke-restart'."
  (assert-compile (= (% (length binding-vec) 2) 0)
                  "expected even number of signal/handler bindings"
                  binding-vec)
  ;; check each handler to be a symbol or a function definition
  (for [i 2 (length binding-vec) 2]
    (let [handler (. binding-vec i)]
      (assert-compile (or (sym? handler)
                          (and (list? handler)
                               (or (= 'fn (first handler))
                                   (= 'hashfn (first handler))
                                   (= 'lambda (first handler))
                                   (= 'λ (first handler)))))
                      "handler must be a function"
                      handler)))
  `(let [cs# (require ,condition-system)
         scope# {:conditions ,(seq-to-table binding-vec)}]
     (tset scope# :parent cs#.conditions.scope)
     (tset cs#.conditions :scope scope#)
     (let [(ok# res#) (pcall (fn [] ,...))]
       (tset cs#.conditions :scope scope#.parent)
       (if ok# res# (assert false res#)))))

(fn restart-case [expr ...]
  "Resumable condition restart point.
Accepts expression `expr' and restarts that can be used when handling
conditions thrown from within the expression.  Similarly to
`handler-case' restarts are lists with condition and fn-tail.

If expression or any of it's subsequent expressions raises a
condition, it will be possible to return into the `restart-case', and
execute one of the provided restarts.  Restarts can be executed with
the `invoke-restart' macro only from handlers bound with
`handler-bind'.  Restart names are always strings.

# Examples
Specifying two restarts for `:signal-condition`:

``` fennel
(restart-case (signal :signal-condition)
  (:some-restart [] :body)
  (:some-other-restart [] :body))
```"
  (each [_ restart (ipairs [...])] ; check if all restarts are lists that define functions
    (assert-compile (list? restart) "restarts must be defined as lists" restart)
    (assert-compile (or (sequence? (second restart))) "expected parameter table" restart)
    (assert-compile (= :string (type (first restart))) "restart name must be a string" restart))
  (let [restarts {:restarts (collect [_ [restart & fn-tail] (ipairs [...])]
                              (values restart (list 'fn (unpack fn-tail))))}]
    `(let [cs# (require ,condition-system)
           scope# ,restarts]
       (tset scope# :parent cs#.restarts.scope)
       (tset cs#.restarts :scope scope#)
       (let [(ok# res#) (pcall (fn [] ,expr))]
         (tset cs#.restarts :scope scope#.parent)
         (if ok# res# (assert false res#))))))

(fn construct-handler [fn-tail]
;;; Constructs handler function for `handler-case'
  `(fn [...]
     (assert false
             {:handled true
              :data [((fn ,(unpack fn-tail)) ...)]})))

(fn handler-case [expr ...]
  "Condition handling.
Accepts expression `expr' and handlers that can be used when handling
conditions raised from within the expression.  Provides the facility
to catch named conditions raised with `signal' or `error' macros.  If
any condition is raised, before propagating condition to error, a
handler is searched.  If handler is bound for this condition, it is
executed, and the result of `handler-case' expression will be result
of the handler.

Handlers are lists where first object represents condition, which can
be of any type, and the rest is fn-tail - sequential table of function
arguments, and function body.


# Examples
Handling `error' condition:

``` fennel
(assert-eq 42 (handler-case (error :error-condition)
                (:error-condition [] 42)))
```"
  (each [_ handler (ipairs [...])]
    (assert-compile (list? handler) "handlers must be defined as lists" handler)
    (assert-compile (sequence? (second handler)) "expected parameter table" handler))
  (let [handlers {:conditions (collect [_ [handler & fn-tail] (ipairs [...])]
                                (values handler (construct-handler fn-tail)))}]
    `(let [cs# (require ,condition-system)
           scope# ,handlers]
       (tset scope# :parent cs#.conditions.scope)
       (tset cs#.conditions :scope scope#)
       (let [(ok# res#) (pcall (fn [] ,expr))]
         (tset cs#.conditions :scope scope#.parent)
         (if ok# res# (assert false res#))))))

(fn invoke-restart [restart-name ...]
  "Invoke restart `restart-name' to handle condition.
Must be used only in handler functions defined with `handler-bind'.
Transfers control flow to handler function when executed.

# Examples
Handle the `error' with `:use-value' restart:

``` fennel
(assert-eq 42 (handler-bind [:error-condition
                             (fn [_c x]
                               (invoke-restart :use-value (+ x 10))
                               (print \"never prints\"))]
                (restart-case (do (error :error-condition 32)
                                  (print \"also never prints\"))
                  (:use-value [x] x))))
```"
  `(let [cs# (require ,condition-system)]
     (assert false {:handled true
                    :data [(cs#.invoke-restart ,restart-name ,...)]})))


;; TODO: use `gensym' to capture `raise-error' result and return it
;;       once https://todo.sr.ht/~technomancy/fennel/54 is fixed
(fn error* [condition-name ...]
  "Raise `condition-name' as an error.

This macro is meant to replace inbuilt `error' function.  It has a bit
different interface than conventional Lua `error' function, as it
accepts condition as it's first argument and arguments of that
condition.  Similarly to `signal' and Lua's `error', this macro will
interrupt function execution where it was called, and no code after
`error' will be executed.  If no handler bound for raised condition,
`error' is promoted to Lua error with detailed message about
condition.

```
>> (error :condition-name 42)
runtime error: condition \"condition-name\" was thrown with the following arguments: 42
stack traceback...
```

# Examples
Error is thrown if not handled:

``` fennel
(assert-not (pcall #(error :error-condition)))
```

Errors can be handled with `handler-case':

``` fennel
(assert-eq 42 (handler-case (warn :signal-condition 42)
                (:signal-condition [_ x] x)))
```

Errors, signal, and warnings can be recovered with `handler-bind' and
`restart-case' by using `invoke-restart':

``` fennel
(assert-eq 42 (handler-bind [:error-condition
                             (fn [_ x]
                               (invoke-restart :use-value (+ x 10)))]
                (restart-case (error :error-condition 32)
                  (:use-value [x] x))))
```"
  (assert-compile (not= 'nil condition-name)
                  "condition must not be nil"
                  condition-name)
  `(let [cs# (require ,condition-system)]
     (lua "do return")
     (cs#.raise :error ,condition-name ,...)
     (lua "end")))

(fn signal [condition-name ...]
  "Raise `condition-name' as a signal.

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
  (assert-compile (not= 'nil condition-name)
                  "condition must not be nil"
                  condition-name)
  `(let [cs# (require ,condition-system)]
     (lua "do return")
     (cs#.raise :signal ,condition-name ,...)
     (lua "end")))

(fn warn [condition-name ...]
  "Raise `condition-name' as a warning.
Warnings are not thrown as errors when no handler is bound but their
message is printed to stderr.

# Examples
Warning is ignored if not handled:

``` fennel
(assert-eq nil (warn :warn-condition))
```

Warnings can be handled like any other conditions.
See `error' for examples of how to handle warnings."
  `(let [cs# (require ,condition-system)]
     (lua "do return")
     (cs#.raise :warn ,condition-name ,...)
     (lua "end")))

(setmetatable
 {: restart-case
  : handler-bind
  : handler-case
  :error error*
  : signal
  : warn
  : invoke-restart}
 {:__index
  {:_DESCRIPTION "Condition system for Fennel language.

This module provides a set of macros, that implement Common
Lisp-inspired condition system for Fennel language.

Because public API is macros only, it is not possible to use this
library from pure Lua. This library depends on Fennel compiler, but it
is embedded in the internal API, and can be packaged together with the
library."}})
