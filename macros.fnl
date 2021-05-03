(local condition-system ; Constructing relative path for runtime require
  (.. (: (or ... "") :gsub "macros$" "")
      :impl.condition-system))

(fn function-form? [form]
;;; Checks if list evaluates to function
  (when (list? form)
    (let [[f name? arglist] form]
      (if (or (= 'fn f) (= 'lambda f) (= 'λ f))
          (if (sym? name?)
              (sequence? arglist)
              (sequence? name?))
          (= 'hashfn f)))))

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
(assert-not
 (handler-bind [:signal-condition (fn [] (print \"caught signal condition\") 10)
                :error-condition (fn [] (print \"caught error condition\") 20)]
   (signal :error-condition)))
```

To provide a return value use either `handler-case' or `restart-case'
and `invoke-restart'."
  (assert-compile (= (% (length binding-vec) 2) 0)
                  "expected even number of signal/handler bindings"
                  binding-vec)
  ;; check each handler to be a symbol or a function definition
  (for [i 2 (length binding-vec) 2]
    (let [handler (. binding-vec i)]
      (assert-compile (or (sym? handler) (function-form? handler))
                      "handler must be a function"
                      handler)))
  `(let [target# {}
         cs# (require ,condition-system)
         scope# {:handlers ,(seq-to-table binding-vec)}]
     (tset scope# :parent cs#.handlers)
     (tset scope# :target target#)
     (tset cs# :handlers scope#)
     (let [(ok# res#) (pcall #[(do ,...)])]
       (tset cs# :handlers scope#.parent)
       (if ok# ((or table.unpack _G.unpack) res#)
           (match res#
             {:state :restarted :target target#} ((or table.unpack _G.unpack) (res#.restart))
             {:state :handled} (cs#.raise res#.type (cs#.compose-error-message res#.condition-object))
             {:state :error :message msg#} (_G.error msg#)
             _# (_G.error res#))))))

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
    (assert-compile (or (sequence? (. restart 2))) "expected parameter table" restart)
    (assert-compile (or (= :string (type (. restart 1)))) "restart name must be a string" restart))
  (let [restarts {:restarts (collect [n [restart & fn-tail] (ipairs [...])]
                              (let [[args descr body] fn-tail]
                                (values restart {:restart (list 'fn (unpack fn-tail))
                                                 :interactive? (not= nil (next args))
                                                 :name restart
                                                 : n
                                                 :description (when (and (= :string (type descr))
                                                                         (not= nil body))
                                                                descr)
                                                 :args (when (not= nil (next args))
                                                         (icollect [_ v (ipairs args)]
                                                           (view v {:one-line? true})))})))}]
    `(let [target# {}
           cs# (require ,condition-system)
           scope# ,restarts]
       (tset scope# :parent cs#.restarts)
       (tset scope# :target target#)
       (tset cs# :restarts scope#)
       (let [(ok# res#) (pcall (fn [] [(do ,expr)]))]
         (tset cs# :restarts scope#.parent)
         (if ok# ((or table.unpack _G.unpack) res#)
             (match res#
               {:state :restarted :target target#} ((or table.unpack _G.unpack) (res#.restart))
               {:state :error :message msg#} (_G.error msg#)
               _# (_G.error res#)))))))

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
    (assert-compile (sequence? (. handler 2)) "expected parameter table" handler))
  (let [handlers {:handlers (collect [_ [handler & fn-tail] (ipairs [...])]
                              (values handler (list 'fn (unpack fn-tail))))}]
    `(let [target# {}
           cs# (require ,condition-system)
           scope# ,handlers]
       (tset scope# :parent cs#.handlers)
       (tset scope# :target target#)
       (tset cs# :handlers scope#)
       (let [(ok# res#) (pcall #[(do ,expr)])]
         (tset cs# :handlers scope#.parent)
         (if ok# ((or table.unpack _G.unpack) res#)
             (match res#
               {:state :handled :target target#} ((or table.unpack _G.unpack) res#.data)
               {:state :handled} (_G.error res#)
               {:state :error :message msg#} (_G.error msg#)
               _# (_G.error res#)))))))

(fn define-condition [condition-symbol ...]
  "Create base condition object with `condition-symbol' from which
conditions will be derived with `make-condition'.  Accepts additional
`:parent` and `:name` key value pairs.

# Examples
Creating `error` condition:

``` fennel
(define-condition error)
```

Creating `simple-error` condition with parent set to `error` condition:

``` fennel
(define-condition error)
(define-condition simple-error :parent error)
```

Altering condition's printable name:

``` fennel
(define-condition dbze :name \"divide by zero error\")
```"
  (assert-compile (sym? condition-symbol) "condition-object must be a symbol" condition-object)
  (let [allowed-options {:parent true :name true}
        options (seq-to-table [...])
        condition-object {:name (tostring condition-symbol) :type :condition}]
    (each [k (pairs options)]
      (assert-compile (. allowed-options k) (.. "invalid key: " (tostring k)) k))
    (each [k (pairs allowed-options)]
      (match (. options k)
        v (tset condition-object k v)))
    `(local ,condition-symbol (let [condition-object# ,condition-object]
                                (doto condition-object#
                                  (tset :id condition-object#))))))

(fn cerror [continue-description condition-object ...]
  "Raise `condition-object' as an error with continue restart described by `continue-description'.

Similarly to `error', `cerror' raises condition as an error, but
automatically binds the `continue' restart, which can be used either
with the `continue' function in the handler, or in the interactive
debugger.  The `continue-description' is a string, describing what
will happen if `continue' restart is invoked.

# Examples
Convert `x` to positive value if it is negative:

``` fennel
(fn sqrt [x]
  (var x x)
  (when (< x 0)
    (cerror \"convert x to positive value\" :neg-sqrt x)
    (set x (- x)))
  (math.sqrt x))

(handler-bind [:neg-sqrt (fn [] (continue))]
  (sqrt -4))
```"
  (assert-compile (= :string (type continue-description))
                  "continue-description must be a string"
                  continue-description)
  (assert-compile (not= 'nil condition-object)
                  "condition-object must not be nil"
                  condition-object)
  `(restart-case (let [cs# (require ,condition-system)]
                   (cs#.raise :error ,condition-object))
     (:continue [] ,continue-description nil)))

(fn ignore-errors [...]
  `(let [(ok# res#) (pcall #[(do ,...)])]
     (if ok# ((or table.unpack _G.unpack) res#)
         (match res#
           {:state :handled :target target#} ((or table.unpack _G.unpack) res#.data)
           {:state :handled} (_G.error res#)
           _# nil))))

(setmetatable
 {: restart-case
  : handler-bind
  : handler-case
  : cerror
  : define-condition
  : ignore-errors}
 {:__index
  {:_DESCRIPTION "Condition system for Fennel language.

This module provides a set of macros, that implement Common
Lisp-inspired condition system for Fennel language.

Because public API is macros only, it is not possible to use this
library from pure Lua. This library depends on Fennel compiler, but it
is embedded in the internal API, and can be packaged together with the
library."}})
