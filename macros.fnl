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
of at least one argument - the condition being handled.  Other arguments
are optional, and can be used inside the handler.

If body expression raises a condition, a bound handler is invoked.
If no handler were bound for condition, condition is thrown as a
Lua error.

If handler exits normally a condition

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
  (let [binding-len (length binding-vec)]
    (assert-compile (= (% binding-len 2) 0)
                    "expected even number of signal/handler bindings"
                    binding-vec)
    ;; check each handler to be a symbol or a function definition
    (for [i 2 binding-len 2]
      (let [handler (. binding-vec i)]
        (assert-compile (or (sym? handler) (function-form? handler))
                        "handler must be a function"
                        handler)))
    `(let [target# {}
           cs# (require ,condition-system)
           binding-vec# ,binding-vec
           orig-scope# cs#.handlers]
       (for [i# ,binding-len 1 -2]
         (let [scope# {:parent cs#.handlers
                       :target target#
                       :handler-type :handler-bind
                       :handlers {(. binding-vec# (- i# 1)) (. binding-vec# i#)}}]
           (tset cs# :handlers scope#)))
       (let [(ok# res#) (pcall #(cs#.pack (do ,...)))]
         (doto cs#
           (tset :current-scope nil)
           (tset :handlers orig-scope#))
         (if ok# (cs#.unpack res#)
             (match res#
               {:state :handled :target target#} (cs#.raise res#.type res#.condition-object)
               {:state :error :message msg#} (_G.error msg#)
               _# (_G.error res#)))))))

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
  (let [restarts
        (icollect [_ [restart & fn-tail] (ipairs [...])]
          (let [[args descr body] fn-tail]
            {:restart (list 'fn (unpack fn-tail))
             :name restart
             :description (when (and (= :string (type descr))
                                     (not= nil body))
                            descr)
             :args (when (not= nil (next args))
                     (icollect [_ v (ipairs args)]
                       (view v {:one-line? true})))}))
        restart-len (length restarts)]
    `(let [target# {}
           cs# (require ,condition-system)
           restarts# ,restarts
           orig-scope# cs#.restarts]
       (for [i# ,restart-len 1 -1]
         (let [scope# {:parent cs#.restarts
                       :target target#
                       :restart (. restarts# i#)}]
           (tset cs# :restarts scope#)))
       (let [scope# cs#.restarts
             (ok# res#) (pcall #(cs#.pack (do ,expr)))]
         (doto cs#
           (tset :restarts orig-scope#)
           (tset :current-scope nil))
         (if ok# (cs#.unpack res#)
             (match res#
               {:state :restarted :target target#} (res#.restart)
               {:state :restarted} (_G.error res#)
               {:state :error :message msg#} (_G.error msg#)
               ,(sym :_)
               (do (tset cs# :restarts scope#) ;; restoring restart context
                   (let [res2# (select 2 (pcall cs#.raise :error res#))]
                     (doto cs#
                       (tset :restarts orig-scope#)
                       (tset :current-scope nil))
                     (match res2#
                       {:state :restarted :target target#} (res#.restart)
                       {:state :restarted} (_G.error res2#)
                       {:state :error :message msg2#} (_G.error msg2#)
                       ,(sym :_) (_G.error res#))))))))))

(fn handler-case [expr ...]
  "Condition handling similar to try/catch.

Accepts expression `expr' and handlers that can be used when handling
conditions, raised from within the expression.  Provides the facility
to catch named conditions raised with `signal', `warn` and `error'
functions.  If any condition is raised, before propagating condition
to error, a handler is searched.  If handler is bound for this
condition, it is executed, and the result of `handler-case' expression
will be result of the handler.

Handlers are defined as lists, where the first object represents the
condition to handle, and the rest is fn-tail - a sequential table of
function arguments, followed by the function body.

# Examples

Handling `error' condition:

``` fennel
(assert-eq 42 (handler-case (error :error-condition)
                (:error-condition [] 42)))
```"

  (let [handlers []]
    (each [_ handler (ipairs [...])]
      (assert-compile (list? handler) "handlers must be defined as lists" handler)
      (assert-compile (sequence? (. handler 2)) "expected parameter table" handler)
      (doto handlers
        (table.insert (. handler 1))
        (table.insert (list 'fn (unpack handler 2)))))
    `(let [target# {}
           cs# (require ,condition-system)
           handlers# ,handlers
           orig-scope# cs#.handlers]
       (for [i# (length handlers#) 1 -2]
         (let [scope# {:parent cs#.handlers
                       :target target#
                       :handler-type :handler-case
                       :handlers {(. handlers# (- i# 1)) (. handlers# i#)}}]
           (tset cs# :handlers scope#)))
       (let [scope# cs#.handlers
             (ok# res#) (pcall #(cs#.pack (do ,expr)))]
         (doto cs#
           (tset :handlers orig-scope#)
           (tset :current-scope nil))
         (if ok# (cs#.unpack res#)
             (match res#
               {:state :handled :target target#} (res#.data)
               {:state :error :message msg#} (_G.error msg#)
               {:state :handled} (_G.error res#)
               _# (match ((fn find# [scope#]
                            (when scope#
                              (match (or (. scope#.handlers :fennel-conditions/error)
                                         (. scope#.handlers :fennel-conditions/condition))
                                handler# handler#
                                nil (find# scope#.parent)))) scope#)
                    handler# (handler# res#)
                    nil (_G.error res#))))))))

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
    (cerror \"convert x to positive value\" :neg-sqrt)
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
     (:fennel-conditions/continue [] ,continue-description nil)))

(fn ignore-errors [...]
  "Ignore all conditions of type error.  If error condition was raised,
returns nil and condition as values.  If no error conditions were
raised, returns the resulting values normally.  Lua errors can be
handled with this macro."
  `(let [cs# (require ,condition-system)]
     (handler-case (do ,...)
       (:fennel-conditions/error [c#] (values nil c#)))))

(fn unwind-protect [expr ...]
  "Runs `expr` in protected call, and runs all other forms as cleanup
forms before returning value, whether `expr` returned normally or
error occurred."
  `(let [cs# (require ,condition-system)]
     (let [(ok# res#) (pcall #(cs#.pack (do ,expr)))]
       (if ok#
           (do ,... (cs#.unpack res#))
           (do ,... (_G.error res#))))))

(setmetatable
 {: restart-case
  : handler-bind
  : handler-case
  : cerror
  : define-condition
  : ignore-errors
  : unwind-protect}
 {:__index
  {:_DESCRIPTION "Condition system for Fennel language.

This module provides a set of macros, that implement Common
Lisp-inspired condition system for Fennel language."}})
