(local condition-system ; Constructing relative path for runtime require
  (.. (: (or ... "") :gsub "macros$" "")
      :impl.condition-system))

(fn current-thread []
  ;; Returns name of the current thread if possible, otherwise returns
  ;; `:main`
  `(or (and coroutine
            coroutine.running
            (tostring (coroutine.running)))
       :main))

(fn function-form? [form]
  ;; Check if `form` evaluates to a function
  (when (list? form)
    (let [[f name? arglist] form]
      (if (or (= 'fn f) (= 'lambda f) (= 'λ f))
          (if (sym? name?)
              (and (sequence? arglist) true)
              (and (sequence? name?) true))
          (= 'hashfn f)))))

(fn seq-to-table [seq]
  ;; Transform sequential bindings into associative table.
  ;; [:a 1 :b 2] => {:a 1 :b 2}
  (let [tbl {}]
    (for [i 1 seq.n 2]
      (tset tbl (. seq i) (. seq (+ i 1))))
    tbl))

;;; Condition library macros

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
  ;; This will be a common pattern for `handler-bind`, `restart-case`,
  ;; and `handler-case`.  A lot of stuff happens at compile time, and
  ;; dynamic scope is constructed at compile time to avoid explicit
  ;; iteration.  Because of it most variables are defined using
  ;; `gensym` to be accessible at compile time to build code, and at
  ;; runtime to access that code.
  (let [_ (sym :_)
        cs (gensym :condition-system)
        thread (gensym :thread)
        target (gensym :target)
        scope (gensym :scope)
        binding-len (length binding-vec)
        setup `(doto ,scope)]
    (assert-compile (= (% binding-len 2) 0)
                    "expected even number of signal/handler bindings"
                    binding-vec)
    ;; check each handler to be a symbol or a function definition, and
    ;; put a handler into dynamic scope constructor stored in `setup`
    (for [i binding-len 1 -2]
      (let [handler (. binding-vec i)]
        (assert-compile (or (sym? handler) (function-form? handler))
                        "handler must be a function"
                        handler)
        (table.insert
         setup
         `(tset :handlers {:parent (. ,scope :handlers)
                           :target ,target
                           :handler-type :handler-bind
                           :handler {,(. binding-vec (- i 1))
                                     ,(. binding-vec i)}}))))
    `(let [,target {}
           ,thread ,(current-thread)
           ,cs (require ,condition-system)
           {:pack pack# :unpack unpack# :raise raise#} ,cs
           ,scope (do (when (not (. ,cs :dynamic-scope ,thread))
                        (tset ,cs :dynamic-scope ,thread {:handlers {}
                                                          :restarts {}}))
                      (. ,cs :dynamic-scope ,thread))
           orig-handlers# (. ,scope :handlers)
           ,_ ,setup
           (ok# res#) (pcall #(pack# (do ,...)))]
       ;; Reset current scope context after exiting user code, but
       ;; before actually handling the condition in order to avoid
       ;; infinity loops.
       (doto ,scope
         (tset :current-context nil)
         (tset :handlers orig-handlers#))
       (if ok# (unpack# res#)
           (match res#
             ;; Handled conditions are re-raised because
             ;; `handler-bind` requires exiting handler with
             ;; `restart-case` or another error.
             {:state :handled :target ,target}
             (raise# res#.type res#.condition-object)
             ;; Internal errors bubble up the dynamic scope until no
             ;; handlers and restarts left, to make sure we can't
             ;; catch those accidentally.
             {:state :error :message msg#}
             (if (or (. ,scope :handlers :parent)
                     (. ,scope :restarts :parent))
                 (_G.error res#)
                 (_G.error msg# 2))
             ,_ (_G.error res#))))))

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
  (let [_ (sym :_)
        cs (gensym :condition-system)
        thread (gensym :thread)
        target (gensym :target)
        scope (gensym :scope)
        restarts (table.pack ...)
        setup `(doto ,scope)]
    (for [i restarts.n 1 -1]
      (let [[restart & fn-tail] (. restarts i)]
        (assert-compile (list? (. restarts i)) "restarts must be defined as lists" restart)
        (assert-compile (or (sequence? (. fn-tail 1))) "expected parameter table" restart)
        (assert-compile (or (= :string (type restart))) "restart name must be a string" restart)
        (let [[args descr body] fn-tail
              restart {:restart (list 'fn (unpack fn-tail))
                       :name restart
                       :description (when (and (= :string (type descr))
                                               (not= nil body))
                                      descr)
                       :args (when (not= nil (next args))
                               (icollect [_ v (ipairs args)]
                                 (view v {:one-line? true})))}]
          (table.insert setup `(tset :restarts {:parent (. ,scope :restarts)
                                                :target ,target
                                                :restart ,restart})))))
    `(let [,target {}
           ,thread ,(current-thread)
           ,cs (require ,condition-system)
           {:pack pack# :unpack unpack# :raise raise#} ,cs
           ,scope (do (when (not (. ,cs :dynamic-scope ,thread))
                        (tset ,cs :dynamic-scope ,thread {:handlers {}
                                                          :restarts {}}))
                      (. ,cs :dynamic-scope ,thread))
           orig-restarts# (. ,scope :restarts)
           ,_ ,setup
           restarts# (. ,scope :restarts)
           (ok# res#) (pcall #(pack# (do ,expr)))]
       (doto ,scope
         (tset :restarts orig-restarts#)
         (tset :current-context nil))
       (if ok# (unpack# res#)
           (match res#
             {:state :restarted :target ,target} (res#.restart)
             {:state :restarted} (_G.error res#)
             {:state :error :message msg#}
             (if (or (. ,scope :handlers :parent)
                     (. ,scope :restarts :parent))
                 (_G.error res#)
                 (_G.error msg# 2))
             ;; Encountered Lua error, we restore bound restarts, and
             ;; try to handle it as a condition.  Mostly repeats what
             ;; was done for a condition
             ,_ (let [(,_ res2#) (do (tset ,scope :restarts restarts#)
                                     (pcall raise# :error res#))]
                  (doto ,scope
                    (tset :restarts orig-restarts#)
                    (tset :current-context nil))
                  (match res2#
                    {:state :restarted :target ,target} (res2#.restart)
                    {:state :restarted} (_G.error res2#)
                    {:state :error :message msg2#}
                    (if (or (. ,scope :handlers :parent)
                            (. ,scope :restarts :parent))
                        (_G.error res2#)
                        (_G.error msg2# 2))
                    ;; If Lua error was not handled, we throw it
                    ;; instead of fail result from the handler
                    ,_ (_G.error res#))))))))

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
  (let [_ (sym :_)
        target (gensym :target)
        thread (gensym :thread)
        scope (gensym :scope)
        cs (gensym :condition-system)
        handlers (table.pack ...)
        setup `(doto ,scope)]
    (for [i handlers.n 1 -1]
      (let [handler (. handlers i)]
        (assert-compile (list? handler) "handlers must be defined as lists" handler)
        (assert-compile (sequence? (. handler 2)) "expected parameter table" handler)
        (table.insert setup `(tset :handlers {:parent (. ,scope :handlers)
                                              :target ,target
                                              :handler-type :handler-case
                                              :handler {,(. handler 1) ,(list 'fn (unpack handler 2))}}))))
    `(let [,target {}
           ,thread ,(current-thread)
           ,cs (require ,condition-system)
           {:pack pack#
            :unpack unpack#
            :raise raise#
            :find-handler find-handler#} ,cs
           ,scope (do (when (not (. ,cs :dynamic-scope ,thread))
                        (tset ,cs :dynamic-scope ,thread {:handlers {}
                                                          :restarts {}}))
                      (. ,cs :dynamic-scope ,thread))
           orig-handlers# (. ,scope :handlers)
           ,_ ,setup
           handlers# (. ,scope :handlers)
           (ok# res#) (pcall #(pack# (do ,expr)))]
       (doto ,scope
         (tset :handlers orig-handlers#)
         (tset :current-context nil))
       (if ok# (unpack# res#)
           (match res#
             {:state :handled :target ,target :handler handler#} (handler#)
             {:state :error :message msg#}
             (if (or (. ,scope :handlers :parent)
                     (. ,scope :restarts :parent))
                 (_G.error res#)
                 (_G.error msg# 2))
             {:state :handled} (_G.error res#)
             ;; In case Lua error was raised we reuse handlers defined
             ;; within this `handler-case` and up the dynamic scope.
             ,_ (match (find-handler# res# :error handlers#)
                  {:handler handler#} (handler# res#)
                  ,_ (_G.error res#)))))))

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
        options (seq-to-table (table.pack ...))
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
  `(restart-case (let [{:raise raise#} (require ,condition-system)]
                   (raise# :error ,condition-object))
     (:fennel-conditions/continue [] ,continue-description nil)))

(fn ignore-errors [...]
  "Ignore all conditions of type error.  If error condition was raised,
returns nil and condition as values.  If no error conditions were
raised, returns the resulting values normally.  Lua errors can be
handled with this macro."
  `(handler-case (do ,...)
     (:fennel-conditions/error [c#] (values nil c#))))

(fn unwind-protect [expr ...]
  "Runs `expr` in protected call, and runs all other forms as cleanup
forms before returning value, whether `expr` returned normally or
error occurred."
  `(let [{:pack pack# :unpack unpack#} (require ,condition-system)]
     (let [(ok# res#) (pcall #(pack# (do ,expr)))]
       (if ok#
           (do (do ,...) (unpack# res#))
           (do (do ,...) (_G.error res#))))))

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
