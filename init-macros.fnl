(local condition-system
  ;; Constructing relative paths for comp-time require call splicing
  (if (and ... (not= ... :init-macros))
      (.. ... :.impl.condition-system)
      :impl.condition-system))

(local utils
  (if (and ... (not= ... :init-macros))
      (.. ... :.impl.utils)
      :impl.utils))

;;; Utils

(fn current-scope []
  ;; A getter for current dynamic scope
  (let [_ (sym :_)
        scope (gensym :scope)]
    `(let [utils# (require ,utils)
           thread# (or (and coroutine
                            coroutine.running
                            (tostring (coroutine.running)))
                       :main)]
       (match (. utils# :dynamic-scope thread#)
         ,scope ,scope
         ,_ (let [,scope {:handlers {}
                          :restarts {}}]
              (tset utils# :dynamic-scope thread# ,scope)
              ,scope)))))

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
  ;; Transform binding sequence into associative table.
  ;; [:a 1 :b 2] => {:a 1 :b 2}
  (let [tbl {}]
    (for [i 1 seq.n 2]
      (tset tbl (. seq i) (. seq (+ i 1))))
    tbl))

;;; Condition library macros

(fn handler-bind [binding-vec ...]
  "Bind handlers to conditions.

`binding-vec' is a sequential table of conditions and their respecting
handlers, followed by the body expression.  Each handler should be a
function of at least one argument - the condition being handled.
Other arguments are optional, and can be used inside the handler.

If the body expression or any of its subsequent expressions raises a
condition, and a handler is bound for this condition type, the handler
function is invoked.  If no handler were bound, handlers are searched
up the dynamic scope. If no handler found, condition is thrown as a
Lua error.

If invoked handler exits normally, a condition is re-raised.  To
prevent re-raising, use `invoke-restart' function.

# Examples

Handlers executed but their return values are not used:

``` fennel
(var res nil)

(assert-not
 (handler-bind [:signal-condition (fn [] (set res \"signal\") 10)
                :error-condition (fn [] (set res \"error\") 20)]
   (signal :error-condition)))

(assert-eq res :error)
```

To provide a return value use either `handler-case' or a combination
of `restart-case' and `invoke-restart'."
  ;; This will be a common pattern for `handler-bind`, `restart-case`,
  ;; and `handler-case`.  A lot of stuff happens at compile time, and
  ;; dynamic scope is constructed at compile time to avoid explicit
  ;; iteration.  Because of it most variables are defined using
  ;; `gensym` to be accessible at compile time to build code, and at
  ;; runtime to access that code.
  (let [target (gensym :target)
        scope (gensym :scope)
        binding-len (length binding-vec)
        setup '(do)]
    (assert-compile (= (% binding-len 2) 0)
                    "expected even number of signal/handler bindings"
                    binding-vec)
    ;; check each handler to be a symbol or a function definition, and
    ;; put a handler into dynamic scope constructor stored in `setup`
    (for [i binding-len 1 -2]
      (let [condition-object (. binding-vec (- i 1))
            handler (. binding-vec i)]
        (assert-compile (or (sym? handler) (function-form? handler))
                        "handler must be a function"
                        handler)
        (table.insert setup `(assert (not= nil ,condition-object)
                                     "condition object must not be nil"))
        (table.insert
         setup
         `(tset ,scope :handlers {:parent (. ,scope :handlers)
                                  :target ,target
                                  :handler-type :handler-bind
                                  :handler {,condition-object
                                            ,handler}}))))
    `(let [,target {}
           {:pack pack#} (require ,utils)
           {:pcall-handler-bind pcall-handler-bind#} (require ,condition-system)
           ,scope ,(current-scope)
           orig-handlers# (. ,scope :handlers)]
       ,setup
       ,(if (. (get-scope) :vararg)
            `(pcall-handler-bind#
              (fn [...] (pack# (do ,...))) ,scope ,target orig-handlers# ...)
            `(pcall-handler-bind#
              (fn [] (pack# (do ,...))) ,scope ,target orig-handlers#)))))

(fn restart-case [expr ...]
  "Condition restart point.

Accepts expression `expr' and restarts that can be used when handling
conditions thrown from within the expression.  Similarly to
`handler-case' restarts are lists with first element being a restart
name, and an fn-tail.

If expression or any of it's subsequent expressions raises a
condition, it will be possible to return into the `restart-case', and
execute one of the provided restarts.  Restarts can be executed with
the `invoke-restart' function from handlers bound with `handler-bind'.
Restart names are always strings.

# Examples

Specifying two restarts for `:signal-condition`:

``` fennel
(restart-case (signal :signal-condition)
  (:some-restart [] :body)
  (:some-other-restart [] :other-body))
```"
  (let [target (gensym :target)
        scope (gensym :scope)
        restarts (table.pack ...)
        setup '(do)]
    (for [i restarts.n 1 -1]
      (let [[restart & [arglist &as fn-tail]] (. restarts i)]
        (assert-compile (list? (. restarts i)) "restarts must be defined as lists" restart)
        (assert-compile (or (sequence? arglist)) "expected parameter table" restart)
        (assert-compile (or (= :string (type restart))) "restart name must be a string literal" restart)
        (let [[args descr body] fn-tail
              restart {:restart (list 'fn (unpack fn-tail))
                       :name restart
                       :description (when (and (= :string (type descr))
                                               (not= nil body))
                                      descr)
                       :args (when (not= nil (next args))
                               (icollect [_ v (ipairs args)]
                                 (view v {:one-line? true})))}]
          (table.insert setup `(tset ,scope :restarts {:parent (. ,scope :restarts)
                                                       :target ,target
                                                       :restart ,restart})))))
    `(let [,target {}
           {:pack pack#} (require ,utils)
           {:pcall-restart-case pcall-restart-case#} (require ,condition-system)
           ,scope ,(current-scope)
           orig-restarts# (. ,scope :restarts)]
       ,setup
       ,(if (. (get-scope) :vararg)
            `(pcall-restart-case#
              (fn [...] (pack# (do ,expr))) ,scope ,target orig-restarts# ...)
            `(pcall-restart-case#
              (fn [] (pack# (do ,expr))) ,scope ,target orig-restarts#)))))

(fn handler-case [expr ...]
  "Condition handling point, similar to try/catch.

Accepts expression `expr' and handlers that can be used to handle
conditions, raised from within the expression or any subsequent
expressions.  Provides the facility to catch named conditions raised
with `signal', `warn` and `error' functions.  If any condition is
raised, before propagating condition to error, a handler is searched.
If handler is bound for this condition, it is executed, and the result
of `handler-case' expression will be the result of the handler.

Handlers are defined as lists, where the first object represents the
condition to handle, and the rest is fn-tail - a sequential table of
function arguments, followed by the function body.

# Examples

Handling `error' condition:

``` fennel
(assert-eq 42 (handler-case (error :error-condition)
                (:error-condition [] 42)))
```"
  (let [target (gensym :target)
        scope (gensym :scope)
        handlers (table.pack ...)
        setup `(do)]
    (for [i handlers.n 1 -1]
      (let [[condition-object arglist &as handler] (. handlers i)]
        (assert-compile (list? handler) "handlers must be defined as lists" handler)
        (assert-compile (sequence? arglist) "expected parameter table" handler)
        (table.insert setup `(assert (not= nil ,condition-object)
                                     "condition object must not be nil"))
        (table.insert setup `(tset ,scope :handlers {:parent (. ,scope :handlers)
                                                     :target ,target
                                                     :handler-type :handler-case
                                                     :handler {,condition-object ,(list 'fn (unpack handler 2))}}))))
    `(let [,target {}
           {:pack pack#} (require ,utils)
           {:pcall-handler-case pcall-handler-case#} (require ,condition-system)
           ,scope ,(current-scope)
           orig-handlers# (. ,scope :handlers)]
       ,setup
       ,(if (. (get-scope) :vararg)
            `(pcall-handler-case#
              (fn [...] (pack# (do ,expr))) ,scope ,target orig-handlers# ...)
            `(pcall-handler-case#
              (fn [] (pack# (do ,expr))) ,scope ,target orig-handlers#)))))

(fn define-condition [condition-symbol ...]
  "Create base condition object with `condition-symbol' from which
conditions can later be derived with `make-condition'.  Accepts
additional `:parent` and `:name` key value pairs.  If no `:name`
specified, uses `condition-symbol`'s `tostring` representation.  If no
`:parent` given uses `Condition' object as a parent.

# Examples

Altering condition's printable name:

``` fennel
(define-condition dbz :name :divide-by-zero)
```

Creating `math-error` condition with parent set to `Error` condition,
and `divide-by-zero` condition with parent set to `math-error`, and handling it:

``` fennel
(define-condition math-error :parent Error)
(define-condition divide-by-zero :parent math-error)

(assert-eq :ok (handler-case (error divide-by-zero)
                 (math-error [] :ok)))
```"
  (assert-compile (sym? condition-symbol) "condition-object must be a symbol" condition-symbol)
  (let [allowed-options {:parent true :name true}
        options (seq-to-table (table.pack ...))
        condition-object {:name (tostring condition-symbol) :type :condition}]
    (each [k (pairs options)]
      (assert-compile (. allowed-options k) (.. "invalid key: " (tostring k)) k))
    (each [k (pairs allowed-options)]
      (match (. options k)
        v (tset condition-object k v)))
    (when (= nil condition-object.parent)
      (tset condition-object :parent `(. (require ,condition-system) :Condition)))
    `(local ,condition-symbol (let [{:condition= eq#} (require ,condition-system)
                                    condition-object# ,condition-object
                                    name# condition-object#.name]
                                (doto condition-object#
                                  (setmetatable {:__eq eq#
                                                 :__name (.. "condition " name#)
                                                 :__fennelview #(.. "#<" (tostring condition-object#) ">")})
                                  (tset :id condition-object#))))))

(fn cerror [continue-description condition-object ...]
  "Raise `condition-object' as an error with auto-bound `:continue` restart, described by `continue-description'.

Similarly to `error', `cerror' raises condition as an error, but
automatically binds the `continue' restart, which can be used either
with the `continue' function in the handler, or in the interactive
debugger.  The `continue-description' must be a string, describing
what will happen if `continue' restart is invoked.

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
  (assert-eq 2 (sqrt -4)))
```"
  (assert-compile (= :string (type continue-description))
                  "continue-description must be a string"
                  continue-description)
  (assert-compile (not= 'nil condition-object)
                  "condition-object must not be nil"
                  condition-object)
  `(restart-case (let [{:raise raise#} (require ,condition-system)]
                   (raise# :error ,condition-object))
     (:continue [] ,continue-description nil)))

(fn ignore-errors [...]
  "Ignore all conditions of type error.  If error condition was raised,
returns nil and the condition as multiple values.  If no error
conditions were raised, returns the resulting values normally.  Lua
errors can be handled with this macro.

# Examples

Condition of type error is ignored:

``` fennel
(local result [])

(local (res condition)
  (ignore-errors
    (table.insert result 1)
    (table.insert result 2)
    (error :some-error)
    (table.insert result 3)))

(assert-not res)
(assert-eq :some-error condition)
(assert-eq [1 2] result)
```"
  `(let [cs# (require ,condition-system)]
     (handler-case (do ,...)
       (cs#.Error [c#] (values nil c#)))))

(fn unwind-protect [expr ...]
  "Runs `expr` in protected call, and runs all other forms as cleanup
forms before returning the value, whether `expr` returned normally or
an error occurred.  Similar to try/finally without a catch.

# Examples

``` fennel
(local result [])

(ignore-errors
  (unwind-protect
      (/ 1 nil)
    (table.insert result 1)
    (table.insert result 2)))

(assert-eq [1 2] result)
```"
  `(let [{:pack pack# :unpack unpack#} (require ,utils)]
     (let [(ok# res#) ,(if (. (get-scope) :vararg)
                           `(pcall (fn [...] (pack# (do ,expr))) ...)
                           `(pcall (fn [] (pack# (do ,expr)))))]
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
  {:_MODULE_NAME "macros"
   :_DESCRIPTION "Condition system for Fennel language.

This module provides a set of macros, that implement Common
Lisp-inspired condition system for Fennel language."}})
