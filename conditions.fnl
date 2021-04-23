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

Accepts a sequence of conditions and their respecting handlers followed
by the body expression.  Each handler is a function of at least one
argument - the signal being handled.  Other arguments are optional,
and can be used inside the handler.

If body expression signals a condition, a bound handler is invoked.
If no handler were bound for condition, condition is thrown as an
exception."
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
Accepts expression and restarts that can be used when handling
conditions thrown from within the expression."
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
Accepts expression and restarts that can be used when handling
conditions thrown from within the expression."
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
         (tset cs#.restarts :scope scope#.parent)
         (if ok# res# (assert false res#))))))

(fn invoke-restart [restart-name ...]
  "Invoke restart `restart-name' to handle condition.
Must be used only in handler functions defined with `handler-bind'."
  `(let [cs# (require ,condition-system)]
     (assert false {:handled true
                    :data [(cs#.invoke-restart ,restart-name ,...)]})))


;; TODO: use `gensym' to capture `raise-error' result and return it
;;       once https://todo.sr.ht/~technomancy/fennel/54 is fixed
(fn error* [condition-name ...]
  "Raise `condition-name' as an error."
  (assert-compile (not= 'nil condition-name)
                  "condition must not be nil"
                  condition-name)
  `(let [cs# (require ,condition-system)]
     (lua "do return")
     (cs#.raise-error ,condition-name ,...)
     (lua "end")))

(fn signal [condition-name ...]
  (assert-compile (not= 'nil condition-name)
                  "condition must not be nil"
                  condition-name)
  "Raise `condition-name' as an signal."
  `(let [cs# (require ,condition-system)]
     (lua "do return")
     (cs#.raise-signal ,condition-name ,...)
     (lua "end")))

{: restart-case
 : handler-bind
 : handler-case
 :error error*
 : signal
 : invoke-restart}
