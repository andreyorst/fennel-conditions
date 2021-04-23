;; Constructing relative path for runtime require
(local condition-system
       (if (and ... (string.match ... "conditions$"))
           (.. (string.gsub ... "conditions$" "") :impl.condition-system)
           :impl.condition-system))

(fn seq-to-table [seq]
;;; Transform sequential bindings into associative table.
;;; [:a 1 :b 2] => {:a 1 :b 2}
  (let [tbl {}]
    (for [i 1 (length seq) 2]
      (tset tbl (. seq i) (. seq (+ i 1))))
    tbl))

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
  `(let [cs# (require ,condition-system)
         bindings# ,(seq-to-table binding-vec)
         scope# {:conditions {}
                 :parent cs#.conditions.scope}]
     (each [h# f# (pairs bindings#)]
       (tset scope#.conditions h# f#))
     (tset cs#.conditions :scope scope#)
     (let [(ok# res#) (pcall #(do ,...))]
       (tset cs#.conditions :scope scope#.parent)
       (if ok# res# (assert false res#)))))

(fn restart-case [expr ...]
  "Resumable exception restart point.
Accepts expression and restarts that can be used when handling
conditions thrown from within the expression."
  (let [restarts (collect [_ [restart & fn-tail] (ipairs [...])]
                   (values restart (list 'fn (unpack fn-tail))))]
    `(let [cs# (require ,condition-system)
           scope# {:restarts {}
                   :parent cs#.restarts.scope}]
       (each [n# f# (pairs ,restarts)]
         (tset scope#.restarts n# f#))
       (tset cs#.restarts :scope scope#)
       (let [(ok# res#) (pcall (fn [] ,expr))]
         (tset cs#.restarts :scope scope#.parent)
         (if ok# res# (assert false res#))))))

(fn handler-case [expr ...]
  (let [handlers (collect [_ [handler & fn-tail] (ipairs [...])]
                   (values handler `(fn [...]
                                      (assert false
                                              {:handled true
                                               :data [((fn ,(unpack fn-tail)) ...)]}))))]
    `(let [cs# (require ,condition-system)
           scope# {:conditions {}
                   :parent cs#.conditions.scope}]
       (each [h# f# (pairs ,handlers)]
         (tset scope#.conditions h# f#))
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


;; TODO: use `gensym' to capture `signal-error' result and return it
;;       once https://todo.sr.ht/~technomancy/fennel/54 is fixed
(fn error* [condition-name ...]
  "Signal `condition-name' as an error."
  `(let [cs# (require ,condition-system)]
     (lua "do return")
     (cs#.signal-error ,condition-name ,...)
     (lua "end")))

(fn signal [condition-name ...]
  "Signal `condition-name' as an signal."
  `(let [cs# (require ,condition-system)]
     (lua "do return")
     (cs#.signal-signal ,condition-name ,...)
     (lua "end")))

{: restart-case
 : handler-bind
 : handler-case
 :error error*
 : signal
 : invoke-restart}
