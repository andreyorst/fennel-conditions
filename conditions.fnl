(local cs
       `(require (let [[module#] [,...]]
                   (if (and module# (string.match module# :conditions$))
                       (.. (string.gsub module# :conditions$ "") :impl.condition-system)
                       :impl.condition-system))))

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
  `(let [cs# ,cs
         bindings# ,(seq-to-table binding-vec)
         scope# {:conditions {}
                 :parent cs#.conditions.scope}
         _# (do (each [h# f# (pairs bindings#)]
                  (tset scope#.conditions h# f#))
                (tset cs#.conditions :scope scope#))
         res# (do ,...)]
     (tset cs#.conditions :scope scope#.parent)
     res#))

(fn restart-case [expr ...]
  "Resumable exception restart point.
Accepts expression and restarts that can be used when handling
conditions thrown from within the expression."
  (let [restarts []]
    (each [_ [restart & fn-tail] (pairs [...])]
      (tset restarts restart (list 'fn (unpack fn-tail))))
    `(let [cs# ,cs
           scope# {:restarts {}
                   :parent cs#.restarts.scope}
           _# (do (each [n# f# (pairs ,restarts)]
                    (tset scope#.restarts n# f#))
                  (tset cs#.restarts :scope scope#))
           res# ,expr]
       (tset cs#.restarts :scope scope#.parent)
       res#)))

(fn invoke-restart [restart-name ...]
  "Invoke restart `restart-name' to handle condition.
Must be used only in handler functions defined with `handler-bind'."
  `(let [cs# ,cs]
     (_G.error {:restart true
                :data [(cs#.invoke-restart ,restart-name ,...)]})))


(fn error* [condition-name ...]
  "Signal `condition-name' as an error."
  ;; TODO: replace sym with gensym once the upstream bug is fixed
  (let [s (sym :_internal_condition_result_)]
    `(let [cs# ,cs
           ,s (cs#.signal-error ,condition-name ,...)]
       (lua ,(.. "do return " (tostring s) " end")))))

{: restart-case
 : handler-bind
 :error error*
 : invoke-restart}
