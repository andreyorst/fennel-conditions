(local condition-system
       `(require (let [[module#] [,...]]
                   (if (and module# (string.match module# :macros$))
                       (.. (string.gsub module# :macros "") :impl.condition-system)
                       :impl.condition-system))))

(fn seq-to-table [seq]
;;; Transform sequential bindings into associative table.
;;; [:a 1 :b 2] => {:a 1 :b 2}
  (let [tbl {}]
    (for [i 1 (length seq) 2]
      (tset tbl (. seq i) (. seq (+ i 1))))
    tbl))

(fn handler-bind [binding-vec ...]
  "Bind handlers to signals.

Accepts a sequence of signals and their respecting handlers followed
by the body expression.  Each handler is a function of at least one
argument - the signal being handled.  Other arguments are optional,
and can be used inside the handler.

If body expression signals a condition, a bound handler is invoked.
If no handler were bound for condition, condition is thrown as an
exception."
  (assert-compile (= (% (length binding-vec) 2) 0)
                  "expected even number of signal/handler bindings"
                  binding-vec)
  `(let [condition-system# ,condition-system
         binding-vec# ,(seq-to-table binding-vec)
         old# (collect [h# f# (pairs binding-vec#)]
                (let [old-f# (. condition-system#.signals h#)]
                  (tset condition-system#.signals h# f#)
                  (values h# old-f#)))
         res# (do ,...)]
     (each [h# f# (pairs old#)]
       (tset condition-system#.signals h# f#))
     res#))

(fn restart-case [expr ...]
  "Resumable exception restart point.
Accepts expression and restarts that can be used when handling
conditions thrown from within the expression."
  (let [restarts []]
    (each [_ [restart & fn-tail] (pairs [...])]
      (tset restarts restart (list 'fn (unpack fn-tail))))
    `(let [condition-system# ,condition-system
           old# (collect [n# f# (pairs ,restarts)]
                  (let [old-f# (. condition-system#.restarts n#)]
                    (tset condition-system#.restarts n# f#)
                    (values n# old-f#)))
           res# ,expr]
       (each [n# f# (pairs old#)]
         (tset condition-system#.restarts n# f#))
       res#)))

{: restart-case
 : handler-bind}
