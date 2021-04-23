(local {: view} (require :fennel))
(local _unpack (or _G.unpack table.unpack))
(local conditions {:scope {:conditions {}
                           :parent nil}})
(local restarts {:scope {:restarts {}
                         :parent nil}})

(fn find [scope-type name scope]
  (match (?. scope scope-type name)
    n n
    _ (match scope.parent
        parent (find scope-type name parent)
        _ nil)))

(fn find-condition [name scope]
  (find :conditions name scope))

(fn find-restart [name scope]
  (find :restarts name scope))

(fn signal-error* [scope condition-name ...]
  (match (and scope (find-condition condition-name scope))
    condition (match (pcall condition condition-name ...)
                (false {:handled true : data}) (_unpack data)
                _ (signal-error* scope.parent condition-name ...))
    _ (let [msg (.. "condition " (view condition-name {:one-line? true})
                    " was thrown with the following arguments: "
                    (table.concat (icollect [_ v (ipairs [...])]
                                    (view v {:one-line? true})) ", "))]
        (error msg))))

(fn signal-error [condition-name ...]
  (signal-error* conditions.scope condition-name ...))

(fn signal-signal [condition-name ...]
  (match (pcall signal-error* conditions.scope condition-name ...)
    (true res) res
    _ nil))

(fn invoke-restart* [scope restart-name ...]
  (if scope
      (match (find-restart restart-name scope)
        restart (restart ...)
        _ (invoke-restart* scope.parent restart-name ...))
      (error (.. "restart " restart-name " is not defined"))))

(fn invoke-restart [restart-name ...]
  (invoke-restart* restarts.scope restart-name ...))

{: invoke-restart
 : signal-error
 : signal-signal
 : restarts
 : conditions}
