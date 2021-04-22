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

(fn throw-error [condition ...]
  (error (.. "condition " condition
             " was thrown with the following arguments: "
             (table.concat (icollect [_ v (ipairs [...])] (view v {:one-line? true})) ", "))))

(fn signal-error* [scope condition-name ...]
  (match (and scope (find-condition condition-name scope))
    condition (match (pcall condition condition-name ...)
                (false {:restart true : data}) (_unpack data)
                _ (signal-error* scope.parent condition-name ...))
    _ (throw-error condition-name ...)))

(fn signal-error [signal-name ...]
  (signal-error* conditions.scope signal-name ...))

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
 : restarts
 : conditions}
