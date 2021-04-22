(local {: view} (require :fennel))

(local conditions {:scope {:conditions {}
                           :parent nil}})
(local restarts {:scope {:restarts {}
                         :parent nil}})

(fn find-scope [scope-type name scope]
  (match (?. scope scope-type name)
    n scope
    _ (match scope.parent
        parent (find-scope scope-type name parent)
        _ nil)))

(fn find-condition-scope [name scope]
  (find-scope :conditions name scope))

(fn find-restart-scope [name scope]
  (find-scope :restarts name scope))

(fn throw-error [condition ...]
  (error (.. "condition " condition
             " was thrown with the following arguments: "
             (table.concat (icollect [_ v (ipairs [...])] (view v {:one-line? true})) ", "))))

(fn signal-error* [scope condition ...]
  (match (and scope (find-condition-scope condition scope))
    s (match [((. s.conditions condition) condition ...)]
        [{:restart true} & rest] (unpack rest)
        _ (signal-error* s.parent condition ...))
    _ (throw-error condition ...)))

(fn signal-error [signal-name ...]
  (signal-error* conditions.scope signal-name ...))

(fn invoke-restart* [scope restart-name ...]
  (match (and scope (find-restart-scope restart-name scope))
    s (match (. s.restarts restart-name)
        restart (restart ...)
        _ (invoke-restart* s.parent restart-name ...))
    _ (error (.. "restart " restart-name " is not defined"))))

(fn invoke-restart [restart-name ...]
  (invoke-restart* restarts.scope restart-name ...))

{: invoke-restart
 : signal-error
 : restarts
 : conditions}
