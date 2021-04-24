(local {: view : metadata} (require :fennel))
(local _unpack (or table.unpack _G.unpack))

(local conditions {:scope {:conditions {}
                           :parent nil}})

(metadata:set conditions :fnl/docstring
              "Dynamic scope for conditions.")

(local restarts {:scope {:restarts {}
                         :parent nil}})

(metadata:set restarts :fnl/docstring
              "Dynamic scope for restarts.")


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

(fn raise-error* [scope condition-name ...]
  (match (and scope (find-condition condition-name scope))
    condition (match (pcall condition condition-name ...)
                (false {:handled true : data}) (_unpack data)
                _ (raise-error* scope.parent condition-name ...))
    _ (let [msg (.. "condition " (view condition-name {:one-line? true})
                    " was raised with the following arguments: "
                    (table.concat (icollect [_ v (ipairs [...])]
                                    (view v {:one-line? true})) ", "))]
        (error msg 2))))

(fn raise-error [condition-name ...]
  (raise-error* conditions.scope condition-name ...))

(fn raise-signal [condition-name ...]
  (match (pcall raise-error* conditions.scope condition-name ...)
    (true res) res
    _ nil))

(fn raise-warning [condition-name ...]
  (match (pcall raise-error* conditions.scope condition-name ...)
    (true res) res
    (false msg) (do (io.stderr:write "WARNING: " msg "\n") nil)))

(fn raise [t condition-name ...]
  "Raise `condition-name' of type `t' with given arguments.
Supported types include: `:signal`, `:warn`, and `:error`."
  (match t
    :signal (raise-signal condition-name ...)
    :warn (raise-warning condition-name ...)
    :error (raise-error condition-name ...)))

(fn invoke-restart* [scope restart-name ...]
  (if scope
      (match (find-restart restart-name scope)
        restart (restart ...)
        _ (invoke-restart* scope.parent restart-name ...))
      (error (.. "restart " restart-name " is not defined"))))

(fn invoke-restart [restart-name ...]
  "Invoke `restart-name' with args."
  (invoke-restart* restarts.scope restart-name ...))

(setmetatable
 {: invoke-restart
  : raise
  : restarts
  : conditions}
 {:__index
  {:_DESCRIPTION "Internal API for condition system."}})
