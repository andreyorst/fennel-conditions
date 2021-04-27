(local {: view : metadata : eval} (require :fennel))
(local _unpack (or table.unpack _G.unpack))

(local conditions
  (metadata:set {:scope {:conditions {}
                         :parent nil}}
                :fnl/docstring "Dynamic scope for conditions."))

(local restarts
  (metadata:set {:scope {:restarts []
                         :parent nil}}
                :fnl/docstring "Dynamic scope for restarts."))

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

(fn build-arg-str [sep args]
  (let [res []]
    (for [i 1 args.n]
      (table.insert res (view (. args i) {:one-line? true})))
    (table.concat res sep)))

(fn throw-error* [condition-object ...]
  (let [args (table.pack ...)
        msg (.. "condition " (view condition-object {:one-line? true})
                " was raised"
                (if (> args.n 0)
                    (..  " with the following arguments: "
                         (build-arg-str ", " args))
                    ""))]
    (error msg 2)))

(fn build-arg-str [sep args]
  (let [res []]
    (for [i 1 args.n]
      (table.insert res (view (. args i) {:one-line? true})))
    (table.concat res sep)))

(fn flatten-restarts [restarts scope]
  (if scope
      (let [ordered []]
        (each [name {: n &as restart} (pairs scope.restarts)]
          (tset ordered n restart))
        (each [_ restart (ipairs ordered)]
          (table.insert restarts restart))
        (flatten-restarts restarts scope.parent))
      restarts))

(fn take-action [{: name : handler : interactive? : docstring : args}]
  (if interactive?
      (do (io.stderr:write
           "Provide inputs for "
           (if args
               (.. name " (args: [" (table.concat args " ") "])")
               name)
           " (^D to cancel)\n"
           "debugger:" name ">> ")
          (match (io.stdin:read "*l")
            input {:handled true
                   :data [(handler (_unpack (eval (.. "[" input "]") {:env _G})))]}
            _ (do (io.stderr:write "\n")
                  (error :cancel))))
      (handler)))

(fn display-restart-prompt [restarts]
  (each [i {: name : handler &as handler-object} (ipairs restarts)]
    (io.stderr:write i ": [" name "]"
                     (match handler-object.description
                       description (.. " " (description:gsub "\n" " "))
                       _ "") "\n"))
  (io.stderr:write "debugger>> "))

(fn restart-menu [restarts]
;;; Reads user input, asserts if it is a number.  If not, asks input
;;; again.  The last restart is always non-interactive internal
;;; restart defined by the debugger.  Other restarts are considered
;;; interactive.
  (display-restart-prompt restarts)
  (let [n (length restarts)]
    (match (tonumber (io.stdin:read "*l"))
      ;; "throw" restart
      n (let [{: handler} (. restarts n)]
          (handler))
      ;; user-defined restarts
      (where action (<= 1 action (- n 1)))
      (match (pcall take-action (. restarts action))
        (true {:handled true : data &as res}) (_unpack data)
        (false _) (restart-menu restarts))
      _ (do (io.stderr:write
             "\rWrong action. Use number between 1 and "  n "\n")
            (restart-menu restarts)))))

(fn invoke-debugger [scope condition-object ...]
  (let [args (table.pack ...)
        restarts (flatten-restarts [] scope)]
    (table.insert restarts
                  {:name "throw"
                   :handler #(throw-error* condition-object (_unpack args))
                   :description "Throw condition as a Lua error"})
    (io.stderr:write
     "Debugger was invoked on unhandled condition "
     (view condition-object {:one-line? true})
     (if (> args.n 0)
         (.. ", raised with the following arguments: "
             (build-arg-str ", " args))
         "")
     "\n")
    (restart-menu restarts)))

(fn throw-error [try-debugger? condition-object ...]
  (if (and try-debugger? _G.fennel-conditions/use-debugger?)
      (invoke-debugger restarts.scope condition-object ...)
      (throw-error* condition-object ...)))

(fn raise-error* [try-debugger? scope condition-object ...]
  (match (and scope (find-condition condition-object scope))
    condition (match (pcall condition condition-object ...)
                (false {:handled true : data}) (_unpack data)
                _ (raise-error* scope.parent condition-object ...))
    _ (throw-error try-debugger? condition-object ...)))

(fn raise-error [condition-object ...]
  (raise-error* true conditions.scope condition-object ...))

(fn raise-signal [condition-object ...]
  (match (pcall raise-error* false conditions.scope condition-object ...)
    (true res) res
    _ nil))

(fn raise-warning [condition-object ...]
  (match (pcall raise-error* false conditions.scope condition-object ...)
    (true res) res
    (false msg) (do (io.stderr:write "WARNING: " msg "\n") nil)))

(fn raise [t condition-object ...]
  "Raise `condition-object' of type `t' with given arguments.
Supported types include: `:signal`, `:warn`, and `:error`."
  (match t
    :signal (raise-signal condition-object ...)
    :warn (raise-warning condition-object ...)
    :error (raise-error condition-object ...)))

(fn invoke-restart* [scope restart-name ...]
  (if scope
      (match (find-restart restart-name scope)
        {:handler restart} (restart ...)
        _ (invoke-restart* scope.parent restart-name ...))
      (error (.. "restart " restart-name " is not defined") 2)))

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
