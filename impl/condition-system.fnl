(local module (: (or ... "") :gsub "impl%.condition%-system$" ""))
(local {: metadata} (require :fennel))
(local {: get-data
        : get-name
        : compose-error-message
        : pack
        :unpack _unpack}
  (require (.. module :impl.utils)))
(local {: invoke-debugger}
  (require (.. module :impl.debugger)))

;; Private library API
(local condition-system
  {:compose-error-message
   (metadata:set compose-error-message :fnl/docstring "See [utils.md#compose-error-message](utils.md#compose-error-message)")
   :pack
   (metadata:set pack :fnl/docstring "See [utils.md#pack](utils.md#pack)")
   :unpack
   (metadata:set _unpack :fnl/docstring "See [utils.md#unpack](utils.md#unpack)")})


;;; Handlers

(set condition-system.handlers
     (metadata:set {:handlers {}
                    :parent nil}
                   :fnl/docstring "Dynamic scope for condition handlers."))

(fn find-parent-handler [condition-object scope]
;;; Searches handler for `condition-object` parent in current scope
;;; `scope` only.
  (when condition-object
    (match (. scope.handlers (?. condition-object :id :parent :id))
      handler {: handler :target scope.target :data condition-object.data}
      nil (find-parent-handler condition-object.parent scope))))

(fn find-object-handler [condition-object scope]
;;; Searches the handler for the `condition-object` in dynamic scope
;;; `scope`.  If no handler is found in the current scope, searches
;;; for handlers of all condition object parents.  If no parent
;;; handler found goes to upper scope.
  (when scope
    (match (. scope.handlers condition-object.id)
      handler {: handler :target scope.target :data condition-object.data}
      nil (match (find-parent-handler condition-object scope)
            parent-handler parent-handler
            nil (find-object-handler condition-object scope.parent)))))

(fn find-primitive-handler [condition-object scope]
;;; Checks is object is present in dynamic scope and a handler is
;;; bound to it.
  (when scope
    (match (. scope.handlers condition-object)
      handler {: handler :target scope.target}
      nil (find-primitive-handler condition-object scope.parent))))

(fn find-handler [condition-object scope]
;;; Finds `condition-object' handler in dynamic scope `scope`.  If
;;; `condition-object' is a table with `type` key equal to
;;; `:condition` searches handler based on condition object's
;;; inheritance.  If anything else, searches handler by object
;;; reference.
  (if (and (= :table (type condition-object))
           (= :condition condition-object.type))
      (find-object-handler condition-object scope)
      (find-primitive-handler condition-object scope)))

(fn condition-system.handle [condition-object type*]
  "Handle the `condition-object' of `type*'.

Finds the `condition-object' handler in the dynamic scope.  If found,
calls the handler, and returns a table with `:state` set to
`:handled`, and `:data` bound to a packed table of handler's return
values."
  (match (find-handler condition-object condition-system.handlers)
    {: handler : target :data ?data}
    {:state :handled
     :data (pack (handler condition-object (_unpack (or ?data []))))
     :target target
     :condition condition-object
     :type type*}
    _ {:state :error
       :message (.. "no handler bound for condition: "
                    (get-name condition-object))}))


;;; Restarts

(set condition-system.restarts
     (metadata:set {:restarts []
                    :parent nil}
                   :fnl/docstring "Dynamic scope for restarts."))

(fn find-restart [restart-name scope]
;;; Searches `restart-name' in dynamic scope `scope`.  Modifies the
;;; restart object, by setting it's `:target` field to the `scope`
;;; target.
  (when scope
    (match (?. scope :restarts restart-name)
      restart (doto restart (tset :target scope.target))
      nil (find-restart restart-name scope.parent))))

(fn condition-system.invoke-restart [restart-name ...]
  "Searches for `restart-name' in the dynamic scope and invokes the
restart with given arguments.  Always throws error, as
`invoke-restart' must transfer control flow out of the handler.  If
restart is found, calls the restart function and returns a table with
`:state` set to `:restarted`, and `:data` bound to a packed table of
restart's return values."
  (error (match (find-restart restart-name condition-system.restarts)
           {: restart : target} {:state :restarted
                                 :data (pack (restart ...))
                                 :target target}
           _ {:state :error
              :message (.. "restart " restart-name " is not found")})))


;;; Conditions

(fn raise-signal [condition-object type*]
;;; Raises `condition-object' as a condition of given `type*`, or
;;; `:signal` if `type*` is not specified.  Conditions of types
;;; `:signal` and `:warn` do not interrupt program flow, but still can
;;; be handled.
  (match (condition-system.handle condition-object (or type* :signal))
    (where (or {:state :handled &as res}
               {:state :restarted &as res})) (error res)
    _ nil))

(fn raise-warning [condition-object]
;;; Raises `condition-object' as a warning.  If condition was not
;;; handled, prints the warning message to stderr, and continues.
  (match (raise-signal condition-object :warn)
    nil (do (io.stderr:write "WARNING: "
                             (compose-error-message condition-object)
                             "\n")
            nil)))

(fn raise-error [condition-object]
;;; The only raise that always throws it's result as an error when
;;; condition was not handled, unless `use-debugger?' is not set to
;;; logical true.  If `use-debugger?' is `true`, invokes the
;;; interactive debugger.
  (match (raise-signal condition-object :error)
    nil (if _G.fennel-conditions/use-debugger?
            (invoke-debugger condition-object condition-system.restarts)
            (error (compose-error-message condition-object)))))

(fn condition-system.raise [condition-type condition-object]
  "Raises `condition-object' as a condition of `condition-type'.
`condition-object' must not be `nil'."
  (assert (not= nil condition-object)
          "condition must not be nil")
  (match condition-type
    :signal (raise-signal condition-object)
    :warn (raise-warning condition-object)
    :error (raise-error condition-object)))

condition-system
