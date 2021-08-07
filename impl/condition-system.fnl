(local {: metadata : view} (require :fennel))
(local {: dynamic-scope
        : current-thread
        : compose-error-message
        : get-data
        : get-name
        : build-arg-str
        : unpack
        : pack}
  (require (: (or ... "") :gsub "(impl%.)condition%-system$" "%1utils")))

(local invoke-debugger
  (require (: (or ... "") :gsub "(impl%.)condition%-system$" "%1debugger")))

;;; Default condition objects

(local Condition
  {:name "condition"
   :type "condition"})
(tset Condition :id Condition)
(metadata:set Condition
              :fnl/docstring
              "Condition object that acts as a base for all conditions.")

(local Warning
  {:name "warning"
   :parent Condition
   :type "condition"})
(tset Warning :id Warning)
(metadata:set Warning
              :fnl/docstring
              "Condition object that acts as a base for all warning conditions.
Inherits `Condition'.")

(local Error
  {:name "error"
   :parent Condition
   :type "condition"})
(tset Error :id Error)
(metadata:set Error
              :fnl/docstring
              "Condition object that acts as a base for all error conditions.
Inherits `Condition'.")

;;; Handlers

(fn find-parent-handler [condition-object scope]
  ;; Searches handler for `condition-object` parent in current scope
  ;; `scope` only.
  (when condition-object
    (match (?. scope.handler (?. condition-object :id :parent :id))
      handler {: handler : scope}
      nil (find-parent-handler condition-object.parent scope))))

(fn find-object-handler [condition-object type* scope]
  ;; Searches the handler for the `condition-object` in dynamic scope
  ;; `scope`.  If no handler is found in the current scope, searches
  ;; for handlers of all condition object parents.  If no parent
  ;; handler found goes to upper scope.
  (when scope
    (let [h scope.handler]
      (match (or (?. h condition-object.id)
                 (?. h (match type*
                         :error Error
                         :warning Warning))
                 (?. h Condition))
        handler {: handler : scope}
        nil (match (find-parent-handler condition-object scope)
              parent-handler parent-handler
              nil (find-object-handler condition-object type* scope.parent))))))

(fn find-primitive-handler [condition-object type* scope]
  ;; Checks is object is present in dynamic scope and a handler is
  ;; bound to it.
  (when scope
    (let [h scope.handler]
      (match (or (?. h condition-object)
                 (?. h (match type*
                         :error Error
                         :warning Warning))
                 (?. h Condition))
        handler {: handler : scope}
        nil (find-primitive-handler condition-object type* scope.parent)))))

(fn find-handler [condition-object type* scope]
  "Finds the `condition-object' handler of `type*` in dynamic scope
`scope`.  If `condition-object' is a table with `type` key equal to
`:condition` searches handler based on condition object's inheritance.
If anything else, searches handler by object reference."
  (if (and (= :table (type condition-object))
           (= :condition condition-object.type))
      (find-object-handler condition-object type* scope)
      (find-primitive-handler condition-object type* scope)))

(fn handle [condition-object type* ?scope]
  "Handle the `condition-object' of `type*' and optional `?scope`.

Finds the `condition-object' handler in the dynamic scope.  If found,
calls the handler, and returns a table with `:state` set to
`:handled`, and `:handler` bound to an anonymous function that calls
the restart."
  (let [thread (current-thread)
        thread-scope (. dynamic-scope thread)]
    (match (find-handler
            condition-object
            type*
            (or ?scope thread-scope.handlers))
      {: handler : scope}
      (do (tset thread-scope :current-context scope)
          (match scope.handler-type
            :handler-case {:state :handled
                           :handler #(handler condition-object (unpack (get-data condition-object)))
                           :target scope.target
                           :condition-object condition-object
                           :type type*}
            :handler-bind (do (handler condition-object (unpack (get-data condition-object)))
                              (handle condition-object type* scope.parent))
            _ {:state :error
               :message (.. "wrong handler-type: " (view _))
               :condition condition-object}))
      _ {:state :error
         :message (.. "no handler bound for condition: "
                      (get-name condition-object))
         :condition condition-object})))


;;; Restarts

(fn find-restart [restart-name scope]
  "Searches `restart-name' in dynamic scope `scope`."
  (when scope
    (match (?. scope :restart)
      (where restart (= restart.name restart-name))
      (values restart.restart scope.target)
      _ (find-restart restart-name scope.parent))))

(fn invoke-restart [restart-name ...]
  "Searches for `restart-name' in the dynamic scope and invokes the
restart with given arguments.  Always throws error, as
`invoke-restart' must transfer control flow out of the handler.  If
restart is found, calls the restart function and returns a table with
`:state` set to `:restarted`, and `:restart` bound to the restart
function."
  (let [args (pack ...)
        thread-scope (. dynamic-scope (current-thread))]
    (error (match (find-restart restart-name thread-scope.restarts)
             (restart target) {:state :restarted
                               :restart #(restart (unpack args))
                               :target target}
             _ (let [msg (.. "restart " (view restart-name) " is not found")]
                 (if thread-scope.current-context
                     {:state :error
                      :message msg}
                     msg))) 2)))

;;; Conditions

(fn raise-condition [condition-object type*]
  ;; Raises `condition-object' as a condition of given `type*`, or
  ;; `:condition` if `type*` is not specified.  Conditions of types
  ;; `:condition` and `:warn` do not interrupt program flow, but still
  ;; can be handled.
  (match (handle condition-object (or type* :condition))
    (where (or {:state :handled &as res}
               {:state :restarted &as res}))
    (error res 2)
    _ nil))

(fn raise-warning [condition-object]
  ;; Raises `condition-object' as a warning.  If condition was not
  ;; handled, prints the warning message to stderr, and continues.
  (match (raise-condition condition-object :warning)
    nil (do (io.stderr:write "WARNING: "
                             (compose-error-message condition-object)
                             "\n")
            nil)))

(fn raise-error [condition-object]
  ;; The only raise that always throws it's result as an error when
  ;; condition was not handled, unless
  ;; `condition-system-use-debugger?' is not set to logical true.  If
  ;; `condition-system-use-debugger?' is `true`, invokes the
  ;; interactive debugger.
  (match (raise-condition condition-object :error)
    nil (if _G.condition-system-use-debugger?
            (invoke-debugger condition-object)
            (error (compose-error-message condition-object) 2))))

(fn raise [condition-type condition-object]
  "Raises `condition-object' as a condition of `condition-type'.
`condition-object' must not be `nil'."
  (assert (not= nil condition-object)
          "condition must not be nil")
  ;; If condition was raided inside handler we need to unwind the
  ;; stack to the point where we were in the handler.  Each `handle`
  ;; invocation sets the `current-context` field, and this field is
  ;; cleared when we exit the handler.
  (let [thread (current-thread)
        thread-scope (do (when (not (. dynamic-scope thread))
                           (tset dynamic-scope thread {:handlers {} :restarts {}}))
                         (. dynamic-scope thread))]
    (match thread-scope.current-context
      scope (let [target scope.target]
              (var scope scope.parent)
              (while (and scope (= target scope.target))
                (set scope scope.parent))
              (tset thread-scope :handlers scope)))
    (match condition-type
      :condition (raise-condition condition-object)
      :warning (raise-warning condition-object)
      :error (raise-error condition-object))))

(fn condition= [c1 c2]
  "Compare `c1` and `c2` condition objects by their `id` field."
  (and (= c1.type :condition)
       (= c2.type :condition)
       (rawequal c1.id c2.id)))

(fn pcall-handler-bind [f scope target orig-handlers]
  "Call `f` in given `scope` and pass result up to `target`.
Restore dynamic scope handlers to `orig-handlers`."
  (let [(ok res) (pcall f)]
    ;; Reset current scope context after exiting user code, but before
    ;; actually handling the condition in order to avoid infinity
    ;; loops.
    (doto scope
      (tset :current-context nil)
      (tset :handlers orig-handlers))
    (if ok (unpack res)
        (match res
          ;; Handled conditions are re-raised because `handler-bind`
          ;; requires exiting handler with `restart-case` or another
          ;; error.
          {:state :handled : target}
          (raise res.type res.condition-object)
          ;; Internal errors bubble up the dynamic scope until no
          ;; handlers and restarts left, to make sure we can't catch
          ;; those accidentally.
          {:state :error :message msg}
          (if (or scope.handlers.parent
                  scope.restarts.parent)
              (error res)
              (error msg 2))
          _ (error res)))))

(fn pcall-restart-case [f scope target orig-restarts]
  "Call `f` in given `scope` and pass result up to `target`.
Restore dynamic scope restarts to `orig-restarts`."
  (let [restarts scope.restarts
        (ok res) (pcall f)]
    (doto scope
      (tset :restarts orig-restarts)
      (tset :current-context nil))
    (if ok (unpack res)
        (match res
          {:state :restarted :target target} (res.restart)
          {:state :restarted} (error res)
          {:state :error :message msg}
          (if (or scope.handlers.parent
                  scope.restarts.parent)
              (error res)
              (error msg 2))
          ;; Encountered Lua error, we restore bound restarts, and
          ;; try to handle it as a condition.  Mostly repeats what
          ;; was done for a condition
          _ (let [(_ res2) (do (set scope.restarts restarts)
                               (pcall raise :error res))]
              (doto scope
                (tset :restarts orig-restarts)
                (tset :current-context nil))
              (match res2
                {:state :restarted : target} (res2.restart)
                {:state :restarted} (error res2)
                {:state :error :message msg2}
                (if (or scope.handlers.parent
                        scope.restarts.parent)
                    (error res2)
                    (error msg2 2))
                ;; If Lua error was not handled, we throw it
                ;; instead of fail result from the handler
                _ (error res)))))))

(fn pcall-handler-case [f scope target orig-handlers]
  "Call `f` in given `scope` and pass result up to `target`.
Restore dynamic scope handlers to `orig-handlers`."
  (let [handlers scope.handlers
        (ok res) (pcall f)]
    (doto scope
      (tset :handlers orig-handlers)
      (tset :current-context nil))
    (if ok (unpack res)
        (match res
          {:state :handled : target : handler} (handler)
          {:state :error : message}
          (if (or scope.handlers.parent
                  scope.restarts.parent)
              (error res)
              (error message 2))
          {:state :handled} (error res)
          ;; In case Lua error was raised we reuse handlers defined
          ;; within this `handler-case` and up the dynamic scope.
          _ (match (find-handler res :error handlers)
              {:handler handler} (handler res)
              _ (error res))))))

(setmetatable
 {: raise
  : invoke-restart
  : handle
  : find-handler
  : find-restart
  : Condition
  : Error
  : Warning
  : condition=
  : pcall-handler-bind
  : pcall-restart-case
  : pcall-handler-case}
 {:__index {:_DESCRIPTION "Condition system for Fennel language.

This module is library's private API that provides functions meant for
internal use only.  For public API docs see
[fennel-conditions.md](../fennel-conditions.md) and
[macros.md](../macros.md)"}})
