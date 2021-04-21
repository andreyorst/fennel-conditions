(local condition-system (require (if (and ... (not= ... :init)) (.. ... :.impl.condition-system) :impl.condition-system)))

(fn error* [signal-name ...]
  (match (. condition-system.signals signal-name)
    signal (signal signal-name ...)
    _ (error (.. "condition " signal-name
                 " was thrown with the following arguments: " (table.concat [...] ", ")))))

(fn invoke-restart [restart-name ...]
  (match (. condition-system.restarts restart-name)
    restart (restart ...)
    _ (error (.. "restart " restart-name " is not defined"))))

{:error error*
 : invoke-restart}
