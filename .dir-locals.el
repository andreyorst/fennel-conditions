;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((fennel-mode . ((eval . (font-lock-add-keywords 'fennel-mode
                                                 `((,(rx word-start
                                                         (group
                                                          (or "restart-case"
                                                              "handler-bind"
                                                              "handler-case"
                                                              "signal"
                                                              "error"
                                                              "invoke-restart"))
                                                         word-end)
                                                    1 'font-lock-keyword-face))))
                 (eval . (put 'handler-bind 'fennel-indent-function 1))
                 (eval . (put 'restart-case 'fennel-indent-function 1))
                 (eval . (put 'handler-case 'fennel-indent-function 1)))))
