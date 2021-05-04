;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((fennel-mode . ((eval . (font-lock-add-keywords 'fennel-mode '(("\\<\\(handler-bind\\)\\>" 1 'font-lock-keyword-face))))
                 (eval . (font-lock-add-keywords 'fennel-mode '(("\\<\\(handler-case\\)\\>" 1 'font-lock-keyword-face))))
                 (eval . (font-lock-add-keywords 'fennel-mode '(("\\<\\(restart-case\\)\\>" 1 'font-lock-keyword-face))))
                 (eval . (font-lock-add-keywords 'fennel-mode '(("\\<\\(define-condition\\)\\>" 1 'font-lock-keyword-face))))
                 (eval . (font-lock-add-keywords 'fennel-mode '(("\\<\\(cerror\\)\\>" 1 'font-lock-keyword-face))))
                 (eval . (font-lock-add-keywords 'fennel-mode '(("\\<\\(ignore-errors\\)\\>" 1 'font-lock-keyword-face))))
                 (eval . (put 'handler-bind 'fennel-indent-function 1))
                 (eval . (put 'handler-case 'fennel-indent-function 1))
                 (eval . (put 'restart-case 'fennel-indent-function 1))
                 (eval . (put 'ignore-errors 'fennel-indent-function 0)))))
