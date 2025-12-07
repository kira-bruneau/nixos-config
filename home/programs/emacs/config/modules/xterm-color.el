(use-package xterm-color
  :init
  ;; Comint
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))

  (define-advice comint-term-environment (:filter-return (environment) append)
    (append '("TERM=xterm-256color" "PAGER=") environment))

  (add-hook 'comint-mode-hook
            (lambda ()
              ;; Disable font-locking in this buffer to improve performance
              (font-lock-mode -1)

              ;; Prevent font-locking from being re-enabled in this buffer
              (make-local-variable 'font-lock-function)
              (setq font-lock-function (lambda (_) nil))

              (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

  ;; Compilation buffers
  (setq compilation-environment '("TERM=xterm-256color" "PAGER="))

  (define-advice compilation-filter (:around (f proc string) xterm-color)
    (funcall f proc (xterm-color-filter string)))

  ;; Eshell
  (with-eval-after-load 'eshell
    (add-hook 'eshell-before-prompt-hook
              (lambda ()
                (setq xterm-color-preserve-properties t)))

    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
    (setenv "TERM" "xterm-256color")))
