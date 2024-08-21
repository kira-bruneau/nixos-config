(use-package nameless
  :hook ((emacs-lisp-mode . nameless-mode)
         (consult-after-jump . (lambda ()
                                 (when (derived-mode-p 'emacs-lisp-mode)
                                   (nameless-mode))))))
