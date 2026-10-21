(use-package rainbow-delimiters
  :hook ((lisp-data-mode . rainbow-delimiters-mode)
         (consult-after-jump . (lambda ()
                                 (when (derived-mode-p 'lisp-data-mode)
                                   (rainbow-delimiters-mode))))))
