(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         (consult-after-jump . (lambda ()
                                 (when (derived-mode-p 'prog-mode)
                                   (rainbow-delimiters-mode))))))
