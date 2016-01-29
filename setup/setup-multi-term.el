(require-package
 '(multi-term))

(require-binary
 '(zsh))

(setq multi-term-program "/bin/zsh")
(global-set-key (kbd "C-z") 'multi-term)

(provide 'setup-multi-term)
