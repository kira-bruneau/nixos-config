(require-package
 '(multi-term))

(require-binary
 '(zsh))

(setq multi-term-program "/bin/zsh")
(global-set-key (kbd "C-S-z") 'multi-term-dedicated-toggle)
(setq multi-term-dedicated-select-after-open-p t)

(provide 'setup-multi-term)
