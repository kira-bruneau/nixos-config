(require-package
 '(yasnippet))

(yas-global-mode 1)
(diminish 'yas-minor-mode) ;; ✂

(setq yas-verbosity 2)
(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate t)))

(provide 'setup-yasnippet)
