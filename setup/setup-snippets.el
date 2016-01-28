(require-package
 '(yasnippet))

(setq yas-verbosity 2)
(yas-global-mode 1)

(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate t)))

(provide 'setup-snippets)
