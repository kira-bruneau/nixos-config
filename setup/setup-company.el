(require-package
 '(company))

(setq company-global-modes '(not eshell-mode))
(setq company-clang-insert-arguments nil)
(add-hook 'after-init-hook 'global-company-mode)

(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case nil)

(provide 'setup-company)
