(require-package
 '(company))

;; (company-quickhelp-mode)
(setq company-global-modes '(not eshell-mode))
(setq company-clang-insert-arguments nil)
(add-hook 'after-init-hook 'global-company-mode)

(provide 'setup-company)
