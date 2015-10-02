(require-package
 '(company))

;; (company-quickhelp-mode 1)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 1)
(setq company-global-modes '(not eshell-mode))
(add-hook 'after-init-hook 'global-company-mode)

(provide 'setup-company)
