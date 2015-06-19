;; (company-quickhelp-mode 1)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 1)
(add-hook 'after-init-hook 'global-company-mode)

(provide 'setup-company)
