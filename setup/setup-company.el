(require-package
 '(company
   company-flx))

(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (diminish 'company-mode " ▶")
  (company-flx-mode t))

(setq company-global-modes '(not eshell-mode gud-mode))
(setq company-clang-insert-arguments t)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case nil)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0)
(setq company-tooltip-align-annotations t)

(setq company-flx-limit 100)

(provide 'setup-company)
