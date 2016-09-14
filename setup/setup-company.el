(require-package
 '(company
   company-flx))

(setq company-global-modes '(not eshell-mode gud-mode))
(setq company-clang-insert-arguments t)
(add-hook 'after-init-hook 'global-company-mode)

(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case nil)

(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0)

(setq company-flx-limit 100)
(with-eval-after-load 'company
  (company-flx-mode t))

(provide 'setup-company)
