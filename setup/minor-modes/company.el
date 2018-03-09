(use-package company
  :straight t
  :diminish " ▶"
  :config
  (setq company-global-modes '(not eshell-mode gud-mode))
  (setq company-clang-insert-arguments t)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  (setq company-tooltip-align-annotations t)
  (global-company-mode))

(use-package company-flx
  :straight t
  :after company
  :config
  (setq company-flx-limit 100)
  (company-flx-mode t))
