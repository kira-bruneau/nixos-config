(use-package company
  :diminish " ▶"
  :init
  (global-company-mode)

  :config
  (setq company-global-modes '(not eshell-mode gud-mode))
  (setq company-clang-insert-arguments t)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0))

(use-package company-flx
  :init
  (company-flx-mode))
