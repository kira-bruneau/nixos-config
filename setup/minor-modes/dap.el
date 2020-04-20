(use-package dap-mode
  :straight t
  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :init
  (require 'dap-gdb-lldb)
  (require 'dap-python)

  (dap-mode t)
  (dap-tooltip-mode t)
  (dap-ui-mode t)

  (setq treemacs-show-cursor t))
