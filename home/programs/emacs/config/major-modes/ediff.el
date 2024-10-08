(use-package ediff
  :hook ((ediff-before-setup . my-store-pre-ediff-winconfig)
         (ediff-quit . my-restore-pre-ediff-winconfig))

  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)

  :config
  ;; Restore window configuration after quitting ediff
  ;; Source: https://emacs.stackexchange.com/questions/7482/restoring-windows-and-layout-after-an-ediff-session

  (defvar my-ediff-last-windows nil "Window configuration before ediff.")

  (defun my-store-pre-ediff-winconfig ()
    (setq my-ediff-last-windows (current-window-configuration)))

  (defun my-restore-pre-ediff-winconfig ()
    (set-window-configuration my-ediff-last-windows)))
