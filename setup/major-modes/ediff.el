(use-package ediff
  :ensure-system-package (diff . diffutils)
  :hook ((ediff-before-setup . my-store-pre-ediff-winconfig)
         (ediff-quit . my-restore-pre-ediff-winconfig))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)



  ;; Restore window configuration after quitting ediff
  ;; Source: https://emacs.stackexchange.com/questions/7482/restoring-windows-and-layout-after-an-ediff-session

  (defvar my-ediff-last-windows nil "Window configuration before ediff.")

  (defun my-store-pre-ediff-winconfig ()
    (setq my-ediff-last-windows (current-window-configuration)))

  (defun my-restore-pre-ediff-winconfig ()
    (set-window-configuration my-ediff-last-windows)))
