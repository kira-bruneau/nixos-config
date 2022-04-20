(use-package drag-stuff
  :straight t
  :diminish "" ;; ↕
  :bind (("M-p" . drag-stuff-up)
         ("M-n" . drag-stuff-down))
  :init
  (drag-stuff-global-mode t))
