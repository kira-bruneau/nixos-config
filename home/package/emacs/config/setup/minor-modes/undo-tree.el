(use-package undo-tree
  :diminish "" ;; ψ
  :init
  (global-undo-tree-mode t)

  :config
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "aux")))))
