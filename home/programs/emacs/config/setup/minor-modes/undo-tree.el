(use-package undo-tree
  :demand
  :diminish "" ;; ψ
  :config
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "aux"))))
  (global-undo-tree-mode t))
