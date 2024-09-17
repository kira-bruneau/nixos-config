(use-package undo-tree
  :demand
  :config
  (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "aux"))))
  (global-undo-tree-mode))
