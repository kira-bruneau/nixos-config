(use-package embark
  :bind*
  (("C-." . embark-act)
   ("C-," . embark-dwim)
   ("C-h B" . embark-bindings)

   :map embark-file-map
   ("s" . sudo-edit-find-file)

   :map embark-identifier-map
   ("RET" . embark-find-definition)
   ("d" . embark-find-definition)
   ("f" . embark-find-references)
   ("h" . embark-show-documentation)
   ("i" . embark-find-impl)
   ("r" . embark-rename)
   ("s" . embark-ripgrep-references)
   ("x" . embark-execute)
   ("I" . info-lookup-symbol)

   :map embark-symbol-map
   ("s" . embark-ripgrep-references)
   ("I" . embark-info-lookup-symbol))

  :config
  (defun embark-find-definition (thing)
    (cond
     ((bound-and-true-p lsp-bridge-mode) (lsp-bridge-find-def))
     (t (xref-find-definitions thing))))

  (defun embark-show-documentation (thing)
    (cond
     ((bound-and-true-p lsp-bridge-mode) (lsp-bridge-show-documentation))
     (t (display-local-help))))

  (defun embark-rename (thing)
    (when (bound-and-true-p lsp-bridge-mode)
      (lsp-bridge-rename)))

  (defun embark-find-references (thing)
    (when (bound-and-true-p lsp-bridge-mode)
      (lsp-bridge-find-references)))

  (defun embark-find-impl (thing)
    (when (bound-and-true-p lsp-bridge-mode)
      (lsp-bridge-find-impl)))

  (defun embark-ripgrep-references (thing)
    (project-consult-ripgrep nil thing))

  (defun embark-execute (thing)
    (cond
     ((bound-and-true-p lsp-bridge-mode) (lsp-bridge-code-action))))

  (add-to-list 'embark-pre-action-hooks
   '(embark-find-definition embark-evil-set-jump))

  (cl-defun embark-evil-set-jump (&key bounds &allow-other-keys)
    (evil-set-jump)))
