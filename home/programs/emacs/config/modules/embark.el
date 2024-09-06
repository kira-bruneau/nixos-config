(use-package embark
  :bind*
  (("C-." . embark-act)
   ("C-," . embark-dwim)
   ("C-h B" . embark-bindings)

   :map embark-file-map
   ("s" . sudo-edit-find-file)))
