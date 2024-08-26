(use-package embark
  :bind*
  (("M-." . embark-act)
   ("M-," . embark-dwim)
   ("C-h B" . embark-bindings)

   :map embark-file-map
   ("s" . sudo-edit-find-file)))
