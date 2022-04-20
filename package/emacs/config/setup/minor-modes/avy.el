(use-package avy
  :straight t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :init
  ;; Colemak optimized keys
  (setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o))
  (avy-setup-default))
