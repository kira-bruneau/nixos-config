(use-package avy :demand
  :bind* (("C-:" . avy-goto-char)
          ("C-'" . avy-goto-char-2)
          ("M-g w" . avy-goto-word-1)
          ("M-g e" . avy-goto-word-0))

  :custom
  ;; Colemak optimized keys
  (avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o))

  :config
  (avy-setup-default))
