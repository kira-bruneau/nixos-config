(straight-use-package 'avy)

(setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o))

(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)

(avy-setup-default)
