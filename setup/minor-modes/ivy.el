(use-package ivy
  :straight t
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  (setq projectile-completion-system 'ivy)
  (ivy-mode))

(use-package counsel
  :straight t
  :bind (("M-x" . 'counsel-M-x)))

;; Used to sort counsel-M-x commands
(use-package amx
  :straight t)
