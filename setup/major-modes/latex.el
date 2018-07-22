;; (use-package tex
;;   :ensure auctex)

(use-package tex-mode
  :bind (:map latex-mode-map
              ("M-p" . latex-preview-pane-mode)))

(use-package latex-preview-pane
  :straight t
  :defer t)
