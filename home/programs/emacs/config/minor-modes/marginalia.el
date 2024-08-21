(use-package marginalia
  :demand
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  :config
  (marginalia-mode))
