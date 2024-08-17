(use-package consult
  :bind (("C-s" . consult-line)
         ("C-r" . nil)

         :map minibuffer-local-map
         ("C-r" . consult-history))

  :config
  (setq consult-async-input-throttle 0.01)
  (setq consult-async-input-debounce 0.01)
  (setq consult-async-min-input 0))
