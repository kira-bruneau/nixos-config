(use-package consult
  :bind* (;; Search
          ("C-s" . consult-line)
          ("C-r" . nil)

          ;; Buffer switching
          ([remap switch-to-buffer] . consult-buffer)
          ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
          ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
          ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
          ([remap project-switch-to-buffer] . consult-project-buffer)

          :map minibuffer-local-map
          ("C-r" . consult-history))

  :config
  (setq consult-narrow-key "<")
  (setq consult-async-input-throttle 0.01)
  (setq consult-async-input-debounce 0.01)
  (setq consult-async-min-input 0))
