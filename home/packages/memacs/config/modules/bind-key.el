(use-package bind-key
  :hook (after-init . (lambda ()
                        ;; Ensure override-global-map takes priority over other emulation mode map alists
                        (setq emulation-mode-map-alists
                              (cons
                               `((override-global-mode . ,override-global-map))
                               emulation-mode-map-alists)))))
