(use-package rust-mode
  :straight t
  :defer t)

(use-package flycheck-rust
  :straight t
  :after rust-mode
  :hook ((flycheck-mode . flycheck-rust-setup)
         (rust-mode . flycheck-mode)))

(use-package racer
  :straight t
  :ensure-system-package (racer . rust-racer)
  :after rust-mode
  :hook ((rust-mode . racer-mode)
         (racer-mode . company-mode)
         (racer-mode . eldoc-mode)))
