(use-package nix-mode
  :straight t)

(use-package company-nixos-options
  :straight t
  :after company
  :init
  (add-to-list 'company-backends 'company-nixos-options))
