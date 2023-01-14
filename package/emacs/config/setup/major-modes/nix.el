(use-package nix-mode
  :init
  (with-eval-after-load 'aphelia
    (add-to-list 'apheleia-formatters '((nixpkgs-fmt "nixpkgs-fmt")))
    (add-to-list 'apheleia-mode-alist '((nix-mode nixpkgs-fmt)))))
