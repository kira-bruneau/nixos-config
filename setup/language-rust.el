(require-package
 '(rust-mode
   racer
   company-racer
   flycheck-rust))

(require-binary
 '(racer))

(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'rust-mode-hook 'flycheck-mode)
(add-hook 'racer-mode-hook 'eldoc-mode)
(add-hook 'racer-mode-hook 'company-mode)
(add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

(provide 'language-rust)
