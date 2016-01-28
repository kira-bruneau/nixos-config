(require-package
 '(rust-mode
   racer
   company-racer
   flycheck-rust))

(require-binary
 '(racer))

(setq racer-cmd "/home/kira/.cargo/bin/racer")
(setq racer-rust-src-path "/home/kira/dev/public/rust/src/")

(add-hook 'rust-mode-hook (lambda ()
                            (racer-mode)
                            (flycheck-mode)))

(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(provide 'language-rust)
