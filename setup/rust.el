(straight-use-package 'company-racer)
(straight-use-package 'flycheck-rust)
(straight-use-package 'racer)
(straight-use-package 'rust-mode)

(pacaur-use-packages
 '(rust-racer))

(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'rust-mode-hook 'flycheck-mode)
(add-hook 'racer-mode-hook 'eldoc-mode)
(add-hook 'racer-mode-hook 'company-mode)
(add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
