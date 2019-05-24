(use-package ccls
  :straight t
  :ensure-system-package ccls
  :hook
  (((c-mode c++-mode objc-mode) . (lambda ()
                                    (require 'ccls)
                                    (lsp)))))
