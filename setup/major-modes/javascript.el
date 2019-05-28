(use-package js2-mode
  :straight t
  :mode "\\.js\\'"
  :interpreter "node"
  :hook (js2-mode . lsp)
  :init
  (with-eval-after-load 'restclient
    (add-to-list 'restclient-content-type-modes '("application/js" . js2-mode))))
