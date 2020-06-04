(use-package js2-mode
  :straight t
  :ensure-system-package typescript-language-server
  :mode "\\.js\\'"
  :interpreter "node"
  :init
  (with-eval-after-load 'restclient
    (add-to-list 'restclient-content-type-modes '("application/js" . js2-mode))))
