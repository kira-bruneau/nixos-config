(use-package json-mode
  :straight t
  :init
  (with-eval-after-load 'restclient
    (add-to-list 'restclient-content-type-modes '("application/json" . json-mode))))
