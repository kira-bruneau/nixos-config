(use-package json-ts-mode
  :mode "\\.json\\'"
  :init
  (with-eval-after-load 'restclient
    (add-to-list 'restclient-content-type-modes '("application/json" . json-ts-mode))))
