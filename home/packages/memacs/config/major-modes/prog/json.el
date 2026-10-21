(use-package json-ts-mode
  :mode "\\(\\.\\(json\\|mcmeta\\)\\|/\\(flake.lock\\|composer.lock\\)\\)\\'"
  :init
  (with-eval-after-load 'restclient
    (add-to-list 'restclient-content-type-modes '("application/json" . json-ts-mode))))
