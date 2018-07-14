(use-package json-mode
  :straight (json-mode :type git :host github :repo "MetaDark/json-mode"
                       :upstream (:host github :repo "joshwnj/json-mode"))
  :init
  (with-eval-after-load 'restclient
    (add-to-list 'restclient-content-type-modes '("application/json" . json-mode))))
