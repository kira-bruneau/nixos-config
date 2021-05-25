(use-package prettier-js
  :straight t
  :hook ((js2-mode . prettier-js-mode)
         (json-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))
