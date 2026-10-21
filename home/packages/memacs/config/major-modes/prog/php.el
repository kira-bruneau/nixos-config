(use-package php-ts-mode
  :mode ("\\.\\(?:php[s345]?\\|phtml\\)\\'"
         "\\.\\(?:php\\|inc\\|stub\\)\\'"
         "/\\.php_cs\\(?:\\.dist\\)?\\'")
  :interpreter "php\\(?:-?[34578]\\(?:\\.[0-9]+\\)*\\)?"
  :hook (php-ts-mode . sgml-electric-tag-pair-mode)

  :init
  (with-eval-after-load 'lsp-bridge
    (setopt lsp-bridge-php-lsp-server "phpactor")))
