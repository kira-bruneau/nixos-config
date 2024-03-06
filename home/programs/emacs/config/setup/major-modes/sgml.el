(use-package sgml-mode
  :hook ((web-mode . sgml-electric-tag-pair-mode)
         (js-ts-mode . sgml-electric-tag-pair-mode)
         (tsx-ts-mode . sgml-electric-tag-pair-mode)))
