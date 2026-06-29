(use-package nxml-mode
  :hook (nxml-mode . sgml-electric-tag-pair-mode)
  :custom
  (nxml-slash-auto-complete-flag t))
