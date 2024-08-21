(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-prefixes)))
