(use-package orderless
  :custom
  (completion-ignored-extensions nil)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (orderless-matching-styles
   '(orderless-literal
     orderless-regexp
     orderless-initialism
     orderless-prefixes)))
