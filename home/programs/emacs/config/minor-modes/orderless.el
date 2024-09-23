(use-package orderless
  :custom
  (completion-ignored-extensions nil)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (orderless-matching-styles
   '(orderless-literal
     orderless-regexp
     orderless-initialism
     orderless-prefixes)))
