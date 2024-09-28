(use-package rg
  :bind* ("C-c s" . rg-menu)

  :custom
  (rg-show-columns t)
  (rg-default-alias-fallback "everything")

  :config
  (add-to-list 'transient-values '(rg-menu "--hidden" "--follow" "--glob=!.git"))
  (add-to-list 'transient-levels '(rg-menu (transient:rg-menu:--follow . 4)))

  (rg-define-search rg-buffer-name-project-dir
    "Search for buffer name (without extension) under the project
root directory."
    :query (file-name-sans-extension (buffer-name))
    :format literal
    :dir project))
