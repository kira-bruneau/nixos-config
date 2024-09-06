(use-package rg
  :bind* ("C-c s" . rg-menu)
  :config
  (setq rg-show-columns t)
  (setq rg-default-alias-fallback "everything")

  (add-to-list 'transient-values '(rg-menu "--hidden" "--follow" "--glob=!.git"))
  (add-to-list 'transient-levels '(rg-menu (transient:rg-menu:--follow . 4)))

  (rg-define-search rg-buffer-name-project-dir
    "Search for buffer name (without extension) under the project
root directory."
    :query (file-name-sans-extension (buffer-name))
    :format literal
    :dir project))
