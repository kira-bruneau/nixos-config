(straight-use-package 'rg)

(pacaur-use-packages
 '(ripgrep))

(rg-enable-default-bindings (kbd "M-s"))
(define-key rg-global-map (kbd "b") 'rg-buffer-name-project-dir)

(setq rg-group-result t)
(setq rg-show-columns t)

(rg-define-search rg-buffer-name-project-dir
  "Search for buffer name (without extension) under the project
root directory."
  :query (file-name-sans-extension (buffer-name))
  :format literal
  :dir project)
