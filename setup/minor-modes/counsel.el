;; Used to prioritize commonly used counsel-M-x commands
(use-package amx
  :straight t)

(use-package counsel
  :straight t
  :ensure-system-package
  ((fzf . fzf)
   (fd . fd)
   (rg . ripgrep))
  :bind (:map counsel-describe-map
              ("M-." . counsel-find-symbol))
  :init
  (require 'amx)
  (counsel-mode)

  :config
  (setq counsel-fzf-cmd "fd -H | fzf -f \"%s\"")
  (setq
   counsel-rg-base-command
   (if (memq system-type '(ms-dos windows-nt))
       "rg --hidden --with-filename --no-heading --line-number --path-separator / --color never %s ."
     "rg --hidden --with-filename --no-heading --line-number --color never %s"))
  (add-to-list 'ivy-re-builders-alist '(counsel-ag-function . ivy--regex))
  (add-to-list 'ivy-re-builders-alist '(counsel-fzf-function . ivy--regex))
  (add-to-list 'ivy-sort-functions-alist '(counsel-fzf-function . nil)))
