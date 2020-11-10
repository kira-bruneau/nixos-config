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
   '("rg"
     "--hidden"
     "--with-filename"
     "--no-heading"
     "--line-number"
     "--color" "never"
     "--path-separator" "/"
     "%s"))

  (with-eval-after-load 'ivy
    ;; Use faster filtering & disable sorting for counsel-ag-function
    (add-to-list 'ivy-re-builders-alist '(counsel-ag-function . ivy--regex))
    (add-to-list 'ivy-sort-functions-alist '(counsel-ag-function . nil))
    (add-to-list 'ivy-sort-matches-functions-alist '(counsel-ag-function . ivy--identity-sort))

    ;; Use faster filtering & disable sorting for counsel-fzf-function
    (add-to-list 'ivy-re-builders-alist '(counsel-fzf-function . ivy--regex))
    (add-to-list 'ivy-sort-functions-alist '(counsel-fzf-function . nil))
    (add-to-list 'ivy-sort-matches-functions-alist '(counsel-fzf-function . ivy--identity-sort))

    ;; Use faster filtering & disable sorting for counsel-minibuffer-history
    (add-to-list 'ivy-re-builders-alist '(counsel-minibuffer-history . ivy--regex-plus))
    (add-to-list 'ivy-sort-functions-alist '(counsel-minibuffer-history . nil))
    (add-to-list 'ivy-sort-matches-functions-alist '(counsel-minibuffer-history . ivy--identity-sort))))
