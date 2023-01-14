;; Used to prioritize commonly used counsel-M-x commands
;; (use-package amx
;;   :config
;;   (setq amx-history-length 10000))

(use-package counsel
  :bind (:map counsel-describe-map
              ("M-." . counsel-find-symbol))
  :init
  ;; (require 'amx)
  (counsel-mode)

  :config
  (setq counsel-fzf-cmd "fd --hidden --exclude .git | fzf --filter \"%s\"")

  (setq
   counsel-rg-base-command
   '("rg"
     "--hidden"
     "--glob" "!.git"
     "--max-columns" "240"
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
