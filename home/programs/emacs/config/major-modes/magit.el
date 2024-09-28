(use-package magit
  :bind* ("<f8>" . magit-file-dispatch)
  :init
  (setq magit-bind-magit-project-status nil)

  :custom
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  (magit-delete-by-moving-to-trash nil)
  (magit-save-repository-buffers nil)
  (transient-enable-popup-navigation t)
  (magit-repository-directories '(("~/Dev" . 4)))
  (magit-repolist-columns
   '(("Name" 15 magit-repolist-column-ident nil)
     ("B<U" 3 magit-repolist-column-unpulled-from-upstream
      ((:right-align t)
       (:help-echo "Upstream changes not in branch")))
     ("B>U" 3 magit-repolist-column-unpushed-to-upstream
      ((:right-align t)
       (:help-echo "Local changes not in upstream")))
     ("Flag" 3 magit-repolist-column-flag
      ((:right-align t)))
     ("Version" 35 magit-repolist-column-version nil)
     ("Path" 99 magit-repolist-column-path nil)))

  :config
  (add-to-list 'transient-values '(magit-diff:magit-status-mode "--no-ext-diff"))
  (add-to-list 'transient-values '(magit-log:magit-log-mode "-n256" "--graph" "--color" "--decorate" "--follow")))

(use-package forge)
