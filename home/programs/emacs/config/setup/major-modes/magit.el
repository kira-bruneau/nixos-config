(use-package magit
  :bind (("<f8>" . magit-file-dispatch))
  :init
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))

  :config
  (setq magit-delete-by-moving-to-trash nil)
  (setq magit-save-repository-buffers nil)
  (setq transient-enable-popup-navigation t)

  (setq
   magit-repolist-columns
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

  (setq magit-repository-directories '(("~/Dev" . 4))))

(use-package forge
  :after magit)
