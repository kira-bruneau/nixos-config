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
  (add-to-list 'transient-values '(magit-log:magit-log-mode "-n256" "--graph" "--color" "--decorate" "--follow"))
  (add-to-list 'transient-levels '(magit-fetch (transient:magit-fetch:--unshallow . 4)))
  (add-to-list 'transient-levels '(magit-remote (magit-remote-unshallow . 4)))

  (defun magit-checkout-or-visit (process)
    (set-process-sentinel
     process
     (lambda (process event)
       (magit--checkout-or-visit #'magit-process-sentinel process event)))
    process)

  (advice-add 'magit-checkout :filter-return #'magit-checkout-or-visit)

  (defun magit--checkout-or-visit (fn &rest args)
    (condition-case err
        (let ((magit-process-raise-error t))
          (apply fn args))
      (magit-git-error
       (if (string-match "\\`'\\(?:.*?\\)' is already used by worktree at '\\(.*?\\)'" (cadr err))
           (magit-diff-visit-directory (match-string 1 (cadr err)))
         (signal 'magit-git-error (cdr err))))))

  (advice-add 'magit--checkout :around #'magit--checkout-or-visit))

(use-package forge)
