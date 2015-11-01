(require-binary
 '(the_silver_searcher))

(require-package
 '(ag
   projectile))

(projectile-global-mode)
(setq projectile-indexing-method 'alien)
(setq projectile-find-dir-includes-top-level t)
(setq projectile-switch-project-action 'projectile-find-file-or-dir)
(setq projectile-git-command "git ls-files -zco")

(setq projectile-globally-ignored-file-suffixes
      '("~" "#"))

(setq projectile-globally-ignored-files
      '("TAGS"
        "GPATH"
        "GRTAGS"
        "GTAGS"))

(defun projectile-find-file-or-dir (&optional arg)
  "Jump to a project's file or directory using completion.
With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (let ((file-or-dir (projectile-completing-read "Find file or directory: "
                                                 (append
                                                  (if projectile-find-dir-includes-top-level '("./"))
                                                  (projectile-current-project-dirs)
                                                  (projectile-current-project-files)))))
    (find-file (expand-file-name file-or-dir (projectile-project-root))))
  ;; TODO: Use existing hooks vs create new hook
  )

(add-hook 'ag-mode-hook
          (lambda ()
            (next-error-follow-minor-mode 1)))

(defun file-name-references ()
  (interactive)
  (projectile-ag (file-name-sans-extension (buffer-name))))

(defun projectile-new (directory)
  (interactive "D")
  (let ((projectile-file (concat directory ".projectile")))
    (write-region "" nil projectile-file t)))

(global-set-key (kbd "C-c C-.") 'file-name-references)
(define-key projectile-mode-map (kbd "<f12>") 'projectile-command-map)
(define-key projectile-mode-map (kbd "<f12> s") 'projectile-ag)
(define-key projectile-mode-map (kbd "<f12> n") 'projectile-new)

(provide 'setup-projectile)
