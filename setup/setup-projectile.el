(require-package
 '(ag
   wgrep-ag
   projectile))

(require-binary
 '(ag))

(projectile-global-mode)
(diminish 'projectile-mode) ;; ➴

(global-set-key (kbd "C-c C-.") 'file-name-references)
(define-key projectile-mode-map (kbd "<f12>") 'projectile-command-map)
(define-key projectile-mode-map (kbd "<f12> s") 'projectile-ag)
(define-key projectile-mode-map (kbd "<f12> n") 'projectile-new)

(setq projectile-indexing-method 'alien)
(setq projectile-find-dir-includes-top-level t)
(setq projectile-switch-project-action 'projectile-find-file-or-dir)

(add-hook 'ag-mode-hook
          (lambda ()
            (next-error-follow-minor-mode 1)))

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

(defun file-name-references ()
  (interactive)
  (projectile-ag (file-name-sans-extension (buffer-name))))

(defun projectile-new (directory)
  (interactive "D")
  (let ((projectile-file (concat directory ".projectile")))
    (write-region "" nil projectile-file t)))

(provide 'setup-projectile)
