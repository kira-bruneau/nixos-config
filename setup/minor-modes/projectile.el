(straight-use-package 'projectile)

(projectile-global-mode)
(diminish 'projectile-mode) ;; ➴

(define-key projectile-mode-map (kbd "<f12>") 'projectile-command-map)
(define-key projectile-mode-map (kbd "<f12> n") 'projectile-new)

(setq projectile-indexing-method 'alien)
(setq projectile-find-dir-includes-top-level t)
(setq projectile-switch-project-action 'projectile-find-file-or-dir)

;; Don't hide currently opened project when using projectile-switch-project
;; Source: https://github.com/bbatsov/projectile/issues/1016
(defun projectile-relevant-known-projects () projectile-known-projects)
(defun projectile-relevant-open-projects () (projectile-open-projects))

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

(defun projectile-new (directory)
  (interactive "D")
  (let ((projectile-file (concat directory ".projectile")))
    (write-region "" nil projectile-file t)))
