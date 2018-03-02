(straight-use-package 'projectile)

(setq projectile-keymap-prefix (kbd "<f12>"))
(setq projectile-find-dir-includes-top-level t)

(projectile-global-mode)
(diminish 'projectile-mode) ;; ➴

(define-key projectile-command-map (kbd "n") 'projectile-new)

(defun projectile-new (directory)
  (interactive "D")
  (let ((projectile-file (concat directory ".projectile")))
    (write-region "" nil projectile-file t)))

;; Patch projectile--find-file to also include directories
(defun projectile--find-file (invalidate-cache &optional ff-variant)
  "Jump to a project's file using completion.
With INVALIDATE-CACHE invalidates the cache first.  With FF-VARIANT set to a
defun, use that instead of `find-file'.   A typical example of such a defun
would be `find-file-other-window' or `find-file-other-frame'"
  (interactive "P")
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let ((file (projectile-completing-read "Find file: "
                                          (append
                                           (if projectile-find-dir-includes-top-level (append '("./")))
                                           (projectile-current-project-files)
                                           (projectile-current-project-dirs))))
        (ff (or ff-variant #'find-file)))
    (funcall ff (expand-file-name file (projectile-project-root)))
    (run-hooks 'projectile-find-file-hook)))

;; Don't hide currently opened project when using projectile-switch-project
;; Source: https://github.com/bbatsov/projectile/issues/1016
(defun projectile-relevant-known-projects ()
  projectile-known-projects)

(defun projectile-relevant-open-projects ()
  (projectile-open-projects))
