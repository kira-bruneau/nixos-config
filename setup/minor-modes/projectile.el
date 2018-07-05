(use-package projectile
  :straight t
  :bind (:map projectile-command-map
              ("n" . 'projectile-new)
              ("x m" .'projectile-run-multi-term))
  :config
  (setq projectile-keymap-prefix (kbd "<f12>"))

  (projectile-global-mode)
  (diminish 'projectile-mode) ;; ➴

  (defun projectile-new (directory)
    (interactive "D")
    (let ((projectile-file (concat directory ".projectile")))
      (write-region "" nil projectile-file t)))

  (defun projectile-run-multi-term ()
    "Invoke `multi-term' in the project's root."
    (interactive)
    (let* ((buffer-name (concat "*multi-term " (projectile-project-name) "*"))
           (buffer (get-buffer buffer-name)))
      (if buffer (switch-to-buffer buffer)
        (require 'multi-term)
        (projectile-with-default-dir (projectile-project-root)
          (multi-term)
          (rename-buffer buffer-name)))))

  ;; Don't hide currently opened project when using projectile-switch-project
  ;; Source: https://github.com/bbatsov/projectile/issues/1016
  (defun projectile-relevant-known-projects ()
    projectile-known-projects)

  (defun projectile-relevant-open-projects ()
    (projectile-open-projects)))

(use-package counsel-projectile
  :straight t
  :bind (:map counsel-projectile-command-map
              ("s" . 'counsel-projectile-rg))
  :config
  (counsel-projectile-mode))
