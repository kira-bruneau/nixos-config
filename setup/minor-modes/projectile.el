(use-package projectile
  :straight t
  :bind (:map projectile-command-map
              ("n" . projectile-new)
              ("x m" . projectile-run-multi-term))
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
  (setq counsel-projectile-switch-project-action
        '(1
          ("o" counsel-projectile-switch-project-action "jump to a project buffer or file")
          ("f" counsel-projectile-switch-project-action-find-file "jump to a project file")
          ("d" counsel-projectile-switch-project-action-find-dir "jump to a project directory")
          ("b" counsel-projectile-switch-project-action-switch-to-buffer "jump to a project buffer")
          ("m" counsel-projectile-switch-project-action-find-file-manually "find file manually from project root")
          ("S" counsel-projectile-switch-project-action-save-all-buffers "save all project buffers")
          ("k" counsel-projectile-switch-project-action-kill-buffers "kill all project buffers")
          ("K" counsel-projectile-switch-project-action-remove-known-project "remove project from known projects")
          ("c" counsel-projectile-switch-project-action-compile "run project compilation command")
          ("C" counsel-projectile-switch-project-action-configure "run project configure command")
          ("E" counsel-projectile-switch-project-action-edit-dir-locals "edit project dir-locals")
          ("v" counsel-projectile-switch-project-action-vc "open project in vc-dir / magit / monky")
          ("s" counsel-projectile-switch-project-action-rg "search project with rg")
          ("xs" counsel-projectile-switch-project-action-run-shell "invoke shell from project root")
          ("xe" counsel-projectile-switch-project-action-run-eshell "invoke eshell from project root")
          ("xt" counsel-projectile-switch-project-action-run-term "invoke term from project root")
          ("O" counsel-projectile-switch-project-action-org-capture "org-capture into project")))
  (counsel-projectile-mode))
