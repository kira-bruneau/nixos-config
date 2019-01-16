(use-package projectile
  :straight t
  :diminish "" ;; ➴
  :ensure-system-package (rg . ripgrep)
  :bind (:map projectile-mode-map
         ("<f12>" . projectile-command-map)
         :map projectile-command-map
         ("n" . projectile-new)
         ("s" . projectile-ripgrep) ;; Use rg as default search tool
         ("x" . projectile-run-multi-term)) ;; Use multi-term as the default terminal
  :init
  (projectile-mode)

  (defun projectile-new (directory)
    (interactive "D")
    (let ((projectile-file (concat directory ".projectile")))
      (write-region "" nil projectile-file t)))

  (defun projectile-run-multi-term ()
    "Invoke `multi-term' in the project's root."
    (interactive)
    (let* ((buffer-name (concat "*multi-term " (projectile-project-name) "*"))
           (buffer (get-buffer buffer-name)))
      (if buffer (switch-to-buffer-other-window buffer)
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
  :after projectile
  :ensure-system-package (rg . ripgrep)
  :init
  (require 'counsel-projectile)

  ;; Use rg as default search tool
  (setq counsel-projectile-key-bindings
        (->> counsel-projectile-key-bindings
          (seq-remove
           (lambda (binding)
             (and
              (stringp (car binding))
              (> (length (car binding)) 1)
              (string-prefix-p "s" (car binding)))))))

  (setq counsel-projectile-switch-project-action
        (->> counsel-projectile-switch-project-action
          (seq-map
           (lambda (action)
             (if (and (listp action) (string= (car action) "sr"))
                 (cons "s" (cdr action))
               action)))
          (seq-remove
           (lambda (action)
             (and
              (listp action)
              (> (length (car action)) 1)
              (string-prefix-p "s" (car action)))))))

  ;; Use multi-term as the default terminal
  (setq counsel-projectile-switch-project-action
        (->> counsel-projectile-switch-project-action
          (seq-map
           (lambda (action)
             (if (and (listp action) (string= (car action) "xt"))
                 (list "x" #'counsel-projectile-switch-project-action-run-multi-term "invoke multi-term from project root")
               action)))
          (seq-remove
           (lambda (action)
             (and
              (listp action)
              (> (length (car action)) 1)
              (string-prefix-p "x" (car action)))))))

  (defun counsel-projectile-switch-project-action-run-multi-term (project)
    "Invoke `multi-term' from PROJECT's root."
    (let ((projectile-switch-project-action 'projectile-run-multi-term))
      (counsel-projectile-switch-project-by-name project))))

  (counsel-projectile-mode)
