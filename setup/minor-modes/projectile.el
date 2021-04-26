(use-package projectile
  :straight t
  :diminish "" ;; ➴
  :ensure-system-package (rg . ripgrep)
  :bind (:map projectile-mode-map
         ("<f9>" . projectile-command-map)
         :map projectile-command-map
         ("n" . projectile-new)
         ("s" . projectile-ripgrep) ;; Use rg as default search method
         ("h" . my-dap-hydra)
         ("W" . dap-debug)
         ("w" . dap-debug-last)
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

  ;; Source: https://github.com/bbatsov/projectile/issues/1637#issuecomment-812478103
  ;; Should be deleted when merged into projectile
  (defvar savehist-additional-variables)
  (add-hook 'savehist-mode-hook
            (lambda nil
              (add-to-list 'savehist-additional-variables 'projectile-project-command-history)))

  :config
  (setq projectile-find-dir-includes-top-level t)
  (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                    :configure "cmake %s -B %sbuild"
                                    :compile "cmake --build build"
                                    :test "cd build && ctest")
  (projectile-register-project-type 'meson '("meson.build")
                                    :configure "meson %s"
                                    :compile "ninja -C build"
                                    :test "ninja -C build test"))

(use-package counsel-projectile
  :straight t
  :after projectile
  :init
  (require 'counsel-projectile)

  ;; Use counsel-fzf in instead of counsel-projectile-find-file
  (add-to-list 'counsel-projectile-key-bindings '("f" . counsel-projectile-fzf))

  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((setfun 1 counsel-projectile-switch-project-action-fzf)))

  (defun counsel-projectile-fzf (&optional no-ignore initial-input)
    (interactive "P")
    (let* ((global-ignores
            (if no-ignore '("-I")
              (append
               (mapcar
                (lambda (exclude) (string-join `("-E" ,exclude) " "))
                (append projectile-globally-ignored-files projectile-globally-ignored-directories))
               (let ((ignore-file (string-trim (shell-command-to-string "git config core.excludesfile"))))
                 (when (not (string-empty-p ignore-file))
                   (list
                    (concat
                     "--ignore-file "
                     (expand-file-name ignore-file (getenv "HOME")))))))))
           (counsel-fzf-cmd
            (string-join `("fd -H" ,(string-join global-ignores " ")  "|" "fzf -f \"%s\"") " ")))
      (counsel-fzf initial-input nil (projectile-prepend-project-name "Find file: "))))

  (defun counsel-projectile-switch-project-action-fzf (project)
    "Jump to a file or buffer in PROJECT."
    (let ((projectile-switch-project-action #'counsel-projectile-fzf))
      (counsel-projectile-switch-project-by-name project)))

  ;; Use rg as default search method
  (setq counsel-projectile-key-bindings
        (seq-remove
         (lambda (binding)
           (and
            (stringp (car binding))
            (> (length (car binding)) 1)
            (string-prefix-p "s" (car binding))))
         counsel-projectile-key-bindings))

  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((setkey "sr" "s")))

  (setq counsel-projectile-switch-project-action
        (seq-remove
         (lambda (action)
           (and
            (listp action)
            (> (length (car action)) 1)
            (string-prefix-p "s" (car action))))
         counsel-projectile-switch-project-action))

  ;; Use multi-term as the default terminal
  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((setkey "xt" "x")
     (setfun "x" counsel-projectile-switch-project-action-run-multi-term)
     (setname "x" "invoke multi-term from project root")))

  (setq counsel-projectile-switch-project-action
        (seq-remove
         (lambda (action)
           (and
            (listp action)
            (> (length (car action)) 1)
            (string-prefix-p "x" (car action))))
         counsel-projectile-switch-project-action))

  (defun counsel-projectile-switch-project-action-run-multi-term (project)
    "Invoke `multi-term' from PROJECT's root."
    (let ((projectile-switch-project-action #'projectile-run-multi-term))
      (counsel-projectile-switch-project-by-name project)))

  (counsel-projectile-mode))
