(use-package projectile
  :diminish "" ;; ➴
  :bind-keymap ("<f7>" . projectile-command-map)
  :bind (:map projectile-command-map
         ("n" . projectile-new)
         ("s" . projectile-ripgrep) ;; Use rg as default search method
         ("h" . my-dap-hydra)
         ("W" . dap-debug)
         ("w" . dap-debug-last)
         ("x" . projectile-run-multi-term)) ;; Use multi-term as the default terminal
  :init
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

  :config
  (setq projectile-find-dir-includes-top-level t)
  (setq projectile-configure-use-comint-mode t)
  (setq projectile-compile-use-comint-mode t)
  (setq projectile-test-use-comint-mode t)
  (setq projectile-install-use-comint-mode t)
  (setq projectile-package-use-comint-mode t)
  (setq projectile-run-use-comint-mode t)
  (setq projectile-per-project-compilation-buffer t)

  (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                    :configure "cmake %s -B %sbuild"
                                    :compile "cmake --build build"
                                    :test "cd build && ctest")
  (projectile-register-project-type 'meson '("meson.build")
                                    :configure "meson %s"
                                    :compile "ninja -C build"
                                    :test "ninja -C build test"))

(use-package counsel-projectile
  :after projectile
  :init
  (require 'counsel-projectile)

  ;; Use counsel-fzf in instead of counsel-projectile-find-file
  (add-to-list 'counsel-projectile-key-bindings '("f" . counsel-projectile-fzf))

  (counsel-projectile-mode)

  :config
  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((setfun 1 counsel-projectile-switch-project-action-fzf)))

  (defun counsel-projectile-fzf (&optional arg dwim)
    (interactive "P")
    (let ((counsel-fzf-cmd
           (if arg "fzf --filter \"%s\""
             "fd --hidden --exclude .git | fzf --filter \"%s\"")))
      (counsel-fzf
       nil
       (projectile-project-root)
       (projectile-prepend-project-name "Find file: "))))

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
      (counsel-projectile-switch-project-by-name project))))
