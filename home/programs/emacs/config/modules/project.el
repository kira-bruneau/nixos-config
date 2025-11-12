(use-package project
  :commands (project-consult-ripgrep)

  :bind-keymap ("<f7>" . project-prefix-map)

  :bind (:map project-prefix-map
         ("p" . project-consult-switch-project)
         ("f" . project-consult-fd)
         ("s" . project-consult-ripgrep)
         ("v" . project-magit-dispatch)
         ("V" . magit-project-status)
         ("x" . project-async-shell-command)
         ("X" . project-shell)
         ("l" . lsp-bridge-restart-process)
         ("g" . nil))

  :custom
  (project-switch-commands
   '((project-consult-fd "Find file")
     (project-consult-ripgrep "Find regexp")
     (project-find-dir "Find directory")
     (project-dired "Dired")
     (project-magit-dispatch "Magit Dispatch")
     (magit-project-status "Magit Status")
     (project-async-shell-command "Execute")
     (project-shell "Shell")))

  (project-vc-extra-root-markers
   '("compile_commands.json"
     "package.json"))

  :config
  (defun project-consult-switch-project (&optional arg initial)
    (interactive "P")
    (if (not (null arg)) (call-interactively #'project-switch-project)
      (require 'consult)
      (project-switch-project
       (expand-file-name
        (let ((default-directory "~/Dev")
              (consult-fd-args
               (append
                (eval (car (get 'consult-fd-args 'standard-value)))
                '("--hidden"
                  "--exclude" "archive"
                  "--exclude" ".stversions"
                  "--max-depth" "5"
                  "--format" "{//}"
                  "--prune"
                  "/\\.git$"))))
          (consult--find
           "Select project: "
           (consult--fd-make-builder '("."))
           initial))
        "~/Dev"))))

  (defun project-consult-ripgrep (&optional dir initial)
    (interactive "P" (list (project-root (project-current))))
    (consult-ripgrep dir initial))

  (defun project-consult-fd (&optional dir initial)
    (interactive "P" (list (project-root (project-current))))
    (consult-fd dir initial))

  (defun project-magit-dispatch ()
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (magit-dispatch)))

  ;; Per-project compilation buffers
  ;; Adapted from: https://github.com/bbatsov/projectile/blob/0163b335a18af0f077a474d4dc6b36e22b5e3274/projectile.el#L5074-L5087
  (defun project-compilation-buffer-name (compilation-mode)
    "Meant to be used for `compilation-buffer-name-function`.
Argument COMPILATION-MODE is the name of the major mode used for the
compilation buffer."
    (concat "*" (downcase compilation-mode) "*"
            (if-let ((project (project-current))) (concat "<" (project-name project) ">") "")))

  (defun project-current-project-buffer-p ()
    "Meant to be used for `compilation-save-buffers-predicate`.
This indicates whether the current buffer is in the same project as the current
window (including returning true if neither is in a project)."
    (if-let ((project (with-current-buffer (window-buffer) (project-current))))
        (string-prefix-p (file-truename (project-root project)) (file-truename (buffer-file-name)))
      t))

  (defun project-per-project-compilation-buffer-advice (orig-fun &rest args)
    (let ((compilation-buffer-name-function #'project-compilation-buffer-name)
          (compilation-save-buffers-predicate #'project-current-project-buffer-p))
      (apply orig-fun args)))

  (advice-add 'project-compile :around #'project-per-project-compilation-buffer-advice))

(use-package projection-core-cache
  :init
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'projection--project-cache)))

(use-package projection-commands
  :bind (:map project-prefix-map
         ("z" . projection-commands-configure-project)
         ("Z" . (lambda ()
                  (interactive)
                  (let ((current-prefix-arg t))
                    (call-interactively #'projection-commands-configure-project))))

         ("c" . projection-commands-build-project)
         ("C" . (lambda ()
                  (interactive)
                  (let ((current-prefix-arg t))
                    (call-interactively #'projection-commands-build-project))))

         ("t" . projection-commands-test-project)
         ("T" . (lambda ()
                  (interactive)
                  (let ((current-prefix-arg t))
                    (call-interactively #'projection-commands-test-project))))

         ("r" . projection-commands-run-project)
         ("R" . (lambda ()
                  (interactive)
                  (let ((current-prefix-arg t))
                    (call-interactively #'projection-commands-run-project))))

         ("w" . projection-commands-install-project)
         ("W" . (lambda ()
                  (interactive)
                  (let ((current-prefix-arg t))
                    (call-interactively #'projection-commands-install-project)))))

  :config
  (advice-add 'projection-commands-configure-project :around #'project-per-project-compilation-buffer-advice)
  (advice-add 'projection-commands-build-project :around #'project-per-project-compilation-buffer-advice)
  (advice-add 'projection-commands-test-project :around #'project-per-project-compilation-buffer-advice)
  (advice-add 'projection-commands-run-project :around #'project-per-project-compilation-buffer-advice)
  (advice-add 'projection-commands-package-project :around #'project-per-project-compilation-buffer-advice)
  (advice-add 'projection-commands-install-project :around #'project-per-project-compilation-buffer-advice))

(use-package projection-multi
  :bind (:map project-prefix-map
         ("a" . projection-multi-compile)
         ("A" . project-compile))

  :init
  ;; projection-multi depends on projection-types, but doesn't require the package itself
  (require 'projection-types))

(use-package projection-multi-embark
  :bind (:map project-prefix-map
         ("a" . projection-multi-compile)
         ("A" . project-compile))
  :config (projection-multi-embark-setup-command-map))
