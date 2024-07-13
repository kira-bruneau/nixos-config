(use-package project
  :bind-keymap ("<f7>" . project-prefix-map)
  :bind (:map project-prefix-map
         ("s" . project-counsel-rg)
         ("S" . rg-project)
         ("v" . project-magit-dispatch)
         ("V" . magit-project-status)
         ("x" . project-async-shell-command)
         ("X" . project-shell)
         ("g" . nil))

  :config
  (setq magit-bind-magit-project-status nil)
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-counsel-rg "Find regexp")
          (rg-project "Ripgrep")
          (project-find-dir "Find directory")
          (project-dired "Dired")
          (project-magit-dispatch "Magit Dispatch")
          (magit-project-status "Magit Status")
          (project-async-shell-command "Execute")
          (project-shell "Shell")))

  (defun project-counsel-rg ()
    (interactive)
    (let ((root (project-root (project-current t))))
      (counsel-rg nil root nil (format "Find regexp in %s: " root))))

  (defun project-magit-dispatch ()
    (interactive)
    (setq default-directory (project-root (project-current t)))
    (magit-dispatch))

  ;; Only save project buffers before compilation
  ;; Source: https://andreyor.st/posts/2022-07-16-project-el-enhancements/
  (define-advice project-compile (:around (fn) save-project-buffers)
    "Only ask to save project-related buffers."
    (let* ((project-buffers (project-buffers (project-current)))
           (compilation-save-buffers-predicate
            (lambda () (memq (current-buffer) project-buffers))))
      (funcall fn)))

  (define-advice recompile (:around (fn &optional edit-command) save-project-buffers)
    "Only ask to save project-related buffers if inside a project."
    (if (project-current)
        (let* ((project-buffers (project-buffers (project-current)))
               (compilation-save-buffers-predicate
                (lambda () (memq (current-buffer) project-buffers))))
          (funcall fn edit-command))
      (funcall fn edit-command))))

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
  (require 'projection))

(use-package projection-core-cache
  :init
  (require 'projection-core-cache)
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'projection--project-cache)))

(use-package projection-multi
  :bind (:map project-prefix-map
         ("a" . projection-multi-compile)
         ("A" . project-compile))

  :config
  (require 'projection))
