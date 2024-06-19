(let ((gc-cons-threshold most-positive-fixnum))
  ;; Override user-emacs-directory to be in xdg state directory
  (require 'xdg)
  (defvar user-emacs-config-directory user-emacs-directory)
  (setq user-emacs-directory (concat (xdg-state-home) "/emacs/"))
  (when (boundp 'native-comp-eln-load-path)
    (startup-redirect-eln-cache (expand-file-name "eln-cache" user-emacs-directory)))
  (require 'transient)
  (setq transient-levels (transient--read-file-contents (concat user-emacs-config-directory "transient/levels.el")))
  (setq transient-values (transient--read-file-contents (concat user-emacs-config-directory "transient/values.el")))
  (setq auto-save-list-file-prefix (concat user-emacs-directory "auto-save-list/.saves-"))
  (defvar dir/setup (concat user-emacs-config-directory "setup/"))

  ;; Define setup files to load
  (defvar setup-files
    (delete-dups
     (append
      (mapcar
       (lambda (name) (concat dir/setup name))
       ;; Prioritize loading these files first
       '("ui.el"
         "evil.el"
         "editing.el"
         "fix-annoyances.el"))
      (directory-files-recursively dir/setup "^[^.].*\.el$"))))

  ;; Load setup files and isolate errors
  (setq use-package-always-defer t)
  (defvar after-successful-init-hook nil)
  (let ((error? nil))
    (dolist (setup setup-files)
      (condition-case err
          (load setup nil t)
        (error
         (setq error? t)
         (message "%s" (error-message-string err)))))
    (unless error?
      (run-hooks 'after-successful-init-hook))))
