(let ((inhibit-message t)
      (gc-cons-threshold most-positive-fixnum))
  ;; Define setup files to load
  (defvar setup-files
    (delete-dups
     (append
      (mapcar
       (lambda (file) (concat user-emacs-directory file))
       ;; Prioritize loading these files first
       '("modules/xdg-user-emacs-dirs.el"
         "modules/ui.el"
         "modules/fix-annoyances.el"
         "modules/evil.el"
         "modules/editing.el"))
      (mapcan
       (lambda (dir)
         (directory-files-recursively (concat user-emacs-directory dir) "^[^.].*\.el$"))
       '("major-modes"
         "minor-modes"
         "modules")))))

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
