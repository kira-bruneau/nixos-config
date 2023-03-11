(defvar dir/aux (concat user-emacs-directory "aux/"))
(defvar dir/cache (concat user-emacs-directory "cache/"))
(defvar dir/conf (concat user-emacs-directory "conf/"))
(defvar dir/setup (concat user-emacs-directory "setup/"))

;; Define setup files to load
(defvar setup-files
  (delete-dups
   (append
    (mapcar
     (lambda (name) (concat dir/setup name))
     ;; Prioritize loading these files first
     '("ui.el"
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
    (run-hooks 'after-successful-init-hook)))
