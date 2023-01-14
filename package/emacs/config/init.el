(defvar dir/aux (concat user-emacs-directory "aux/"))
(defvar dir/cache (concat user-emacs-directory "cache/"))
(defvar dir/conf (concat user-emacs-directory "conf/"))
(defvar dir/setup (concat user-emacs-directory "setup/"))

;; Define setup files to load
;;
;; TODO: Load files before recursing through directories so we don't
;; have to be explict with load order
(defvar setup-files
  (delete-dups
   (append
    (mapcar
     (lambda (name)
       (expand-file-name (concat dir/setup name)))
     '("ui.el"
       "editing.el"
       "fix-annoyances.el"))
    (directory-files-recursively dir/setup "^[^.].*\.el$"))))

;; Load setup files and isolate any errors
(setq use-package-always-defer t)
(dolist (setup setup-files)
  (condition-case err
      (load setup nil t)
    (error (message "%s" (error-message-string err)))))
