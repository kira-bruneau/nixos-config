(defvar dir/setup (concat user-emacs-directory "setup/"))
(defvar dir/cache (concat user-emacs-directory "cache/"))
(defvar dir/conf (concat user-emacs-directory "conf/"))

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

;; Bootstrap straight.el
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun pacaur-use-packages (binaries)
  ;; (message (concat
  ;;           "pacaur -S --needed "
  ;;           (mapconcat 'symbol-name binaries " ")))
  )

;; Load setup files and isolate any errors
(straight-transaction
  (straight-mark-transaction-as-init)
  (straight-use-package 'use-package)
  (setq use-package-always-defer t)

  (use-package use-package-ensure-system-package
    :straight t)

  (use-package diminish
    :straight t)

  (dolist (setup setup-files)
    (condition-case err
        (load setup nil t)
      (error (message "%s" (error-message-string err))))))
