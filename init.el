(defvar dir/setup (concat user-emacs-directory "setup/"))
(defvar dir/cache (concat user-emacs-directory "cache/"))
(defvar dir/conf (concat user-emacs-directory "conf/"))

;; Define setup files to load
(defvar setup-files
  (append
   (mapcar
    (lambda (name)
      (concat user-emacs-directory name))
    '("ui.el"
      "misc.el"))
   (directory-files dir/setup t "^[^.].*\.el$" t)))

;; Bootstrap straight.el
(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
      (bootstrap-version 2))
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
(dolist (setup setup-files)
  (condition-case err
      (load setup nil t)
    (error (message "%s" (error-message-string err)))))
