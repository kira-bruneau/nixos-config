(defvar dir/setup (concat user-emacs-directory "setup/"))
(defvar dir/cache (concat user-emacs-directory "cache/"))
(defvar dir/conf (concat user-emacs-directory "conf/"))

(defvar setup-files
  (append
   (mapcar
    (lambda (name)
      (concat user-emacs-directory name))
    '("ui.el"
      "misc.el"))
   (directory-files dir/setup t "^[^.].*\.el$" t)))

(defun require-package (packages)
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package))))

(defun require-binary (binaries)
  ;; (message (concat
  ;;           "sudo pacman -S --needed "
  ;;           (mapconcat 'symbol-name binaries " ")))
  )

;; Load setup files
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Refresh the archive
(unless package-archive-contents
  (package-refresh-contents))

(require-package '(diminish))

(dolist (setup setup-files)
  (condition-case err
      (load setup nil t)
    (error (message "%s" (error-message-string err)))))
