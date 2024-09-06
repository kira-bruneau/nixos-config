(require 'xdg)

(defvar user-emacs-config-directory user-emacs-directory)
(defvar user-emacs-state-directory (concat (xdg-state-home) "/emacs/"))
(defvar user-emacs-cache-directory (concat (xdg-cache-home) "/emacs/"))

(setq
 auto-save-list-file-prefix
 (cond ((eq system-type 'ms-dos)
        ;; MS-DOS cannot have initial dot, and allows only 8.3 names
        (concat user-emacs-state-directory "auto-save.list/_s"))
       (t
        (concat user-emacs-state-directory "auto-save-list/.saves-"))))

(startup-redirect-eln-cache (expand-file-name "eln-cache" user-emacs-cache-directory))
(setq image-dired-dir (expand-file-name "image-dired/" user-emacs-cache-directory))

(setq user-emacs-directory user-emacs-state-directory)
