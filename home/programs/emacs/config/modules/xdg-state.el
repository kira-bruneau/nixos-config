;; Override user-emacs-directory to be in xdg state directory
(require 'xdg)
(defvar user-emacs-config-directory user-emacs-directory)
(setq user-emacs-directory (concat (xdg-state-home) "/emacs/"))
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln-cache" user-emacs-directory)))
(setq auto-save-list-file-prefix (concat user-emacs-directory "auto-save-list/.saves-"))
