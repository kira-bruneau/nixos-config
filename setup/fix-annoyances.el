;; Simplify all yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Push clipboard to the kill ring
(setq save-interprogram-paste-before-kill t)

;; Don't ask to create a new file
(setq confirm-nonexistent-file-or-buffer nil)

;; Fast keybinding for killing the current buffer
(global-set-key (kbd "C-S-q") 'kill-current-buffer)

;; Key binding for killing both a buffer and a window
(global-set-key (kbd "C-S-M-q") 'kill-buffer-and-window)

;; Don't prompt to revert a buffer
(setq revert-without-query '(".*"))

;; Stop giving me annoying tramp messages
(setq tramp-verbose 2)

;; Stop putting custom config in my init.el
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Turn off the annoying bell
(setq ring-bell-function 'ignore)

;; Disable warning about running lisp code in theme
(setq custom-safe-themes t)

;; Don't prompt to follow git symlink
(setq vc-follow-symlinks t)

;; Disable bidirectional reordering to improve performance of file with long lines
(setq-default bidi-display-reordering nil)

;; Store all backup files in a single directory
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backup/"))))

;; Improve performance opening file under version control
(remove-hook 'find-file-hooks 'vc-find-file-hook)

;; Don't prompt to create a new buffer for multiple async shells
(setq async-shell-command-buffer 'new-buffer)

;; Recursive minibuffers (support counsel-yank-pop within minibuffer)
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

;; Support ANSI colours in compilation output
;; Source: https://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
(require 'ansi-color)
(defun colourize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook 'colourize-compilation-buffer)

;; Infinite history without duplicates
(setq history-length t)
(setq history-delete-duplicates t)

;; Use interactive shell for access to bash aliases and functions
(setq shell-command-switch "-ic")

;; Don't require final newline for specific files
(defun no-final-newline ()
  (dolist (regexp '("\\.csproj\\'"
                    "\\.vcx?proj\\'"))
    (when (string-match regexp (buffer-name))
      (setq require-final-newline nil))))

(add-hook #'find-file-hook #'no-final-newline)
