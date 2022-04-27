;; Simplify all yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Push clipboard to the kill ring
(setq save-interprogram-paste-before-kill t)

;; Don't ask to create a new file
(setq confirm-nonexistent-file-or-buffer nil)

;; Don't prompt to revert a buffer
(setq revert-without-query '(".*"))

;; Stop giving me annoying tramp messages
(setq tramp-verbose 2)

;; Stop putting custom config in my init.el
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;; Turn off the annoying bell
(setq ring-bell-function 'ignore)

;; Don't prompt to follow git symlink
(setq vc-follow-symlinks t)

;; Disable bidirectional reordering to improve performance of file with long lines
(setq bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right)

;; Write auxiliary files somewhere else
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Not-writing-files-to-the-current-directory.html
(setq
 lock-file-name-transforms
 `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat dir/aux "\\2") t)))

(setq
 auto-save-file-name-transforms
 `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat dir/aux "\\2") t)))

(setq
 backup-directory-alist
 `(("." . ,dir/aux)))

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

;; Don't require final newline for specific files
(defun no-final-newline ()
  (dolist (regexp '("\\.csproj\\'"
                    "\\.vcx?proj\\'"))
    (when (string-match regexp (buffer-name))
      (setq require-final-newline nil))))

(add-hook #'find-file-hook #'no-final-newline)

;; Use command modifier for meta on macOS
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'nil)
  (setq mac-command-modifier 'meta))

;; Trade memory for higher performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Prevent *Warnings* buffer from taking over existing buffer
(setq
 display-buffer-alist
 '(("\\`\\*Warnings\\*\\'"
    (display-buffer-in-side-window)
    (window-width . 0.25)
    (side . right))))

;; Read auth source from XDG_DATA_HOME
(setq auth-sources (list (expand-file-name "authinfo/authinfo.gpg" (xdg-data-home))))
