;; Simplify all yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Push clipboard to the kill ring
(setopt save-interprogram-paste-before-kill t)

;; Don't ask to create a new file
(setopt confirm-nonexistent-file-or-buffer nil)

;; Don't prompt to revert a buffer
(setopt revert-without-query '(".*"))

;; Stop giving me annoying tramp messages
(setopt tramp-verbose 2)

;; Stop putting custom config in my init.el
(setopt custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

;; Turn off the annoying bell
(setq ring-bell-function 'ignore)

;; Don't prompt to follow git symlink
(setopt vc-follow-symlinks t)

;; Disable bidirectional reordering to improve performance of file with long lines
(setq bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right)

;; Write auxiliary files somewhere else
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Not-writing-files-to-the-current-directory.html
(setopt
 lock-file-name-transforms
 `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat user-emacs-directory "aux/\\2") t)))

(setopt
 auto-save-file-name-transforms
 `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat user-emacs-directory "aux/\\2") t)))

(setopt
 backup-directory-alist
 `(("." . ,(concat user-emacs-directory "aux"))))

;; Improve performance opening file under version control
(remove-hook 'find-file-hooks 'vc-find-file-hook)

;; Don't prompt to create a new buffer for multiple async shells
(setopt async-shell-command-buffer 'new-buffer)

;; Recursive minibuffers
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

;; Hide M-x commands which do not work in the current mode
(use-package simple
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Don't allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Infinite history without duplicates
(setq history-length t)
(setq history-delete-duplicates t)

;; Don't require final newline for specific files
(defun no-final-newline ()
  (dolist (regexp '("\\.csproj\\'"
                    "\\.vcx?proj\\'"))
    (when (string-match regexp (buffer-name))
      (setopt require-final-newline nil))))

(add-hook #'find-file-hook #'no-final-newline)

;; Use command modifier for meta on macOS
(when (eq system-type 'darwin)
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta))

;; Prevent *Warnings* buffer from taking over existing buffer
(setopt display-buffer-alist
 '(("\\`\\*Warnings\\*\\'"
    (display-buffer-in-side-window)
    (window-width . 0.25)
    (side . right))))

;; Read auth source from XDG_DATA_HOME
(require 'xdg)
(setopt auth-sources (list (expand-file-name "authinfo/authinfo.gpg" (xdg-data-home))))

;; Prefer tsc executable from PATH
(setopt tsc-dyn-get-from nil)

;; Prefer tree-sitter modes
(setopt major-mode-remap-alist
 '((c++-mode . c++-ts-mode)
   (c-mode . c-ts-mode)
   (c-or-c++-mode . c-or-c++-ts-mode)
   (cmake-mode . cmake-ts-mode)
   (css-mode . css-ts-mode)
   (java-mode . java-ts-mode)
   (ruby-mode . ruby-ts-mode)
   (sh-mode . bash-ts-mode)))
