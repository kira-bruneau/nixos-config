(require-package
 '(drag-stuff
   dtrt-indent
   expand-region
   hide-lines
   whitespace-cleanup-mode))

;; Auto-indent yanked lines (try auto-indent instead)
;; Source: http://www.emacswiki.org/emacs/AutoIndentation
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;; Remove consecutive duplicates
;; Source: http://www.emacswiki.org/emacs/DuplicateLines
(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

;; Centralized backups:
;; Source http://www.emacswiki.org/emacs/BackupDirectory
;; (setq
;;  backup-by-copying t      ; don't clobber symlinks
;;  backup-directory-alist
;;  '(("." . "~/.saves"))    ; don't litter my fs tree
;;  delete-old-versions t
;;  kept-new-versions 6
;;  kept-old-versions 2
;;  version-control t)       ; use versioned backups

;; Minor modes
(global-whitespace-cleanup-mode t)
(winner-mode t)
(dtrt-indent-mode t)
(windmove-default-keybindings)
(drag-stuff-global-mode t)

(global-set-key (kbd "C-c C-/") 'hide-lines)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-x C--") 'goto-last-change)

;; Fix annoyances
(defalias 'yes-or-no-p 'y-or-n-p) ;; Simplify all yes/no to y/n
(setq save-interprogram-paste-before-kill t) ;; Push clipboard to the kill ring
(setq confirm-nonexistent-file-or-buffer nil) ;; Don't ask to create a new file
(global-set-key (kbd "C-c C-k") 'kill-this-buffer) ;; Don't prompt to kill a buffer

;; Default configuration
(global-auto-revert-mode t)
(global-subword-mode t)
(setq-default indent-tabs-mode nil)
(global-visual-line-mode t)
(delete-selection-mode t)
(setq shift-select-mode nil)

;; File used for customize
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; scroll one line at a time (less "jumpy" than defaults)
;; Source: http://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 1) ;; keyboard scroll one line at a time

(provide 'setup-misc)
