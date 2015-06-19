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

;; Fix annoyances
(defalias 'yes-or-no-p 'y-or-n-p) ;; Simplify all yes/no to y/n
(setq save-interprogram-paste-before-kill t) ;; Push clipboard to the kill ring
(setq confirm-nonexistent-file-or-buffer nil) ;; Don't ask to create a new file
(global-set-key (kbd "C-c C-k") 'kill-this-buffer) ;; Don't prompt to kill a buffer

;; Misc
;; (bury-successful-compilation 1)
(global-subword-mode t)
(winner-mode t)
(recentf-mode t)
(dtrt-indent-mode t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq-default indent-tabs-mode nil)
(global-visual-line-mode t)
(delete-selection-mode t)
(setq shift-select-mode nil)
(windmove-default-keybindings)
(drag-stuff-global-mode t)
(global-whitespace-cleanup-mode t)

(global-set-key (kbd "C-x C--") 'goto-last-change)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Start frames maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'setup-misc)
