(require-package
 '(dtrt-indent
   expand-region
   hide-lines
   lua-mode
   markdown-mode
   paradox
   pkgbuild-mode
   whitespace-cleanup-mode))

;; Default configuration
(global-auto-revert-mode t)
(setq-default indent-tabs-mode nil)
(set-default 'truncate-lines t)
(delete-selection-mode t)
(setq shift-select-mode nil)

;; Minor modes
(global-whitespace-cleanup-mode t)
(diminish 'whitespace-cleanup-mode) ;; ⌫
(global-subword-mode t)
(diminish 'subword-mode) ;; "w̲"
(winner-mode t)
(dtrt-indent-mode t)
(windmove-default-keybindings)

(global-set-key (kbd "C-c C-r") 'revert-buffer)
(global-set-key (kbd "C-c v") 'visual-line-mode)
(diminish 'visual-line-mode "↩")

(global-set-key (kbd "C-c C-/") 'hide-lines)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Fix annoyances
(defalias 'yes-or-no-p 'y-or-n-p) ;; Simplify all yes/no to y/n
(setq save-interprogram-paste-before-kill t) ;; Push clipboard to the kill ring
(setq confirm-nonexistent-file-or-buffer nil) ;; Don't ask to create a new file
(global-set-key (kbd "C-c C-k") 'kill-this-buffer) ;; Don't prompt to kill a buffer
(setq revert-without-query '(".*")) ;; Don't prompt to revert a buffer
(setq tramp-verbose 2) ;; Stop giving me annoying tramp messages
(setq ad-redefinition-action 'accept) ;; Stop warning `ido-completing-read' got redefined
(setq custom-file (concat user-emacs-directory "custom.el")) ;; Stop putting custom config in my init.el
(setq ring-bell-function 'ignore) ;; Turn off the annoying bell

;; scroll one line at a time (less "jumpy" than defaults)
;; Source: http://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 1) ;; keyboard scroll one line at a time

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

(provide 'setup-misc)
