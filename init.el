;; Model this: https://github.com/1ambda/emacs-linux

;; == Initialization == ;;
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-list
      '(adaptive-wrap
        auctex
        buffer-move
        company
        company-quickhelp
        company-tern
        ;; diminish
        drag-stuff
        dtrt-indent
        elisp-slime-nav
        expand-region
        flx
        flx-ido
        flx-isearch
        flycheck
        ggtags
        ido-ubiquitous
        ido-vertical-mode
        js2-mode
        latex-preview-pane
        lua-mode
        magit
        monokai-theme
        multiple-cursors
        php-mode
        pkgbuild-mode
        powerline
        projectile
        rust-mode
        slime
        smartparens
        smex
        tern
        sql-indent
        undo-tree
        visual-regexp
        visual-regexp-steroids
        web-mode
        whitespace-cleanup-mode))

(package-initialize)

;; Refresh the archive
(unless package-archive-contents
  (package-refresh-contents))

;; Install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; == Semantic settings == ;;

;; Tree based undo / redo instead of stack based
(global-undo-tree-mode t)
(global-set-key (kbd "M-?") 'undo-tree-visualize)

;; Project management
(projectile-global-mode)
(setq projectile-indexing-method 'alien)

;; Recent files
(recentf-mode 1)

;; Smart symbol pairing
(require 'smartparens-config)
(smartparens-global-mode t)
(global-set-key (kbd "M-F") 'sp-forward-symbol)
(global-set-key (kbd "M-B") 'sp-backward-symbol)

;; Integrated git
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-c C-v") 'magit-status)

;; Fuzzy find everywhere with ido
(ido-mode 1)
(ido-vertical-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode t)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-use-virtual-buffers t)

;; Smarter flex matching for ido
(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-use-faces nil)
(setq flx-ido-use-faces t)

;; Fuzzy isearch using flx
(global-set-key (kbd "C-M-s") #'flx-isearch-forward)
(global-set-key (kbd "C-M-r") #'flx-isearch-backward)

;; Ido for M-x commands
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ggtags
;; (ggtags-update-on-save t) ;; Will be available in version 0.8.10
(global-set-key (kbd "M-,") 'pop-tag-mark)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'php-mode)
              (ggtags-mode 1))))

;; Auto-completion
(company-quickhelp-mode 1)
(setq company-idle-delay 0.3)
(setq company-minimum-prefix-length 1)
(add-hook 'after-init-hook 'global-company-mode)

;; Snippets
;; (yas-global-mode 1)

;; Syntax checking
;;(global-flycheck-mode t)
;;(setq flycheck-check-syntax-automatically '(mode-enabled save))

;; Spell checking
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (hook '(c-mode-common-hook emacs-lisp-mode-hook lisp-mode-hook))
  (flyspell-prog-mode))

(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)

;; Multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-^") 'mc/mark-all-like-this)
(global-set-key (kbd "C-'") 'mc-hide-unmatched-lines-mode)

;; Visual regexp
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
;; (define-key esc-map (kbd "M-r") 'vr/isearch-backward) ;; C-M-r
;; (define-key esc-map (kbd "M-s") 'vr/isearch-forward) ;; C-M-s

;; Buffer move
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

;; C/C++
(setq gdb-many-windows t)

;; js2-mode
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;; tern
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-tern))

;; sql
(eval-after-load "sql"
  '(load-library "sql-indent"))

;; elisp
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; lisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))
(require 'slime)
(slime-setup)

;; Org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
;;(setq org-log-done t)

;; LaTeX
;; (latex-preview-pane-enable)

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

;; == Graphical settings == ;;
(load-theme 'monokai t)

;; Theme company-mode
;; (require 'color)

;; (let ((bg (face-attribute 'default :background)))
;;   (custom-set-faces
;;    `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
;;    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;;    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
;;    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
;;    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; GUI cleanup
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Powerline mode
(powerline-default-theme)

;; Diminish certain modeline modes
;; (diminish 'smartparens-mode "")
;; (diminish 'undo-tree-mode "")
;; (diminish 'elisp-slime-nav-mode "")
;; (diminish 'projectile-mode "")

;; Misc
(setq-default tab-width 4)
(setq column-number-mode t)
(show-paren-mode t)
(setq-default word-wrap t)
(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)
;;(setq doc-view-resolution 200) ; Note: this makes latex-preview-pane-mode crash randomly

;; Extra ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Eval and replace any lisp expresison
;; Source: https://emacs.wordpress.com/2007/01/17/eval-and-replace-anywhere/
(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
         (current-buffer)))

(global-set-key (kbd "C-c e") 'fc-eval-and-replace)

(defun sudo-edit ()
  "Re-open a file with elevated privileges"
  (interactive)
  (cond
   ((not (file-writable-p buffer-file-name))
    (write-file (format "/sudo::%s" buffer-file-name)))
   (t
    (message "current buffer is already writeable"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :foundry "unknown" :slant normal :weight normal :height 98 :width normal))))
 '(vertical-border ((t (:foreground "#1f1f1b")))))
