;; == Initialization == ;;
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq package-list

      '(;; 2048-game
        auctex
        adaptive-wrap
        buffer-move
        company
        ;; company-irony
       	ctags
        ;; diminish
        drag-stuff
        dtrt-indent
        elisp-slime-nav
        expand-region
        flx
        flx-ido
        flx-isearch
        flycheck
		;; ggtags
        ido-ubiquitous
        latex-preview-pane
        linum-off
        lua-mode
        ;; irony
        magit
        monokai-theme
        multiple-cursors
        pkgbuild-mode
        powerline
        projectile
        rust-mode
        slime
        smartparens
        smex
        ;; smooth-scrolling
        sublimity
		sql-indent
        undo-tree
        visual-regexp
        visual-regexp-steroids
        web-mode))

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

;; Project Management
(projectile-global-mode)

;; Recent files
(recentf-mode 1)

;; Smart symbol pairing
(require 'smartparens-config)
(smartparens-global-mode t)
(global-set-key (kbd "M-F") 'sp-forward-symbol)
(global-set-key (kbd "M-B") 'sp-backward-symbol)

;; Fuzzy match almost anything with ido
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode t)
(setq org-completion-use-ido t)
(setq magit-completing-read-function 'magit-ido-completing-read)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-use-virtual-buffers nil)

;; Fuzzy match commands with smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Super awesome (but intensive) flex matching for ido
(require 'flx-ido)
(flx-ido-mode 1)
;; (setq gc-cons-threshold 20000000) ;; This is speeds up flx-ido-mode by doing GC less often
(setq ido-use-faces nil)
(setq flx-ido-use-faces t)

;; Super awesome flex matching isearch
(global-set-key (kbd "C-M-s") #'flx-isearch-forward)
(global-set-key (kbd "C-M-r") #'flx-isearch-backward)

;; Auto-completion
;; (add-hook 'after-init-hook 'global-company-mode)
;; (add-to-list 'company-backends 'company-math-symbols-unicode)
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))

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

;; ;; Ace-jump
;; (global-set-key (kbd "C-c SPC") 'ace-jump-mode)
;; (global-set-key (kbd "C-c C-a") 'ace-window)
;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; Visual regexp
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
(define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
(define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s

;; Buffer move
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; C/C++
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)

;; ;; replace the `completion-at-point' and `complete-symbol' bindings in
;; ;; irony-mode's buffers by irony-mode's function
;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))
;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(setq gdb-many-windows t)

;; Web-mode
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; SQL
(eval-after-load "sql"
  '(load-library "sql-indent"))

;; Elisp
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; Lisp
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))
(require 'slime)
(slime-setup)

;; Eval and replace any lisp expresison
;; Source: https://emacs.wordpress.com/2007/01/17/eval-and-replace-anywhere/
(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
         (current-buffer)))

(global-set-key (kbd "C-c e") 'fc-eval-and-replace)

;; Org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
;;(setq org-log-done t)

;; LaTeX
;; (latex-preview-pane-enable)

;; Fix annoyances
(defalias 'yes-or-no-p 'y-or-n-p)
(setq save-interprogram-paste-before-kill t)
(setq confirm-nonexistent-file-or-buffer nil)
(global-set-key (kbd "C-c C-k") 'kill-this-buffer)

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

;; Misc convenience settings
(winner-mode t)
(delete-selection-mode t)
(setq shift-select-mode nil)
(windmove-default-keybindings)
(drag-stuff-global-mode t)

;; (visual-line-mode)
;; (global-visual-line-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Stuff I don't really use
(global-set-key (kbd "C-x C--") 'goto-last-change)
(global-set-key (kbd "C-=") 'er/expand-region)

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

;; == Graphical settings == ;;
(load-theme 'monokai t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :foundry "unknown" :slant normal :weight normal :height 98 :width normal))))
 '(vertical-border ((t (:foreground "#1f1f1b")))))

;; GUI cleanup
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Line numbering on most major modes
(global-linum-mode t)
(require 'linum-off)
(add-to-list 'linum-disabled-modes-list 'doc-view-mode)

;; Sublime-like smooth scrolling
(require 'sublimity-scroll) ;; This can be annoying on eshell, figure out how to fix
(sublimity-mode 1)

;; Powerline mode
(powerline-default-theme)

;; Diminish certain modeline modes
;; (diminish 'smartparens-mode "")
;; (diminish 'undo-tree-mode "")
;; (diminish 'elisp-slime-nav-mode "")
;; (diminish 'projectile-mode "")

(defun sudo-edit ()
  "Re-open a file with elevated privileges"
  (interactive)
  (cond
   ((not (file-writable-p buffer-file-name))
    (write-file (format "/sudo::%s" buffer-file-name)))
   (t
    (message "current buffer is already writeable"))))

;; Misc
;; (dtrt-indent-mode t)
(setq indent-tabs-mode nil)
(setq-default tab-width 4)
(setq column-number-mode t)
(show-paren-mode t)
(setq-default word-wrap t)
(adaptive-wrap-prefix-mode) ;; Indent on wrap
;;(setq doc-view-resolution 200) ; Note: this makes latex-preview-pane-mode crash randomly
