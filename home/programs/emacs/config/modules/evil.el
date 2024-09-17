(use-package evil
  :demand
  :init
  (setq evil-want-keybinding nil)

  (defvar evil-key-rotation
    '(;; Remap jkl commands to their equivalent colemak position
      ("j" . "n")
      ("J" . "N")
      ("gj" . "gn")
      ("gJ" . "gN")
      ("zj" . "zn")
      ("zJ" . "zN")
      ("M-j" . "M-n")
      ("M-J" . "M-N")
      ("C-j" . "C-n")
      ("C-S-j" . "C-S-n")
      ("k" . "e")
      ("K" . "E")
      ("gk" . "ge")
      ("gK" . "gE")
      ("zk" . "ze")
      ("zK" . "zE")
      ("M-k" . "M-e")
      ("M-K" . "M-E")
      ("C-k" . "C-e")
      ("C-S-k" . "C-S-e")
      ("l" . "i")
      ("L" . "I")
      ("gl" . "gi")
      ("gL" . "gI")
      ("zl" . "zi")
      ("zL" . "zI")
      ("M-l" . "M-i")
      ("M-L" . "M-I")
      ;; C-i is usually used for "TAB" bindings and shouldn't be remapped
      ;; ("C-l" . "C-i")
      ;; ("C-S-l" . "C-S-i")
      ;; Remap nei commands to their equivalent qwerty position (in colemak)
      ("n" . "k")
      ("N" . "K")
      ("gn" . "gk")
      ("gN" . "gK")
      ("zn" . "zk")
      ("zN" . "zK")
      ("M-n" . "M-k")
      ("M-N" . "M-K")
      ("C-n" . "C-k")
      ("C-S-n" . "C-S-k")
      ("e" . "f")
      ("E" . "F")
      ("ge" . "gf")
      ("gE" . "gF")
      ("ze" . "zf")
      ("zE" . "zF")
      ("M-e" . "M-f")
      ("M-E" . "M-F")
      ("C-e" . "C-f")
      ("C-S-e" . "C-S-f")
      ("i" . "u")
      ("I" . "U")
      ("gi" . "gu")
      ("gI" . "gU")
      ("zi" . "zu")
      ("zI" . "zU")
      ("M-i" . "M-u")
      ("M-I" . "M-U")
      ;; C-u is the universal argument prefix key and shouldn't be remapped
      ;; ("C-i" . "C-u")
      ;; ("C-S-i" . "C-S-u")
      ;; Remap fu commands to their equivalent qwerty position (in colemak)
      ("f" . "t")
      ("F" . "T")
      ("gf" . "gt")
      ("gF" . "gT")
      ("zf" . "zt")
      ("zF" . "zT")
      ("M-f" . "M-t")
      ("M-F" . "M-T")
      ("C-f" . "C-t")
      ("C-S-f" . "C-S-t")
      ("u" . "l")
      ("U" . "L")
      ("gu" . "gl")
      ("gU" . "gL")
      ("zu" . "zl")
      ("zU" . "zL")
      ("M-u" . "M-l")
      ("M-U" . "M-L")
      ;; Evil shouldn't have any commands mapped to C-u
      ;; ("C-u" . "C-l")
      ;; ("C-S-u" . "C-S-l")
      ;; Remap t to j (not qwerty equivalent, but the remaining key in the rotation)
      ("t" . "j")
      ("T" . "J")
      ("gt" . "gj")
      ("gT" . "gJ")
      ("zt" . "zj")
      ("zT" . "zJ")
      ("M-t" . "M-j")
      ("M-T" . "M-J")
      ("C-t" . "C-j")
      ("C-S-t" . "C-S-j")))

  (defvar evil-key-rotation--maps
    '(evil-emacs-state-map
      evil-ex-completion-map
      evil-ex-search-keymap
      evil-inner-text-objects-map
      evil-insert-state-map
      evil-motion-state-map
      evil-normal-state-map
      evil-operator-shortcut-map
      evil-operator-state-map
      evil-outer-text-objects-map
      evil-read-key-map
      evil-replace-state-map
      evil-visual-state-map
      evil-window-map))

  (defvar evil-key-rotation--force nil)

  (defun evil-key-rotation--rotate-key (key)
    (if-let ((new-key (alist-get key evil-key-rotation nil nil
                                 (lambda (new-key key)
                                   (equal (kbd new-key) key)))))
        (kbd new-key)
      key))

  (defadvice define-key (around evil-key-rotation activate)
    (when
        (or
         evil-key-rotation--force
         (cl-some
          (lambda (map)
            (eq
             (ad-get-arg 0)
             (if (symbolp map)
                 (when (boundp map) (symbol-value map))
               map)))
          evil-key-rotation--maps))
      (ad-set-arg 1 (evil-key-rotation--rotate-key (ad-get-arg 1))))
    ad-do-it)

  :config
  (evil-set-undo-system 'undo-tree)

  (setq evil-goto-definition-functions
        '(evil-goto-definition-xref
          evil-goto-definition-semantic
          evil-goto-definition-imenu
          evil-goto-definition-search))

  (defadvice evil-define-key* (around evil-key-rotation activate)
    (let ((evil-key-rotation--force t))
      ad-do-it))

  (defadvice evil-define-minor-mode-key (around evil-key-rotation activate)
    (let ((evil-key-rotation--force t))
      ad-do-it))

  (defadvice evil-make-overriding-map (before evil-key-rotation activate)
    (add-to-list 'evil-key-rotation--maps (ad-get-arg 0)))

  (defadvice evil-make-intercept-map (before evil-key-rotation activate)
    (add-to-list 'evil-key-rotation--maps (ad-get-arg 0)))

  (evil-mode))

(use-package evil-collection
  :demand
  :config
  ;; Default to normal mode in shell-mode like in compilation-mode
  ;; shell-mode derives from comint-mode which defaults to insert mode
  (evil-set-initial-state 'shell-mode 'normal)

  ;; evil-collection-forge-setup automatically sets up bindings for
  ;; forge, and this prevents errors remapping over default keys
  (setq forge-add-default-bindings nil)

  (with-eval-after-load 'magit-files
    (evil-collection-define-key nil 'magit-blob-mode-map
      "p" nil ;; p (originally bound to magit-blob-previous, can use <f8>-p instead)
      "j" nil)) ;; n (originally bound to magit-blob-next, can use <f8>-n instead)

  (with-eval-after-load 'rg-result
    (evil-collection-define-key 'normal 'rg-mode-map
      "f" 'rg-rerun-change-literal ;; t (originally bound to rg-rerun-change-files)
      "p" 'rg-rerun-change-files)) ;; p

  (with-eval-after-load 'rg-menu
    (transient-suffix-put 'rg-menu 'rg-rerun-change-files--transient :key "p"))

  (with-eval-after-load 'comint
    (evil-collection-define-key 'normal 'comint-mode-map
      "C-j" #'next-error-no-select
      "C-k" #'previous-error-no-select
      "gj" #'next-error-no-select
      "gk" #'previous-error-no-select))

  (evil-collection-init))

(use-package evil-mc
  :demand
  :config
  ;; Add extra known commands from multiple-cursors:
  ;; https://github.com/magnars/multiple-cursors.el/blob/c870c18462461df19382ecd2f9374c8b902cd804/multiple-cursors-core.el#L494-L569
  ;; https://github.com/gabesoft/evil-mc/issues/24
  (setq
   evil-mc-custom-known-commands
   '(;; (quoted-insert
     ;;  (:default . evil-mc-execute-default-call-with-last-input))
     (previous-line
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (next-line
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (newline
      (:default . evil-mc-execute-default-call-with-count))
     (newline-and-indent
      (:default . evil-mc-execute-default-call-with-count))
     (open-line
      (:default . evil-mc-execute-default-call-with-count))
     (delete-blank-lines
      (:default . evil-mc-execute-default-call))
     (transpose-lines
      (:default . evil-mc-execute-default-call-with-count))
     (transpose-paragraphs
      (:default . evil-mc-execute-default-call-with-count))
     ;; (transpose-regions
     ;;  (:default . evil-mc-execute-call-with-region-or-pos))
     (join-line
      (:default . evil-mc-execute-default-call))
     (right-char
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (right-word
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (forward-char
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (forward-word
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (left-char
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (left-word
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (backward-char
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (backward-word
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (forward-paragraph
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (backward-paragraph
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (forward-list
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (backward-list
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (yank
      (:default . evil-mc-execute-default-call-with-count))
     (yank-pop
      (:default . evil-mc-execute-default-call-with-count))
     (append-next-kill
      (:default . evil-mc-execute-default-call))
     (kill-word
      (:default . evil-mc-execute-default-call-with-count))
     (kill-line
      (:default . evil-mc-execute-default-call-with-count))
     (kill-whole-line
      (:default . evil-mc-execute-default-call-with-count))
     (backward-kill-word
      (:default . evil-mc-execute-default-call-with-count))
     (cperl-electric-backspace
      (:default . evil-mc-execute-default-call-with-count))
     (just-one-space
      (:default . evil-mc-execute-default-call-with-count))
     ;; (zap-to-char
     ;;  (:default . evil-mc-execute-default-call-with-last-input))
     (end-of-line
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (set-mark-command
      (:default . evil-mc-execute-default-force-normal-state)
      (visual . evil-mc-execute-visual-char))
     ;; (cua-set-mark
     ;;  (:default . evil-mc-execute-default-force-normal-state)
     ;;  (visual . evil-mc-execute-visual-char))
     ;; (cua-delete-region
     ;;  (:default . evil-mc-execute-default-call))
     (move-end-of-line
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (beginning-of-line
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (move-beginning-of-line
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (kill-ring-save
      (:default . evil-mc-execute-default-evil-yank))
     (back-to-indentation
      (:default . evil-mc-execute-default-call)
      (visual . evil-mc-execute-visual-call))
     (subword-forward
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (subword-backward
      (:default . evil-mc-execute-default-call-with-count)
      (visual . evil-mc-execute-visual-call-with-count))
     (subword-mark
      (:default . evil-mc-execute-default-call-with-count))
     (subword-kill
      (:default . evil-mc-execute-default-call-with-count))
     (subword-backward-kill
      (:default . evil-mc-execute-default-call-with-count))
     (subword-transpose
      (:default . evil-mc-execute-default-call-with-count))
     (subword-capitalize
      (:default . evil-mc-execute-default-call-with-count))
     (subword-upcase
      (:default . evil-mc-execute-default-call-with-count))
     (subword-downcase
      (:default . evil-mc-execute-default-call-with-count))))

  (global-evil-mc-mode))

(use-package evil-textobj-tree-sitter
  :init
  (with-eval-after-load 'evil
    (evil-define-key nil evil-outer-text-objects-map
      "e" (evil-textobj-tree-sitter-get-textobj "function.outer")
      "c" (evil-textobj-tree-sitter-get-textobj "class.outer")
      "b" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

    (evil-define-key nil evil-inner-text-objects-map
      "e" (evil-textobj-tree-sitter-get-textobj "function.inner")
      "c" (evil-textobj-tree-sitter-get-textobj "class.inner")
      "b" (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner"))))

  (with-eval-after-load 'evil-collection-unimpaired
    (evil-collection-define-key 'normal 'evil-collection-unimpaired-mode-map
      "]f"
      (lambda ()
        (interactive)
        (evil-textobj-tree-sitter-goto-textobj "function.outer"))
      "[f"
      (lambda ()
        (interactive)
        (evil-textobj-tree-sitter-goto-textobj "function.outer" t))
      "]F"
      (lambda ()
        (interactive)
        (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t))
      "[F"
      (lambda ()
        (interactive)
        (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))

      "]c"
      (lambda ()
        (interactive)
        (evil-textobj-tree-sitter-goto-textobj "class.outer"))
      "[c"
      (lambda ()
        (interactive)
        (evil-textobj-tree-sitter-goto-textobj "class.outer" t))
      "]C"
      (lambda ()
        (interactive)
        (evil-textobj-tree-sitter-goto-textobj "class.outer" nil t))
      "[C"
      (lambda ()
        (interactive)
        (evil-textobj-tree-sitter-goto-textobj "class.outer" t t)))))
