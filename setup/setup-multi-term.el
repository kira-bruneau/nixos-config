(require-package
 '(multi-term))

(require-binary
 '(zsh))

(setq multi-term-program "/bin/zsh")
(global-set-key (kbd "C-S-z") 'multi-term-dedicated-toggle)
(setq multi-term-dedicated-select-after-open-p t)

(setq term-bind-key-alist
      '(("C-c C-c" . term-interrupt-subjob)
        ("C-c C-e" . term-send-esc)
        ("C-m" . term-send-return)
        ("C-y" . term-paste)
        ("M-f" . term-send-forward-word)
        ("M-b" . term-send-backward-word)
        ("M-o" . term-send-backspace)
        ("M-p" . term-send-up)
        ("M-n" . term-send-down)
        ("M-d" . term-send-forward-kill-word)
        ("M-DEL" . term-send-backward-kill-word)
        ("<C-delete>" . term-send-forward-kill-word)
        ("<C-backspace>" . term-send-backward-kill-word)))

(provide 'setup-multi-term)
