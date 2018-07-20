(use-package buffer-move
  :straight t
  ;; Mirror i3 window move commands (vim-like + colemak layout)
  :bind (("C-M-S-n" . buf-move-left)
         ("C-M-S-e" . buf-move-down)
         ("C-M-S-i" . buf-move-up)
         ("C-M-S-o" . buf-move-right)))
