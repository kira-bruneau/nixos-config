(use-package whitespace
  :hook (diff-mode . whitespace-mode)
  :config
  (setq whitespace-style
        '(face
          tabs spaces trailing lines-tail space-before-tab
          newline indentation empty space-after-tab tab-mark)))
