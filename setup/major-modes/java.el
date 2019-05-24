(use-package lsp-java
  :straight t
  :ensure-system-package (javac . openjdk)
  :hook
  (java-mode . (lambda ()
                 (require 'lsp-java)
                 (lsp))))
