(use-package ellama
  :custom
  (ellama-major-mode 'ellama-mode)
  (ellama-auto-scroll t)

  :config
  (setopt ellama-provider
          (make-llm-ollama
           :host "quartz"
           :chat-model "qwen3-coder:30b"
           :embedding-model "qwen3-coder:30b"))

  (define-derived-mode ellama-mode org-mode "Ellama"
    "Major mode for Ellama buffers"
    (org-indent-mode 0)))
