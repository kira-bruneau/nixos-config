(use-package ellama
  :custom
  (ellama-major-mode 'ellama-mode)
  (ellama-auto-scroll t)

  :config
  (setopt ellama-provider
          (make-llm-ollama
           :host "quartz"
           :chat-model "qwen2.5-coder:32b-instruct-q4_K_M"
           :embedding-model "qwen2.5-coder:32b-instruct-q4_K_M"))

  (define-derived-mode ellama-mode org-mode "Ellama"
    "Major mode for Ellama buffers"
    (org-indent-mode 0)))
