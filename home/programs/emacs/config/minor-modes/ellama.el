(use-package ellama
  :custom
  (ellama-major-mode 'ellama-mode)

  :config
  (setopt ellama-provider
          (make-llm-ollama
           :host "quartz"
           :chat-model "deepseek-coder-v2:16b-lite-instruct-q8_0"
           :embedding-model "deepseek-coder-v2:16b-lite-instruct-q8_0"))

  (define-derived-mode ellama-mode org-mode "Ellama"
    "Major mode for Ellama buffers"
    (org-indent-mode 0)))
