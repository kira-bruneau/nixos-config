(use-package ellama
  :config
  (setopt ellama-provider
          (make-llm-ollama
           :host "quartz"
           :chat-model "deepseek-coder-v2"
           :embedding-model "deepseek-coder-v2")))
