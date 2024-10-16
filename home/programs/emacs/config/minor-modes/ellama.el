(use-package ellama
  :config
  (setopt ellama-provider
          (make-llm-ollama
           :host "quartz"
           :chat-model "deepseek-coder-v2:16b-lite-instruct-q8_0"
           :embedding-model "deepseek-coder-v2:16b-lite-instruct-q8_0")))
