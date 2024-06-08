(use-package ellama
  :config
  (setopt ellama-provider
          (make-llm-ollama
           :host "quartz"
           :chat-model "deepseek-coder:33b-instruct"
           :embedding-model "deepseek-coder:33b-instruct")))
