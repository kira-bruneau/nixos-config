(use-package ellama
  :config
  (setopt ellama-provider
          (make-llm-ollama
           :host "quartz"
           :chat-model "zephyr"
           :embedding-model "zephyr")))
