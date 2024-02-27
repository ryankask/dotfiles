;;; -*- lexical-binding: t; -*-

(use-package gptel
  :ensure t
  :custom
  (gptel-model "gpt-4")
  :bind (("C-c #" . gptel)
         ("C-c RET" . gptel-send))
  :config
  (gptel-make-openai "Mistral"
    :host "api.mistral.ai"
    :key 'gptel-api-key
    :stream t
    :models '("mistral-large-latest" "mistral-medium" "mistral-small" "mistral-tiny"))

  (gptel-make-openai "Anyscale"
    :host "api.endpoints.anyscale.com"
    :key 'gptel-api-key
    :stream t
    :models '("meta-llama/Llama-2-7b-chat-hf" "meta-llama/Llama-2-13b-chat-hf"
              "meta-llama/Llama-2-70b-chat-hf" "codellama/CodeLlama-34b-Instruct-hf"
              "mistralai/Mistral-7B-Instruct-v0.1" "mistralai/Mixtral-8x7B-Instruct-v0.1"
              "HuggingFaceH4/zephyr-7b-beta" "Open-Orca/Mistral-7B-OpenOrca")))

(provide 'init-ai)
