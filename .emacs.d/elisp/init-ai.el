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
    :models '("meta-llama/Llama-2-7b-chat-hf"
              "mistralai/Mistral-7B-Instruct-v0.1"
              "meta-llama/Llama-2-13b-chat-hf"
              "meta-llama/Llama-2-70b-chat-hf"
              "mistralai/Mixtral-8x7B-Instruct-v0.1"
              "BAAI/bge-large-en-v1.5"
              "thenlper/gte-large"
              "mlabonne/NeuralHermes-2.5-Mistral-7B"
              "Meta-Llama/Llama-Guard-7b"
              "codellama/CodeLlama-70b-Instruct-hf"
              "Open-Orca/Mistral-7B-OpenOrca"
              "codellama/CodeLlama-34b-Instruct-hf"
              "HuggingFaceH4/zephyr-7b-beta"
              "google/gemma-7b-it")))

(provide 'init-ai)
