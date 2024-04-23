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
    :models '("mistralai/Mixtral-8x7B-Instruct-v0.1"
              "mistralai/Mistral-7B-Instruct-v0.1"
              "meta-llama/Llama-3-8b-chat-hf"
              "meta-llama/Llama-3-70b-chat-hf"
              "codellama/CodeLlama-70b-Instruct-hf"
              "mistralai/Mixtral-8x22B-Instruct-v0.1"
              "mlabonne/NeuralHermes-2.5-Mistral-7B"
              "google/gemma-7b-it"))

  (gptel-make-anthropic "Claude"
    :stream t
    :key 'gptel-api-key))

(provide 'init-ai)
