;;; -*- lexical-binding: t; -*-

(use-package gptel
  :ensure t
  :custom
  (gptel-model "gpt-4o")
  :bind (("C-c #" . gptel)
         ("C-c RET" . gptel-send))
  :config
  (gptel-make-openai "Mistral"
    :host "api.mistral.ai"
    :key 'gptel-api-key
    :stream t
    :models '("mistral-large-latest"
              "mistral-medium-latest"
              "mistral-small-latest"
              "mistral-tiny"
              "codestral-latest"))

  (gptel-make-openai "Anyscale"
    :host "api.endpoints.anyscale.com"
    :key 'gptel-api-key
    :stream t
    :models '("mistralai/Mistral-7B-Instruct-v0.1"
              "meta-llama/Meta-Llama-3-70B-Instruct"
              "mistralai/Mixtral-8x7B-Instruct-v0.1"
              "meta-llama/Meta-Llama-3-8B-Instruct"
              "BAAI/bge-large-en-v1.5"
              "thenlper/gte-large"
              "mlabonne/NeuralHermes-2.5-Mistral-7B"
              "mistralai/Mixtral-8x22B-Instruct-v0.1"
              "google/gemma-7b-it"
              "llava-hf/llava-v1.6-mistral-7b-hf"))

  (gptel-make-anthropic "Claude"
    :stream t
    :key 'gptel-api-key)

  (setq gptel-backend (alist-get "Claude" gptel--known-backends nil nil #'equal)
        gptel-model "claude-3-5-sonnet-20240620"))

(use-package gptel-quick
  :ensure (:host github :repo "karthink/gptel-quick")
  :init
  (with-eval-after-load 'embark
    (bind-key "/" #'gptel-quick embark-general-map)))

(provide 'init-ai)
