;;; -*- lexical-binding: t; -*-

(defun my-gptel-context-remove-all ()
  (interactive)
  (gptel-context-remove-all))

(use-package gptel
  :ensure t
  :bind (("C-#" . gptel)
         ("C-c RET" . gptel-send)
         :prefix-map my-gptel-prefix-map
         :prefix "s-t"
         ("t" . gptel)
         ("a" . gptel-add)
         ("f" . gptel-add-file)
         ("d" . gptel-context-remove-all)
         ("m" . gptel-menu)
         ("o t" . gptel-org-set-topic)
         ("o p" . gptel-org-set-properties)
         ("p" . gptel-system-prompt)
         ("r" . gptel-rewrite)
         ("z" . gptel-abort))
  :config
  (gptel-make-openai "Mistral"
    :host "api.mistral.ai"
    :key 'gptel-api-key
    :stream t
    :models '(mistral-large-latest
              mistral-medium-latest
              mistral-small-latest
              mistral-tiny
              codestral-latest))

  (gptel-make-anthropic "Claude"
    :stream t
    :key 'gptel-api-key)

  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key 'gptel-api-key
    :models '(deepseek/deepseek-r1
              deepseek/deepseek-chat
              meta-llama/llama-4-scout
              meta-llama/llama-4-maverick
              qwen/qwen-2.5-72b-instruct
              amazon/nova-pro-v1
              x-ai/grok-2-1212))

  (setopt gptel-backend (alist-get "Claude" gptel--known-backends nil nil #'equal)
          gptel-model 'claude-3-7-sonnet-20250219)

  (defun my-eglot-strip-mode-suffix-advice (mode-sym)
    (pcase mode-sym
      ('rustic-mode "Rust")
      (_ nil)))

  (advice-add #'gptel--strip-mode-suffix
              :before-until
              #'my-eglot-strip-mode-suffix-advice))

(use-package gptel-quick
  :ensure (:host github :repo "karthink/gptel-quick")
  :bind (("s-t q" . gptel-quick))
  :init
  (with-eval-after-load 'embark
    (bind-key "/" #'gptel-quick embark-general-map)))

(use-package aidermacs
  :ensure (:host github :repo "MatthewZMD/aidermacs")
  :bind (("s-a" . aidermacs-transient-menu))
  :custom
  (aidermacs-backend 'vterm)
  :config
  (aidermacs-setup-minor-mode))

(provide 'init-ai)
