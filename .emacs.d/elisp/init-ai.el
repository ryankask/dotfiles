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
  ;; Models

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
    :models '(deepseek/deepseek-r1-0528
              google/gemini-2.5-pro
              google/gemini-2.5-flash
              google/gemini-2.5-flash-preview-05-20:thinking
              google/gemini-2.5-flash-lite-preview-06-17
              meta-llama/llama-4-scout
              meta-llama/llama-4-maverick))

  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(gemma3n:latest))

  (setopt gptel-backend (alist-get "Claude" gptel--known-backends nil nil #'equal)
          gptel-model 'claude-opus-4-20250514)

  ;; Presets

  (gptel-make-preset 'rust
                     :system "You are an expert Rust programmer operating in emacs. Respond concisely.")

  (gptel-make-preset 'search
                     :pre (lambda () (gptel-mcp-connect '("kagi")))
                     :tools '("kagi_search_fetch"))

  ;; Other

  (defun my-eglot-strip-mode-suffix-advice (mode-sym)
    (pcase mode-sym
      ('rustic-mode "Rust")
      (_ nil)))

  (advice-add #'gptel--strip-mode-suffix
              :before-until
              #'my-eglot-strip-mode-suffix-advice))

(use-package gptel-quick
  :ensure (:host github :repo "karthink/gptel-quick")
  :bind (nil
         ("s-t q" . gptel-quick))
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

(use-package mcp
  :ensure t
  :bind (nil
         ("s-t h" . mcp-hub))
  :custom
  (mcp-hub-servers
   `(("git"
      :command "uvx"
      :args ("mcp-server-git"))
     ("time"
      :command "uvx"
      :args ("mcp-server-time" "--local-timezone=Europe/London"))
     ("kagi"
      :command "uvx"
      :args ("kagimcp")
      :env (:KAGI_API_KEY ,(1p-read "op://Private/Kagi/api-key"))))))

(use-package gptel-integrations
  :after (gptel mcp))

(provide 'init-ai)
