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
  :hook ((gptel-mode . gptel-highlight-mode))
  :custom
  (gptel-track-media t)
  (gptel-highlight-methods '(fringe))
  :config
  ;; Models

  (gptel-make-anthropic "Claude"
    :stream t
    :key 'gptel-api-key)

  (gptel-make-anthropic "Claude-thinking"
    :stream t
    :key 'gptel-api-key
    :request-params '(:thinking (:type "enabled" :budget_tokens 2048)
                                :max_tokens 4096))

  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key 'gptel-api-key
    :models '(google/gemini-2.5-pro
              google/gemini-2.5-flash
              google/gemini-2.5-flash-lite
              moonshotai/kimi-k2))

  (when (eq my-device-type 'work)
    (gptel-make-openai "OpenRouter-work"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (lambda () (1p-read "op://Private/OpenRouter/work-api-key"))
      :models '(openai/gpt-5
                anthropic/claude-sonnet-4.5
                anthropic/claude-haiku-4.5
                google/gemini-2.5-pro
                google/gemini-2.5-flash)))

  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(gemma3n:latest gpt-oss:20b))

  (setopt gptel-backend (alist-get "Claude" gptel--known-backends nil nil #'equal)
          gptel-model 'claude-sonnet-4-5-20250929)

  ;; Presets

  (gptel-make-preset 'think
    :backend "Claude-thinking")

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
