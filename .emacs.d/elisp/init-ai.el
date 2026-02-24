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
         ("z" . gptel-abort)
         :map gptel-mode-map
         ("M-n" . gptel-end-of-response)
         ("M-p" . gptel-beginning-of-response)
         :repeat-map my-gptel-mode-repeat-map
         ("n" . gptel-end-of-response)
         ("p" . gptel-beginning-of-response))
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
    :models '(google/gemini-2.5-flash-lite
              google/gemini-3-flash-preview
              google/gemini-3.1-pro-preview
              minimax/minimax-m2.5
              moonshotai/kimi-k2.5
              deepseek/deepseek-v3.2
              z-ai/glm-5))

  (when (eq my-device-type 'work)
    (gptel-make-openai "OpenRouter-work"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (lambda () (1p-read "op://Private/OpenRouter/work-api-key"))
      :models '(openai/gpt-5.2
                openai/gpt-5.2-chat
                anthropic/claude-opus-4.6
                anthropic/claude-sonnet-4.6
                anthropic/claude-haiku-4.5
                google/gemini-2.5-pro
                google/gemini-2.5-flash
                google/gemini-3-pro-preview
                google/gemini-3-flash-preview)))

  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(gemma3n:latest gpt-oss:20b))

  (setopt gptel-backend (alist-get "Claude" gptel--known-backends nil nil #'equal)
          gptel-model 'claude-sonnet-4-6)

  ;; Presets

  (gptel-make-preset 'think
    :backend "Claude-thinking")

  (gptel-make-preset 'rust
    :system "You are an expert Rust programmer operating in emacs. Respond concisely.")

  (gptel-make-preset 'search
    :pre (lambda () (gptel-mcp-connect '("kagi")))
    :tools '("kagi_search_fetch"))

  ;; Other

  ;; from https://github.com/karthink/.emacs.d/blob/master/lisp/setup-gptel.el#L89
  (defun my-gptel-remove-headings (beg end)
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char beg)
        (while (re-search-forward org-heading-regexp end t)
          (forward-line 0)
          (delete-char (1+ (length (match-string 1))))
          (insert-and-inherit "*")
          (end-of-line)
          (skip-chars-backward " \t\r")
          (insert-and-inherit "*")))))

  (add-hook 'gptel-post-response-functions #'my-gptel-remove-headings))

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

(use-package gptel-agent
  :ensure t
  :defer t
  :config
  (gptel-agent-update))

(provide 'init-ai)
