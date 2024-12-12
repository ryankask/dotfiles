;;; -*- lexical-binding: t; -*-

(use-package gptel
  :ensure t
  :bind (("C-#" . gptel)
         ("C-c RET" . gptel-send)
         :prefix-map my-gptel-prefix-map
         :prefix "s-t"
         ("t" . gptel)
         ("a" . gptel-add)
         ("f" . gptel-add-file)
         ("m" . gptel-menu)
         ("r" . gptel-rewrite)
         ("p" . gptel-system-prompt)
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

  (setopt gptel-backend (alist-get "Claude" gptel--known-backends nil nil #'equal)
          gptel-model 'claude-3-5-sonnet-20241022)

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

(provide 'init-ai)
