;;; -*- lexical-binding: t; -*-

(use-package gptel
  :ensure t
  :bind (("C-c #" . gptel)
         ("C-#" . gptel)
         ("C-c RET" . gptel-send)
         ("C-c a" . gptel-add))
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
  :init
  (with-eval-after-load 'embark
    (bind-key "/" #'gptel-quick embark-general-map)))

(provide 'init-ai)
