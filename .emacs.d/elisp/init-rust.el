;;; -*- lexical-binding: t; -*-

(defun my-rustic-mode-setup ()
  (subword-mode +1)
  (setq-local buffer-save-without-query t))

(use-package rustic
  :elpaca t
  :bind (:map rustic-compilation-mode-map
              ("C-o" . nil))
  :hook (rustic-mode . my-rustic-mode-setup)
  :custom
  (rustic-lsp-client my-lsp-provider)
  :config
  (if (fboundp 'sp-local-pair)
      (sp-with-modes '(rustic-mode)
        (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))))

(with-eval-after-load 'eglot
  (defun my-eglot-rust-analyzer-reload-workspace ()
    "Reload the workspace managed by Rust Analyzer"
    (interactive)
    (jsonrpc-request (eglot--current-server-or-lose)
                     :rust-analyzer/reloadWorkspace
                     nil)))

(provide 'init-rust)
