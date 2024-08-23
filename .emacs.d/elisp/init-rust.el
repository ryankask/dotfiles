;;; -*- lexical-binding: t; -*-

(defun my-rustic-mode-setup ()
  (subword-mode +1)
  (setq-local buffer-save-without-query t))

(use-package rustic
  :ensure (:host github :repo "emacs-rustic/rustic")
  :bind (:map rustic-compilation-mode-map
              ("C-o" . nil))
  :hook (rustic-mode . my-rustic-mode-setup)
  :custom
  (rustic-lsp-client my-lsp-provider)
  :config
  (if (fboundp 'sp-local-pair)
      (sp-with-modes '(rustic-mode)
        (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))))

  ;; Remove rust-mode from `auto-mode-alist' after Elpaca fetches
  ;; packages. This prevents rust-mode's autoload from overriding
  ;; rustic-mode for Rust files.
  (let ((entry '("\\.rs\\'" . rust-mode)))
    (add-hook 'elpaca-post-queue-hook
              (lambda ()
                (when (member entry auto-mode-alist)
                  (setq auto-mode-alist (remove entry auto-mode-alist)))))))

(with-eval-after-load 'eglot
  (setopt eglot-workspace-configuration
          (plist-put eglot-workspace-configuration
                     :rust-analyzer
                     '(:check (:command "clippy"))))

  (defun my-eglot-rust-analyzer-reload-workspace ()
    "Reload the workspace managed by Rust Analyzer"
    (interactive)
    (jsonrpc-request (eglot--current-server-or-lose)
                     :rust-analyzer/reloadWorkspace
                     nil)))

(defun my-append-decimal-to-integers (start end)
  "Append .0 to all integer literals in the region."
  (interactive "*r")
  (save-excursion
    (goto-char start)
    (save-restriction
      (narrow-to-region start end)
      (while (re-search-forward "\\(-?\\b[0-9]+\\b\\)\\([^.0-9]\\|\\'\\)" nil t)
        (unless (eq (char-before (match-beginning 1)) ?.)
          (replace-match "\\1.0\\2"))))))

(provide 'init-rust)
