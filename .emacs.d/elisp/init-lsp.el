;;; -*- lexical-binding: t; -*-

(defun my-lsp-mode-setup ()
  ;; Optimisations - copied from Doom Emacs
  (setq-local gcmh-high-cons-threshold (* 2 (default-value 'gcmh-high-cons-threshold))))

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-o d" . lsp-describe-thing-at-point)
              ("C-o f" . lsp-format-buffer))
  :hook (lsp-mode . my-lsp-mode-setup)
  :custom
  (lsp-enable-snippet nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil))

(use-package consult-lsp
  :straight t
  :after (lsp-mode consult))

(provide 'init-lsp)
