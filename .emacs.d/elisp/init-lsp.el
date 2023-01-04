;;; -*- lexical-binding: t; -*-

(defconst my-lsp-provider 'eglot
  "LSP implementation to use")

;; lsp-mode

(defun my-lsp-mode-update-keybindings ()
  (setcar (assq ?g (cdr lsp-command-map)) ?t))

(defun my-lsp-mode-setup ()
  ;; Optimisations - copied from Doom Emacs
  (setq-local gcmh-high-cons-threshold (* 2 (default-value 'gcmh-high-cons-threshold)))
  (lsp-enable-which-key-integration))

(defun my-lsp-mode-setup-completion ()
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless))
  (when (fboundp 'tempel-expand)
    (tempel-setup-capf)))

(defun my-lsp-help-mode-setup ()
  "Customize faces for the lsp-help buffer"
  (when (string= (buffer-name) "*lsp-help*")
    (face-remap-add-relative 'nobreak-space :underline nil)))

(use-package lsp-mode
  :if (eq my-lsp-provider 'lsp-mode)
  :straight t
  :commands (lsp lsp-deferred)
  :bind (nil
         :map lsp-mode-map
         ("C-o d" . lsp-describe-thing-at-point)
         ("C-o f" . lsp-format-buffer))
  :hook ((lsp-mode . my-lsp-mode-setup)
         (lsp-completion-mode . my-lsp-mode-setup-completion)
         (help-mode . my-lsp-help-mode-setup))
  :custom
  (lsp-completion-provider :none)
  (lsp-enable-snippet nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  :config
  (my-lsp-mode-update-keybindings))

;; eglot

(use-package eglot
  :if (eq my-lsp-provider 'eglot)
  :custom
  (eglot-sync-connect 1)
  (eglot-connect-timeout 10)
  (eglot-autoshutdown t)
  (eglot-menu-string "€")
  :bind (nil
         :map eglot-mode-map
         ("C-o f" . eglot-format)))

;; Common interface

(defun my-lsp-ensure ()
  "Ensure an LSP session is set up for the enabled provider"
  (pcase-exhaustive my-lsp-provider
    ('lsp-mode (lsp-deferred))
    ('eglot (eglot-ensure))))

(defun my-lsp-format ()
  (interactive)
  (pcase-exhaustive my-lsp-provider
    ('lsp-mode (lsp-format-buffer))
    ('eglot (call-interactively #'eglot-format))))

(defun my-lsp-organize-imports ()
  (interactive)
  (pcase-exhaustive my-lsp-provider
    ('lsp-mode (lsp-organize-imports))
    ('eglot (call-interactively #'eglot-code-action-organize-imports))))

(provide 'init-lsp)
