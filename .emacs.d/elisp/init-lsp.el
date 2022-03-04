;;; -*- lexical-binding: t; -*-

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
  :straight t
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
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
  (lsp-signature-render-documentation nil))

(use-package consult-lsp
  :straight t
  :after (lsp-mode consult))

(provide 'init-lsp)
