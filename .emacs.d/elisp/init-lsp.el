;;; -*- lexical-binding: t; -*-

(defconst my-lsp-provider 'eglot
  "LSP implementation to use")

;; lsp-mode

(defun my-lsp-prepare-provider--lsp-mode ()
  "Conform to the common LSP interface for `lsp-mode'"
  (defalias 'my-lsp-ensure #'lsp-deferred)
  (defun my-lsp-enabled-in-buffer ()
    (bound-and-true-p lsp-mode))
  (defalias 'my-lsp-format #'lsp-format-buffer)
  (defalias 'my-lsp-organize-imports #'my-lsp-organize-imports))

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
  :elpaca nil
  :commands (lsp lsp-deferred)
  :bind (nil
         :map lsp-mode-map
         ("C-o d" . lsp-describe-thing-at-point))
  :hook ((lsp-mode . my-lsp-mode-setup)
         (lsp-completion-mode . my-lsp-mode-setup-completion)
         (help-mode . my-lsp-help-mode-setup))
  :custom
  (lsp-completion-provider :none)
  (lsp-enable-snippet nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (my-format-lsp-function #'lsp-format-buffer)
  :init
  (my-lsp-prepare-provider--lsp-mode)
  :config
  (my-lsp-mode-update-keybindings))

;; eglot

(defun my-lsp-prepare-provider--eglot ()
  "Conform to the common LSP interface for `eglot'"
  (defalias 'my-lsp-ensure #'eglot-ensure)
  (defun my-lsp-enabled-in-buffer ()
    (bound-and-true-p eglot--managed-mode))
  (defalias 'my-lsp-format #'eglot-format)
  (defalias 'my-lsp-organize-imports #'eglot-code-action-organize-imports))

(defun my-eglot-managed-mode-hook ()
  "eglot buffer customisations"
  (dolist (type '(eglot-note eglot-warning eglot-error))
    (put type 'flymake-overlay-control nil)))

(use-package eglot
  :if (eq my-lsp-provider 'eglot)
  :hook (eglot-managed-mode . my-eglot-managed-mode-hook)
  :custom
  (eglot-sync-connect 1)
  (eglot-autoshutdown t)
  (eglot-menu-string "€")
  (eglot-events-buffer-size 0)
  (my-format-lsp-function #'eglot-format-buffer)
  :bind (("C-o C-s s" . eglot)
         :map eglot-mode-map
         ("C-c ." . eglot-code-actions)
         :prefix-map my-eglot-mode-map
         :prefix "C-o C-s"
         ("v" . eglot-events-buffer)
         ("f" . eglot-format)
         ("t" . eglot-code-actions)
         ("C-t" . eglot-code-actions)
         ("S" . eglot-reconnect)
         ("r" . eglot-rename)
         ("c" . eglot-show-workspace-configuration)
         ("q" . eglot-shutdown))
  :init
  (my-lsp-prepare-provider--eglot)
  :config
  (setf (alist-get 'styles (alist-get 'eglot completion-category-defaults))
        '(orderless)))

(use-package consult-eglot
  :if (eq my-lsp-provider 'eglot)
  :elpaca t
  :after (eglot consult)
  :bind (nil
         :map eglot-mode-map
         ("C-c /" . consult-eglot-symbols)
         :map my-eglot-mode-map
         ("C-s" . consult-eglot-symbols)))

(use-package eglot-booster
  :disabled t
  :if (eq my-lsp-provider 'eglot)
  :elpaca (eglot-booster :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :init
  (eglot-booster-mode))


(provide 'init-lsp)
