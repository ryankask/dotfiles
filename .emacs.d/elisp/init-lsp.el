;;; -*- lexical-binding: t; -*-

(defconst my-lsp-provider 'eglot
  "LSP implementation to use")

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
         ("C-o C-n" . eglot-code-actions)
         :prefix-map my-eglot-mode-map
         :prefix "C-o C-s"
         ("v" . eglot-events-buffer)
         ("f" . eglot-format)
         ("t" . eglot-code-actions)
         ("S" . eglot-reconnect)
         ("r" . eglot-rename)
         ("l" . eglot-show-call-hierarchy)
         ("y" . eglot-show-type-hierarchy)
         ("c" . eglot-show-workspace-configuration)
         ("q" . eglot-shutdown))
  :init
  (my-lsp-prepare-provider--eglot)
  :config
  (setf (alist-get 'styles (alist-get 'eglot completion-category-defaults))
        '(orderless)))

(use-package consult-eglot
  :if (eq my-lsp-provider 'eglot)
  :ensure t
  :after (eglot consult)
  :bind (nil
         :map eglot-mode-map
         ("C-o C-k" . consult-eglot-symbols)
         :map my-eglot-mode-map
         ("k" . consult-eglot-symbols)))

(use-package eglot-booster
  :disabled t
  :if (eq my-lsp-provider 'eglot)
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :init
  (eglot-booster-mode))


(provide 'init-lsp)
