;;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :ensure t
  :hook ((python-mode go-mode) . lsp-deferred)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-prefer-flymake nil))

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-header t))

(use-package lsp-ivy
  :ensure t
  :after (lsp-mode))

(use-package company-lsp
  :ensure t
  :after (lsp-mode company)
  :commands (company-lsp)
  :init
  (push 'company-lsp company-backends))

(provide 'init-lsp)
