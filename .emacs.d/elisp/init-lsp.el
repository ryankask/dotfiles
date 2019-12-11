;;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map              ;
              ("C-c ." . lsp-find-definition)
              ("C-o f" . lsp-format-buffer))
  :custom
  (lsp-prefer-flymake nil))

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map              ;
              ("C-o d" . lsp-ui-doc-glance))
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-header t)
  (lsp-ui-sideline-enable nil))

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
