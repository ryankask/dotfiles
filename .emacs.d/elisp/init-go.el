;;; -*- lexical-binding: t; -*-

(defun my-go-mode-setup ()
  "Hook to run when go-mode is enabled"
  (subword-mode 1)
  (lsp-deferred))

(use-package go-mode
  :ensure t
  :bind (:map go-mode-map
              ("C-c C-r" . go-remove-unused-imports)
              ("s-h f" . godoc-at-point))
  :hook ((go-mode . my-go-mode-setup)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports)))


(provide 'init-go)
