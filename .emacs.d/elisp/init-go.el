;;; -*- lexical-binding: t; -*-

(defun my-go-mode-before-save ()
  (lsp-format-buffer)
  (lsp-organize-imports))

(defun my-go-mode-setup ()
  "Hook to run when go-mode is enabled"
  (subword-mode 1)
  (add-hook 'before-save-hook #'my-go-mode-before-save)
  (lsp-deferred))

(use-package go-mode
  :ensure t
  :bind (:map go-mode-map
              ("C-c C-r" . go-remove-unused-imports)
              ("s-h f" . godoc-at-point))
  :hook ((go-mode . my-go-mode-setup)))

(provide 'init-go)
