;;; -*- lexical-binding: t; -*-

(defun my-go-mode-before-save ()
  (my-lsp-format)
  (my-lsp-organize-imports))

(defun my-go-mode-setup ()
  "Hook to run when go-mode is enabled"
  (subword-mode +1)
  (add-hook 'before-save-hook #'my-go-mode-before-save)
  (my-lsp-ensure))

(use-package go-mode
  :straight t
  :bind (:map go-mode-map
              ("C-c C-r" . go-remove-unused-imports)
              ("s-h f" . godoc-at-point))
  :hook ((go-mode . my-go-mode-setup)))

(provide 'init-go)
