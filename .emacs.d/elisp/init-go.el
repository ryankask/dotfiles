;;; -*- lexical-binding: t; -*-

(defun my-go-mode-before-save ()
  (eglot-format-buffer)
  (eglot-code-action-organize-imports))

(defun my-go-mode-setup ()
  "Hook to run when go-ts-mode is enabled"
  (subword-mode 1)
  (add-hook 'before-save-hook #'my-go-mode-before-save nil t)
  (eglot-ensure))

(use-package go-ts-mode
  :hook (go-ts-mode . my-go-mode-setup))

(provide 'init-go)
