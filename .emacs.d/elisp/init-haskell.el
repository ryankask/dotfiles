;;; -*- lexical-binding: t; -*-

(defun my-haskell-mode-setup ()
  (interactive-haskell-mode)
  (subword-mode 1))

(use-package haskell-mode
  :straight t
  :custom
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-show-overlays nil)
  :hook (haskell-mode . my-haskell-mode-setup)
  :init
  (add-to-list 'completion-ignored-extensions ".hi"))

(use-package lsp-haskell
  :if (eq my-lsp-provider 'lsp-mode)
  :straight t
  :after haskell-mode
  :hook ((haskell-mode . my-lsp-ensure)
         (haskell-liteate-mode . my-lsp-ensure)))

(provide 'init-haskell)
