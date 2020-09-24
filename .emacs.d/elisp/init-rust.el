;;; -*- lexical-binding: t; -*-

(defun my-rust-mode-setup ()
  (lsp-deferred))

(use-package rust-mode
  :ensure t
  :defer t
  :hook (rust-mode . my-rust-mode-setup))

(use-package cargo
  :ensure t
  :after rust-mode
  :diminish cargo-minor-mode
  :hook (rust-mode . cargo-minor-mode))

(use-package toml-mode
  :ensure t
  :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'")

(provide 'init-rust)
