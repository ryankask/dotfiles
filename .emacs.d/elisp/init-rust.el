(use-package rust-mode
  :ensure t
  :defer t
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :after rust-mode
  :diminish cargo-minor-mode
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package racer
  :ensure t
  :after (rust-mode)
  :diminish racer-mode
  :hook
  (rust-mode . racer-mode)
  (racer-mode . eldoc-mode))

(use-package toml-mode
  :ensure t
  :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'")

(provide 'init-rust)
