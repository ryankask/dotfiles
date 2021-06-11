;;; -*- lexical-binding: t; -*-

(defun my-rustic-mode-setup ()
  (setq-local buffer-save-without-query t))

(use-package rustic
  :ensure t
  :defer t
  :hook (rustic-mode . my-rustic-mode-setup))

(provide 'init-rust)
