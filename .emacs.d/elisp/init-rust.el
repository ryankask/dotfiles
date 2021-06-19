;;; -*- lexical-binding: t; -*-

(defun my-rustic-mode-setup ()
  (setq-local buffer-save-without-query t))

(use-package rustic
  :ensure t
  :bind (:map rustic-compilation-mode-map
              ("C-o" . nil))
  :hook (rustic-mode . my-rustic-mode-setup))

(provide 'init-rust)
