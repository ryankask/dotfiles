;;; -*- lexical-binding: t; -*-

(defun my-rustic-mode-setup ()
  (subword-mode +1)
  (setq-local buffer-save-without-query t))

(use-package rustic
  :straight t
  :bind (:map rustic-compilation-mode-map
              ("C-o" . nil))
  :hook (rustic-mode . my-rustic-mode-setup)
  :config
  (if (fboundp 'sp-local-pair)
      (sp-with-modes '(rustic-mode)
        (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))))

(provide 'init-rust)
