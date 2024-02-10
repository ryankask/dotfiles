;;; -*- lexical-binding: t; -*-

(use-package elixir-mode
  :ensure t
  :defer t)

(use-package alchemist
  :ensure t
  :defer t)

(use-package ruby-end
  :ensure t
  :defer t
  :init
  (add-hook 'elixir-mode-hook
            (lambda ()
              (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                   "\\(?:^\\|\\s-+\\)\\(?:do\\)")
              (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers)
                   nil)
              (ruby-end-mode 1)))
  :config
  (remove-hook 'ruby-mode-hook 'ruby-end-mode)
  (remove-hook 'enh-ruby-mode-hook 'ruby-end-mode))

(provide 'init-elixir)
