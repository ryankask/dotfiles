;;; -*- lexical-binding: t; -*-

(use-package elixir-mode
  :straight t
  :defer t)

(use-package alchemist
  :straight t
  :defer t
  :diminish "alch")

(use-package ruby-end
  :straight t
  :defer t
  :diminish ruby-end-mode
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
