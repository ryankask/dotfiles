(add-hook 'elixir-mode-hook
          (lambda ()
            (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                 "\\(?:^\\|\\s-+\\)\\(?:do\\)")
            (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
            (ruby-end-mode +1)))

(provide 'init-elixir)
