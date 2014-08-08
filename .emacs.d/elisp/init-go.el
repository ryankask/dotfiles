(setq gofmt-show-errors nil)

(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
            (local-set-key (kbd "C-c C-g") 'go-goto-imports)
            (local-set-key (kbd "C-c C-f") 'gofmt)
            (local-set-key (kbd "C-c C-k") 'godoc)
            (set (make-local-variable 'company-backends) '(company-go))
            (add-hook 'before-save-hook 'gofmt-before-save)))

(provide 'init-go)
