;;; -*- lexical-binding: t; -*-

(use-package go-mode
  :ensure t
  :bind (:map go-mode-map
              ("C-c C-r" . go-remove-unused-imports)
              ("s-h f" . godoc-at-point))
  :init
  (setq gofmt-show-errors nil)
  (add-hook 'go-mode-hook
            (lambda ()
              ;; Prefer goreturns to gofmt if installed
              (let ((goreturns (executable-find "goreturns")))
                (when goreturns
                  (setq gofmt-command goreturns)))

              (subword-mode 1)
              (set (make-local-variable 'company-backends) '(company-go))
              (add-hook 'before-save-hook 'gofmt-before-save))))

(use-package company-go
  :ensure t)

(provide 'init-go)
