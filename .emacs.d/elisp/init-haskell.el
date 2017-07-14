(use-package haskell-mode
  :ensure t
  :pin melpa-stable
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (subword-mode 1)
              (set (make-local-variable 'company-backends)
                   (append '((company-capf company-dabbrev-code))
                           company-backends)))))

(provide 'init-haskell)
