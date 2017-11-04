(use-package haskell-mode
  :ensure t
  :pin melpa-stable
  :defer t
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (subword-mode 1)
              (set (make-local-variable 'company-backends)
                   (append '((company-capf company-dabbrev-code))
                           company-backends)))))

(use-package intero
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package hindent
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook 'hindent-mode))

(provide 'init-haskell)
