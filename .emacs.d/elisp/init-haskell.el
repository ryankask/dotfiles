(use-package haskell-mode
  :ensure t
  :pin melpa-stable
  :defer t
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (subword-mode 1))))

(use-package ghc
  :ensure t
  :commands (ghc-init ghc-debug)
  :init (add-hook 'haskell-mode-hook 'ghc-init))

(use-package company-ghc
  :ensure t
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-ghc :with company-dabbrev-code)
                     company-dabbrev-code)))))

(use-package hindent
  :ensure t
  :defer t
  :diminish t
  :init
  (add-hook 'haskell-mode-hook 'hindent-mode))

(use-package intero
  :disabled
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook 'intero-mode))

(provide 'init-haskell)
