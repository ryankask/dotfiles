;;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :disabled
  :ensure t
  :init
  (setq haskell-process-type 'stack-ghci)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (flycheck-select-checker 'haskell-stack-ghc)
              (setq-local flycheck-check-syntax-automatically '(save idle-change mode-enabled))
              (interactive-haskell-mode)
              (subword-mode 1))))

(use-package ghc
  :disabled
  :ensure t
  :commands (ghc-init ghc-debug)
  :init (add-hook 'haskell-mode-hook 'ghc-init))

(use-package company-ghc
  :disabled
  :ensure t
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq-local company-backends
                          '((company-ghc :with company-dabbrev-code) company-dabbrev-code)))))

(use-package hindent
  :disabled
  :ensure t
  :defer t
  :diminish t
  :init
  (add-hook 'haskell-mode-hook 'hindent-mode))

(use-package intero
  :disabled
  :ensure t
  :defer t
  :diminish t
  :init
  (add-hook 'haskell-mode-hook 'intero-mode))

(provide 'init-haskell)
