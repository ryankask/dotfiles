;;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :disabled
  :straight t
  :init
  (setq haskell-process-type 'stack-ghci)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (flycheck-select-checker 'haskell-stack-ghc)
              (setq-local flycheck-check-syntax-automatically '(save idle-change mode-enabled))
              (interactive-haskell-mode)
              (subword-mode +1))))

(use-package ghc
  :disabled
  :straight t
  :commands (ghc-init ghc-debug)
  :init (add-hook 'haskell-mode-hook 'ghc-init))

(use-package company-ghc
  :disabled
  :straight t
  :init
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq-local company-backends
                          '((company-ghc :with company-dabbrev-code) company-dabbrev-code)))))

(use-package hindent
  :disabled
  :straight t
  :defer t
  :diminish t
  :init
  (add-hook 'haskell-mode-hook 'hindent-mode))

(use-package intero
  :disabled
  :straight t
  :defer t
  :diminish t
  :init
  (add-hook 'haskell-mode-hook 'intero-mode))

(provide 'init-haskell)
