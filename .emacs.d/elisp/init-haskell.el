(use-package haskell-mode
  :ensure t
  :pin melpa-stable
  :mode "\\.hs\\'"
  :bind (("C-c C-l" . haskell-process-load-or-reload)
         ("C-`" . haskell-interactive-bring)
         ("C-c C-t" . haskell-process-do-type)
         ("C-c C-i" . haskell-process-do-info)
         ("C-c C-c" . haskell-process-cabal-build)
         ("C-c C-k" . haskell-interactive-mode-clear)
         ("C-c c" . haskell-process-cabal)
         ("SPC" . haskell-mode-contextual-space))
  :init
  (message "loaded haskell")
  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t)

  (add-hook 'haskell-mode-hook
            (lambda ()
              (subword-mode 1)
              (turn-on-haskell-doc)
              (turn-on-haskell-indentation)))

  (eval-after-load "haskell-interactive-mode"
    '(progn
       (add-to-list 'company-dabbrev-code-modes 'haskell-interactive-mode))))

(use-package haskell-cabal-mode
  :mode "\\.cabal\\'"
  :config
  (bind-keys :map haskell-cabal-mode-map
             ("C-`" . haskell-interactive-bring)
             ("C-c C-k" . haskell-interactive-mode-clear)
             ("C-c C-c" . haskell-process-cabal-build)
             ("C-c c" . haskell-process-cabal)))

(provide 'init-haskell)
