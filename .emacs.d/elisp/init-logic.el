;;; -*- lexical-binding: t; -*-

(use-package prolog
  :defer t
  :custom
  (prolog-indent-width 2))

(use-package ediprolog
  :ensure t
  :after prolog
  :bind (nil
         :map prolog-mode-map
         ("C-c C-e" . ediprolog-dwim)))

(provide 'init-logic)
