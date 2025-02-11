;;; -*- lexical-binding: t; -*-

(use-package ediprolog
  :ensure t
  :after prolog
  :bind (nil
         :map prolog-mode-map
         ("s-e" . ediprolog-dwim)))

(provide 'init-logic)
