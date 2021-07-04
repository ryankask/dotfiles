;;; -*- lexical-binding: t; -*-

(use-package clojure-mode
  :straight t
  :pin melpa-stable
  :config
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)))

(use-package cider
  :straight t
  :pin melpa-stable)

(provide 'init-clojure)
