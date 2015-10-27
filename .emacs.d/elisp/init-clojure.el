(use-package clojure-mode
  :ensure t
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
  :ensure t
  :pin melpa-stable)

(provide 'init-clojure)
