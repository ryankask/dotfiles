(require 'ac-nrepl)

(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)

(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'nrepl-mode))

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

;; Clojurescript
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

(provide 'init-clojure)
