(autoload 'python "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-hook 'python-mode-hook
          (lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))

(defun python-debug-insert-ipdb-set-trace ()
  "Insert ipdb trace call into buffer."
  (interactive)
  (insert "import ipdb; ipdb.set_trace()"))

(define-key my-kbs-map (kbd "C-c /") 'python-debug-insert-ipdb-set-trace)

;; Highlight the 79th and 99th columns in python-mode
(add-hook 'python-mode-hook
          (lambda ()
            (interactive)
            (column-marker-1 79)
            (column-marker-2 99)))

;; Django stuff - http://garage.pimentech.net/libcommonDjango_django_emacs/
;; and http://metapundit.net/tech_blog/emacs_and_django
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(provide 'init-python)
