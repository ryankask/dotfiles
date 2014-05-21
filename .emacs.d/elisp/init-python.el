(add-hook 'python-mode-hook
          (lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))

(defun python-debug-insert-ipdb-set-trace ()
  "Insert ipdb trace call into buffer."
  (interactive)
  (insert "import ipdb; ipdb.set_trace()"))

(defun python-debug-insert-pudb-set-trace ()
  "Insert pudb trace call into buffer."
  (interactive)
  (insert "import pudb; pu.db"))

(define-key my-kbs-map (kbd "C-c /") 'python-debug-insert-ipdb-set-trace)
(define-key my-kbs-map (kbd "C-c p") 'python-debug-insert-pudb-set-trace)

(provide 'init-python)
