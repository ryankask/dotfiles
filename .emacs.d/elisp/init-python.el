(defun python-debug-insert-ipdb-set-trace ()
  "Insert ipdb trace call into buffer."
  (interactive)
  (insert "import ipdb; ipdb.set_trace()"))

(add-hook 'python-mode-hook
          (lambda ()
            (interactive)
            (column-marker-1 80)
            (column-marker-2 100)
            (define-key python-mode-map (kbd "C-m") 'newline-and-indent)
            (define-key python-mode-map (kbd "C-c /") 'python-debug-insert-ipdb-set-trace)
            (my-python-mode-set-company-backends)))

(defun my-python-mode-set-company-backends ()
  (set (make-local-variable 'company-backends)
       '(company-dabbrev-code)))

(provide 'init-python)
