(defun python-debug-insert-ipdb-set-trace ()
  "Insert ipdb trace call into buffer."
  (interactive)
  (insert "import ipdb; ipdb.set_trace()"))

(defun python-debug-insert-pudb-set-trace ()
  "Insert pudb trace call into buffer."
  (interactive)
  (insert "import pudb; pu.db"))

(add-hook 'python-mode-hook
          (lambda ()
            (interactive)
            (column-marker-1 80)
            (column-marker-2 100)
            (anaconda-mode)
            (my-python-mode-set-company-backends)
            (define-key python-mode-map (kbd "C-m") 'newline-and-indent)
            (define-key python-mode-map (kbd "C-c /") 'python-debug-insert-ipdb-set-trace)
            (define-key python-mode-map (kbd "C-c p") 'python-debug-insert-pudb-set-trace)
            (define-key anaconda-mode-map (kbd "s-.") 'anaconda-mode-goto-definitions)
            (define-key anaconda-mode-map (kbd "s-*") 'anaconda-mode-nav-pop-marker)
            (define-key anaconda-mode-map (kbd "s-?") 'anaconda-mode-view-doc)))

(defun my-python-mode-set-company-backends ()
  (set (make-local-variable 'company-backends)
       '(company-dabbrev-code
         company-anaconda)))

(provide 'init-python)
