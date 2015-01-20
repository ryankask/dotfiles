(defun python-debug-insert-ipdb-set-trace ()
  "Insert ipdb trace call into buffer."
  (interactive)
  (insert "import ipdb; ipdb.set_trace()"))

(defun python-debug-insert-pudb-set-trace ()
  "Insert pudb trace call into buffer."
  (interactive)
  (insert "import pudb; pu.db"))

(defvar my-python-enable-anaconda-mode nil)

(add-hook 'python-mode-hook
          (lambda ()
            (interactive)
            (column-marker-1 80)
            (column-marker-2 100)

            (define-key python-mode-map (kbd "C-m") 'newline-and-indent)
            (define-key python-mode-map (kbd "C-c /") 'python-debug-insert-ipdb-set-trace)
            (define-key python-mode-map (kbd "C-c p") 'python-debug-insert-pudb-set-trace)

            (my-python-mode-set-company-backends)

            (when my-python-enable-anaconda-mode
              (anaconda-mode)
              (define-key anaconda-mode-map (kbd "s-.") 'anaconda-mode-goto-definitions)
              (define-key anaconda-mode-map (kbd "s-*") 'anaconda-mode-nav-pop-marker)
              (define-key anaconda-mode-map (kbd "s-?") 'anaconda-mode-view-doc))))

(defun my-python-mode-set-company-backends ()
  (let ((my-python-company-backends '(company-dabbrev-code)))
    (set (make-local-variable 'company-backends)
         (if my-python-enable-anaconda-mode
             (append my-python-company-backends
                     '(company-capf
                       company-anaconda))
           my-python-company-backends))))

(provide 'init-python)
