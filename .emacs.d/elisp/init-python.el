(require 'f)

(setq venv-location "~/.virtualenvs"
      my-default-virtualenv-path (f-join venv-location "local")
      my-default-virtualenv-bin (f-join my-default-virtualenv-path "bin")
      python-shell-virtualenv-path my-default-virtualenv-path
      jedi:environment-root my-default-virtualenv-path
      flycheck-python-flake8-executable (f-join my-default-virtualenv-bin "flake8"))

;; If `project-venv-name` is a string, mark it as safe
(put 'project-venv-name 'safe-local-variable #'stringp)

(defun python-debug-insert-ipdb-set-trace ()
  "Insert ipdb trace call into buffer."
  (interactive)
  (insert "import ipdb; ipdb.set_trace()"))

(add-hook 'python-mode-hook
          (lambda ()
            (column-marker-1 80)
            (column-marker-2 100)
            (define-key python-mode-map (kbd "C-m") 'newline-and-indent)
            (define-key python-mode-map (kbd "C-c /") 'python-debug-insert-ipdb-set-trace)
            (hack-local-variables)
            (when (boundp 'project-venv-name)
              (venv-workon project-venv-name))
            (jedi:setup)
            (my-python-mode-set-company-backends)))

(defun my-python-mode-set-company-backends ()
  (set (make-local-variable 'company-backends)
       '((company-dabbrev-code
          company-jedi))))

(with-eval-after-load "company-jedi"
  (defun company-jedi-annotation (candidate)
    "Override annotating function"
    nil))

(provide 'init-python)
