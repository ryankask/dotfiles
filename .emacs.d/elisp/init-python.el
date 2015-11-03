(defun my-python-debug-insert-ipdb-set-trace ()
  "Insert ipdb trace call into buffer."
  (interactive)
  (insert "import ipdb; ipdb.set_trace()"))

(use-package python-environment
  :ensure t
  :init
  (setq venv-location "~/.virtualenvs"
        python-environment-directory venv-location
        python-environment-default-root-name "local"))

(use-package python
  :init
  (setq my-default-virtualenv-path (python-environment-root-path)
        python-shell-virtualenv-path my-default-virtualenv-path
        flycheck-python-flake8-executable (python-environment-bin "flake8" my-default-virtualenv-path))
  (put 'project-venv-name 'safe-local-variable #'stringp)
  (add-hook 'python-mode-hook
            (lambda ()
              (column-marker-1 80)
              (column-marker-2 100)))
  :config
  (bind-key "C-m" 'newline-and-indent python-mode-map)
  (bind-key "C-c /" 'my-python-debug-insert-ipdb-set-trace python-mode-map))

(defun my-python-mode-set-company-backends ()
  (set (make-local-variable 'company-backends)
       '((company-dabbrev-code
          company-jedi))))

(use-package company-jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (jedi:setup)
              (my-python-mode-set-company-backends)))
  :config
  (defun company-jedi-annotation (candidate)
    "Override annotating function"
    nil))

(provide 'init-python)
