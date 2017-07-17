(defun my-python-activate-virtualenv ()
  "Find and activate the project's virtualenv root directory"
  (let* ((project-root (projectile-project-root))
         (virtualenv-root-file (concat project-root ".virtualenv-root"))
         (buffer (current-buffer)))
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents virtualenv-root-file)
        (let ((virtualenv-root (string-trim (thing-at-point 'line t))))
          (if (and (not (string-empty-p virtualenv-root))
                   (file-directory-p virtualenv-root))
              (with-current-buffer buffer
                (setq-local python-shell-virtualenv-root virtualenv-root))))))))

(defun my-python-debug-insert-ipdb-set-trace ()
  "Insert ipdb trace call into buffer."
  (interactive)
  (insert "import ipdb; ipdb.set_trace()"))

(defun my-pytest-get-test-name-at-point ()
  (save-excursion
    (and (python-nav-beginning-of-defun)
         (re-search-forward "^def \\([A-Za-z0-9_]+\\)(.*$" nil t)
         (format "%s::%s" (buffer-file-name) (match-string-no-properties 1)))))

(defun my-pytest-run-test-at-path (&rest paths)
  (deferred:$
    (python-environment-run (append '("pytest" "-q" "--tb" "short" "--color" "no") paths))
    (deferred:error it
      (lambda (err)
        (let ((output (cadr err)))
          (when (string-match "===\\(?:.\\|\n\\)+\\'" output)
            (match-string-no-properties 0 output)))))))

(defun my-pytest-run-test-at-point ()
  (interactive)
  (when-let (test-path (and (python-environment-exists-p)
                            (my-pytest-get-test-name-at-point)))
    (deferred:$
      (my-pytest-run-test-at-path test-path)
      (deferred:nextc it #'message))))

(use-package python-environment
  :ensure t
  :init
  (setq venv-location "~/.virtualenvs"
        python-environment-directory venv-location
        python-environment-default-root-name "local"))

(use-package python
  :bind (:map python-mode-map
              ("C-c /" . my-python-debug-insert-ipdb-set-trace)
              ("C-c C-t" . my-pytest-run-test-at-point))
  :init
  (setq my-default-virtualenv-path (python-environment-root-path)
        python-shell-virtualenv-root my-default-virtualenv-path
        flycheck-python-flake8-executable (python-environment-bin "flake8" my-default-virtualenv-path))
  (add-hook 'python-mode-hook
            (lambda ()
              (my-python-activate-virtualenv)
              (column-marker-1 80)
              (column-marker-2 100))))

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

(defun my-py-isort--call (orig-fun &rest args)
  "Call ORIG-FUN with the current virtualenv's root at the front of EXEC-PATH"
  (let ((exec-path (append (list (expand-file-name "bin" python-shell-virtualenv-root))
                           exec-path)))
    (apply orig-fun args)))

(use-package py-isort
  :ensure t
  :config
  (advice-add 'py-isort--call :around #'my-py-isort--call))

(provide 'init-python)
