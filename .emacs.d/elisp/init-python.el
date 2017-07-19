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
    (and (re-search-backward "^def \\(test_[A-Za-z0-9_]+\\)(.*$" nil t)
         (match-string-no-properties 1))))

(defun my-pytest-test-command (test-name)
  (format "pytest %s -s -k %s"
          (car (projectile-make-relative-to-root (list (buffer-file-name))))
          test-name))

(defun my-pytest-copy-test-command-at-point ()
  (interactive)
  (when-let ((test-name (my-pytest-get-test-name-at-point)))
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc (my-pytest-test-command test-name))
        (process-send-eof proc)))
    (message "Copied \"%s\" to clipboard" test-name)))

(defvar my-pytest-tmux-target-pane nil
  "The tmux pane which will receive test commands")

(defun my-pytest-send-test-command-at-point-to-tmux (arg)
  "Send a command to run the test at point to the active tmux pane"
  (interactive "P")
  (when my-pytest-tmux-target-pane
    (when-let ((test-name (my-pytest-get-test-name-at-point)))
      (let ((process-connection-type nil))
        (start-process
         "pytest-tmux"
         nil
         "tmux" "send-keys" "-t" my-pytest-tmux-target-pane
         (my-pytest-test-command test-name)
         (if arg "" "C-m"))))))

(use-package python-environment
  :ensure t
  :init
  (setq venv-location "~/.virtualenvs"
        python-environment-directory venv-location
        python-environment-default-root-name "local"))

(use-package python
  :bind (:map python-mode-map
              ("C-c /" . my-python-debug-insert-ipdb-set-trace)
              ("C-c C-t" . my-pytest-copy-test-command-at-point)
              ("C-c C-s" . my-pytest-send-test-command-at-point-to-tmux))
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
