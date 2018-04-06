;;; -*- lexical-binding: t; -*-

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

(defvar my-pytest-command "pytest"
  "The pytest command to send to tmux")

(defun my-pytest-get-test-name-at-point ()
  (save-excursion
    (and (re-search-backward "^def \\(test_[A-Za-z0-9_]+\\)(.*$" nil t)
         (match-string-no-properties 1))))

(defun my-pytest-test-command (&optional test-name)
  (format "%s %s -s%s"
          my-pytest-command
          (car (projectile-make-relative-to-root (list (buffer-file-name))))
          (if (and test-name (not (string= "" test-name)))
              (concat " -k " test-name)
            "")))

(defun my-pytest-copy-test-command-at-point ()
  (interactive)
  (when-let* ((test-name (my-pytest-get-test-name-at-point)))
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc (my-pytest-test-command test-name))
        (process-send-eof proc)))
    (message "Copied \"%s\" to clipboard" test-name)))

(defcustom my-pytest-tmux-target-pane nil
  "The tmux pane which will receive test commands."
  :type 'string
  :safe 'stringp)

(defun my-pytest-set-tmux-target-pane ()
  "Read and set the target tmux pane"
  (interactive)
  (let* ((target (string-trim (read-string "target tmux pane: " my-pytest-tmux-target-pane))))
    (if (not (equal target ""))
        (setq my-pytest-tmux-target-pane target))))


(defun my-pytest-send-test-buffer-file-command-to-tmux (arg)
  "Send a command to the active tmux pane that runs the tests in the
current buffer's file."
  (interactive "P")
  (when my-pytest-tmux-target-pane
    (let ((process-connection-type nil))
      (start-process
       "pytest-tmux"
       nil
       "tmux" "send-keys" "-t" my-pytest-tmux-target-pane
       (my-pytest-test-command)
       (if arg "" "C-m")))))

(defun my-pytest-send-test-command-at-point-to-tmux (arg)
  "Send a command to run the test at point to the active tmux pane"
  (interactive "P")
  (when my-pytest-tmux-target-pane
    (when-let* ((test-name (my-pytest-get-test-name-at-point))
                (test-command (my-pytest-test-command test-name)))
      (let ((process-connection-type nil))
        (start-process
         "pytest-tmux"
         nil
         "tmux" "send-keys" "-t" my-pytest-tmux-target-pane
         test-command
         (if arg "" "C-m"))))))

(defun my-set-flycheck-flake8rc (&optional flake8rc)
  (interactive)
  (setq flycheck-flake8rc (or flake8rc "~/.config/flake8")))

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
  :after (python)
  :bind (:map python-mode-map
              ("C-c S" . py-isort-buffer))
  :config
  (advice-add 'py-isort--call :around #'my-py-isort--call))

(use-package blacken
  :ensure t
  :after (python)
  :bind (:map python-mode-map
              ("C-o f" . blacken-buffer))
  :init
  (setq blacken-executable (python-environment-bin "black" my-default-virtualenv-path)))

(provide 'init-python)
