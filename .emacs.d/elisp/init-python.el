;;; -*- lexical-binding: t; -*-

(defun my-python-debug-insert-ipdb-set-trace ()
  "Insert ipdb trace call into buffer."
  (interactive)
  (insert "import ipdb; ipdb.set_trace()"))

(defcustom my-pytest-run-directory-file-marker nil
  "A file to use as a maker to determine which directory to run the tests from"
  :type 'string
  :safe 'stringp)

(defun my-pytest-locate-custom-project-root ()
  "If `my-pytest-run-directory-file-marker' is set, use it to
find the root of a project that uses pytest. Return nil if a root
isn't found."
  (and my-pytest-run-directory-file-marker
       (locate-dominating-file default-directory
                               my-pytest-run-directory-file-marker)))

(defun my-pytest-project-name-adapter (project-name)
  "If a custom project root is available, append its base name to the project name."
  (if-let ((subproject-root (my-pytest-locate-custom-project-root)))
      (format "%s:%s"
              project-name
              (file-name-nondirectory (directory-file-name subproject-root)))
    project-name))

(defcustom my-pytest-tmux-target-pane nil
  "The tmux pane which will receive test commands."
  :type 'string
  :safe 'stringp)

(defun my-pytest-set-tmux-target-pane ()
  "Read and set the target tmux pane"
  (interactive)
  (let* ((target (string-trim (read-string "target tmux pane: "
                                           my-pytest-tmux-target-pane))))
    (if (not (equal target ""))
        (setq my-pytest-tmux-target-pane target))))

(defun my-tmux-select-pane ()
  "Select a tmux pane from a list of active choices"
  (let* ((panes (process-lines "tmux" "list-panes" "-a" "-F" "#S:#I.#P (#W)"))
         (candidates (lambda (str pred action)
                       (if (eq action 'metadata)
                           '(metadata
                             (cycle-sort-function . identity)
                             (display-sort-function . identity))
                         (complete-with-action action panes str pred))))
         (selected-pane (completing-read "Pane: " candidates nil t)))
    (when (string-match "\\`\\(.*\\) .*\\'" selected-pane)
      (match-string 1 selected-pane))))

(defun my-pytest-select-tmux-pane ()
  (interactive)
  (when-let (pane (my-tmux-select-pane))
    (setq my-pytest-tmux-target-pane pane)
    (message "Selected pane: %s" my-pytest-tmux-target-pane)))

(defun my-python-mode-setup ()
  "Hook to run when python-mode is enabled"
  (my-lsp-ensure))

(use-package python
  :bind (:map python-mode-map
              ("C-c /" . my-python-debug-insert-ipdb-set-trace))
  :hook (python-mode . my-python-mode-setup)
  :custom
  (python-fill-docstring-style 'symmetric))

(use-package python-pytest
  :after python
  :straight t
  :bind (:map python-mode-map
              ("C-o C-t" . python-pytest-dispatch))
  :config
  (pcase-dolist (`(,loc . ,key) '(("t" . "a")
                                  ("f" . "_") ("F" . "f") ("_" . "F")
                                  ("d" . "T") ("D" . "t")))
    (transient-suffix-put 'python-pytest-dispatch loc :key key))
  (advice-add #'python-pytest--project-root :before-until
              #'my-pytest-locate-custom-project-root)
  (advice-add #'python-pytest--project-name :filter-return
              #'my-pytest-project-name-adapter))

(provide 'init-python)
