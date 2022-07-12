;;; Load miscellaneous packages -*- lexical-binding: t; -*-

;;; Productivity

(defun my-avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act)))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(use-package avy
  :straight t
  :bind (("C-'" . avy-goto-char-timer)
         :map goto-map
         ("l" . avy-goto-line)
         ("w" . avy-goto-word-0)
         ("M-w" . avy-goto-word-1)
         :map isearch-mode-map
         ("C-'" . avy-isearch))
  :custom
  (avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
  (avy-timeout-seconds 0.3)
  :config
  ;; Extra actions from Karthink (https://github.com/karthink/.emacs.d)
  (setf (alist-get ?. avy-dispatch-alist) 'my-avy-action-embark))

(use-package ace-window
  :straight t
  :bind (("C-o C-o" . ace-window)
         ("s-o" . ace-window))
  :custom
  (aw-keys '(?t ?s ?r ?n ?e ?i ?o ?a)))

(use-package direnv
  :straight t
  :custom
  (direnv-always-show-summary nil)
  :init
  (direnv-mode))

(use-package deadgrep
  :straight t
  :bind ("C-c u" . deadgrep))

(use-package elfeed
  :straight t
  :bind ("C-c r" . elfeed)
  :custom
  (elfeed-feeds
   '("https://cloud.google.com/feeds/gcp-release-notes.xml"
     "https://status.cloud.google.com/en/feed.atom"
     "https://docs.digitalocean.com/release-notes/index.xml"
     "https://www.londonreconnections.com/feed/")))

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package flycheck
  :straight t
  :custom (flycheck-keymap-prefix (kbd "C-o C-k")))

(use-package gcmh
  :straight t
  :hook (emacs-startup . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024))
  (gcmh-verbose nil))

(use-package helpful
  :straight t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)
         ("C-o d" . helpful-at-point)))

(defun my-lispy-emacs-lisp-mode-hook ()
  "Enable lispy-mode in any Emacs lisp buffer except for the scratch buffer."
  (when (not (string= (buffer-name) "*scratch*"))
    (lispy-mode 1)))

(use-package lispy
  :straight t
  :hook (emacs-lisp-mode . my-lispy-emacs-lisp-mode-hook)
  :bind (:map lispy-mode-map-lispy
         ("s-," . lispy-mark-symbol)
         ("C-s-," . lispy-mark))
  :config
  ;; Colemak-friendly replacements
  ;; Note this comment from the author: https://github.com/abo-abo/lispy/issues/324#issuecomment-270357175
  (lispy-define-key lispy-mode-map "n" 'lispy-down)
  (lispy-define-key lispy-mode-map "e" 'lispy-up)
  (lispy-define-key lispy-mode-map "i" 'lispy-right)
  (lispy-define-key lispy-mode-map "t" 'lispy-flow)
  (lispy-define-key lispy-mode-map "f" 'lispy-teleport)
  (lispy-define-key lispy-mode-map "j" 'lispy-new-copy)
  (lispy-define-key lispy-mode-map "k" 'lispy-eval)
  (lispy-define-key lispy-mode-map "y" 'lispy-tab)
  (lispy-define-key lispy-mode-map "l" 'lispy-occur)

  (lispy-defverb
   "other"
   (("h" lispy-move-left)
    ("n" lispy-down-slurp)
    ("e" lispy-up-slurp)
    ("i" lispy-move-right)
    ("SPC" lispy-other-space)
    ("d" lispy-goto-mode)))

  (defhydra lh-knight ()
    "knight"
    ("n" lispy-knight-down)
    ("e" lispy-knight-up)
    ("z" nil)))

(use-package magit
  :straight t
  :bind (("s-m m" . magit-status)
         ("s-m d" . magit-file-dispatch)
         ("s-m l" . magit-log)
         ("s-m f" . magit-log-buffer-file)
         ("s-m b" . magit-blame))
  :hook (git-commit-setup . git-commit-turn-on-flyspell)
  :custom
  (magit-define-global-key-bindings nil)
  (magit-diff-refine-hunk t))

(defun my-org-open-line-after-meta-data ()
  "Open a new line after the the following metadata."
  (interactive)
  (org-end-of-meta-data t)
  (open-line 1)
  (while (org-previous-line-empty-p)
    (next-line -1)))

(use-package org
  :straight t
  :bind (:map org-mode-map
              ("C-'" . nil)
              ("s-<return>" . org-meta-return)
              ("s-<left>" . org-metaleft)
              ("s-<right>" . org-metaright)
              ("s-<up>" . org-metaup)
              ("s-<down>" . org-metadown)
              ("M-t o" . consult-org-heading)
              ("C-s-<return>" . my-org-open-line-after-meta-data)
              ("s-RET" . my-org-open-line-after-meta-data))
  :hook (org-mode . my-fill-column-setup)
  :custom
  (org-catch-invisible-edits 'show-and-error)
  (org-cycle-separator-lines 1)
  (org-hide-emphasis-markers t)
  (org-insert-heading-respect-content t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-pretty-entities t)
  (org-special-ctrl-a/e t)
  (org-use-sub-superscripts "{}"))

;; Adapted from https://github.com/karthink/project-x/blob/master/project-x.el

(defcustom my-project-local-identifier (list ".project" "pyproject.toml")
  "Filename(s) that identifies a directory as a project.
You can specify a single filename or a list of names."
  :type '(choice (string :tag "Single file")
                 (repeat (string :tag "Filename")))
  :safe (lambda (x)
          (or (stringp x)
              (and (listp x)
                   (cl-every #'stringp x)))))

(defcustom my-project-local-commands (list #'vterm-toggle)
  "List of commands that should use local identifiers as project
markers."
  :type '(list function))

(defun my-project-try-local (dir)
  "Determine if DIR is a non-VC project.
DIR must include a .project file to be considered a project."
  (when-let (root (and
                   (or (not this-command)
                       (not my-project-local-commands)
                       (memq this-command my-project-local-commands))
                   (if (listp my-project-local-identifier)
                       (seq-some (lambda (n)
                                   (locate-dominating-file dir n))
                                 my-project-local-identifier)
                     (locate-dominating-file dir my-project-local-identifier))))
    (cons 'local root)))

(use-package project
  :bind-keymap ("s-p" . project-prefix-map)
  :custom
  (vterm-toggle-scope 'project)
  :config
  (add-hook 'project-find-functions 'my-project-try-local -10)
  (cl-defmethod project-root ((project (head local)))
    "Return root directory of current PROJECT."
    (cdr project)))

(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode-enable))

;; internal
(use-package straight-helpers)

(defun tempel-setup-capf ()
  (setq-local completion-at-point-functions
              (cons #'tempel-expand
                    completion-at-point-functions)))

(defun my-tempel-immediate-done-beginning ()
  "If required, move to the beginning of the first field and then
finish the session."
  (interactive)
  (when-let (pos (tempel--beginning))
    (when (> (point) pos)
      (goto-char pos))
    (tempel-done)))

(defun my-tempel-immediate-done-end ()
  "If required, move to the last field and then finish the
session."
  (interactive)
  (when-let (pos (tempel--end))
    (when (< (point) pos)
      (goto-char pos))
    (tempel-done)))

(use-package tempel
  :disabled t
  :straight (tempel :type git :host github :repo "minad/tempel")
  :bind (:map tempel-map
         ("s-]" . tempel-next)
         ("s-[" . tempel-previous)
         ("s-<return>" . tempel-done)
         ("C-s-[" . my-tempel-immediate-done-beginning)
         ("C-s-]" . my-tempel-immediate-done-end))
  :hook ((prog-mode text-mode) . tempel-setup-capf))

;; internal
(use-package use-package-helpers
  :config
  (dolist (func '(eval-last-sexp lispy-eval))
    (advice-add func :before-until #'uph-eval-last-sexp-advice)))

(defun my-vterm-mode-hook ()
  (setq-local confirm-kill-processes nil))

(defun my-vterm-send-C-k ()
  "Send `C-k' to libvterm."
  (interactive)
  (kill-ring-save (point) (vterm-end-of-line))
  (vterm-send-key "k" nil nil t))

(use-package vterm
  :straight t
  :hook (vterm-mode . my-vterm-mode-hook)
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 5000)
  (vterm-timer-delay .025)
  :bind (:map vterm-mode-map
         ("C-k" . my-vterm-send-C-k)
         ("C-o" . nil)
         ("C-o C-t" . vterm-copy-mode)
         ("C-o C-g" . vterm-send-C-g)
         ("C-o C-u" . vterm-send-C-u)
         ("C-o C-l" . vterm-clear-scrollback)
         :map vterm-copy-mode-map
         ("C-o C-t" . vterm-copy-mode)))

(use-package vterm-toggle
  :straight t
  :bind ("C-o C-v" . vterm-toggle))

(use-package which-key
  :straight t
  :custom
  (which-key-use-C-h-commands nil)
  :init
  (which-key-mode))

(defun my-yasnippet-snippet-mode-hook ()
  (setq-local require-final-newline nil))

(use-package yasnippet
  :straight t
  :hook (snippet-mode . my-yasnippet-snippet-mode-hook)
  :init
  (yas-global-mode 1))

;;; Languages

(use-package css
  :defer t
  :custom
  (css-indent-offset 2))

(use-package js-mode
  :mode ("\\.js\\'"
         "\\.json\\'")
  :custom
  (js-indent-level 2))

(use-package lua-mode
  :straight t
  :defer t)

(use-package scss-mode
  :straight t
  :defer t
  :custom
  (scss-compile-at-save nil))

(use-package sh-mode
  :defer t
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2)
  :config
  (dolist (capf (list #'sh-completion-at-point-function
                      #'comint-completion-at-point))
    (advice-add capf :around
                (lambda (orig)
                  (cape-wrap-properties orig :exclusive 'no)))))

;;; Formats

(use-package toml-mode
  :straight t
  :defer t)

(use-package yaml-mode
  :straight t
  :defer t)

;;; Writing

(use-package markdown-mode
  :straight t
  :hook (markdown-mode . my-fill-column-setup)
  :custom
  (markdown-command "pandoc"))

(use-package rst-mode
  :defer t
  :hook (rst-mode . my-fill-column-setup))

(provide 'init-misc)
