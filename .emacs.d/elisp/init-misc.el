;;; Load miscellaneous packages -*- lexical-binding: t; -*-

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

(use-package css
  :defer t
  :custom
  (css-indent-offset 2))

(use-package direnv
  :straight t
  :custom
  (direnv-always-show-summary nil)
  :init
  (direnv-mode))

(use-package deadgrep
  :straight t
  :bind ("C-c u" . deadgrep))

(defun my-shr-strong-tag (dom)
  "Improve the styling of a <strong> tag if it appears to come
from a GCP release notes entry."
  (if-let ((class (dom-attr dom 'class))
           ((string-match-p "\\brelease-note-product-title\\b" class)))
      (shr-tag-h2 dom)
    (shr-tag-strong dom)))

(defun my-elfeed-show-mode-hook ()
  (setq-local shr-width 88
              shr-external-rendering-functions '((strong . my-shr-strong-tag)))
  (pcase-dolist (`(,face . ,height) '((shr-h1 . 2.074)
                                      (shr-h2 . 1.728)
                                      (shr-h3 . 1.44)
                                      (shr-h4 . 1.2)
                                      (shr-h5 . 1.0)
                                      (shr-h6 . 1.0)))
    (face-remap-add-relative face :height height :weight 'semi-bold)))

(use-package elfeed
  :straight t
  :hook (elfeed-show-mode . my-elfeed-show-mode-hook)
  :bind ("C-c r" . elfeed))

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package gcmh
  :straight t
  :hook (emacs-startup . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024))
  (gcmh-verbose nil))

;; internal
(use-package golink
  :bind (("C-o g" . golink-open)
         :map embark-url-map
         ("g" . golink-create)))

(use-package helpful
  :straight t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)
         ("C-o d" . helpful-at-point)))

(use-package js-mode
  :mode ("\\.js\\'"
         "\\.json\\'")
  :custom
  (js-indent-level 2))

(use-package just-mode
  :straight t
  :defer t)

(defun my-ledger-mode-hook ()
  (setq-local corfu-quit-no-match t))

(use-package ledger-mode
  :straight t
  :defer t
  :hook
  (ledger-mode . my-ledger-mode-hook)
  :custom
  (ledger-default-date-format ledger-iso-date-format))

(defun my-lispy-emacs-lisp-mode-hook ()
  "Enable lispy-mode in any Emacs lisp buffer except for the scratch buffer."
  (when (not (string= (buffer-name) "*scratch*"))
    (lispy-mode 1)))

(use-package lispy
  :straight t
  :hook (emacs-lisp-mode . my-lispy-emacs-lisp-mode-hook)
  :bind (:map lispy-mode-map-lispy
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

(use-package lua-mode
  :straight t
  :defer t)

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

(use-package markdown-mode
  :straight t
  :hook (markdown-mode . my-fill-column-setup)
  :custom
  (markdown-command "pandoc"))

(use-package nix-mode
  :straight t
  :defer t)

(defun my-org-open-line-after-meta-data ()
  "Open a new line after the the following metadata."
  (interactive)
  (org-end-of-meta-data t)
  (open-line 1)
  (while (org-previous-line-empty-p)
    (next-line -1)))

(use-package org
  :straight t
  :bind (nil
         :map org-mode-map
         ("C-'" . nil)
         ("s-<return>" . org-meta-return)
         ("s-<left>" . org-metaleft)
         ("s-<right>" . org-metaright)
         ("s-<up>" . org-metaup)
         ("s-<down>" . org-metadown)
         ("M-t o" . consult-org-heading)
         ("C-s-<return>" . my-org-open-line-after-meta-data)
         ("s-RET" . my-org-open-line-after-meta-data)
         :repeat-map my-org-motion-repeat-map
         :exit
         ("j" . org-goto)
         :continue
         ("u" . outline-up-heading)
         ("b" . org-backward-heading-same-level)
         ("f" . org-forward-heading-same-level)
         ("p" . org-previous-visible-heading)
         ("n" . org-next-visible-heading))
  :hook (org-mode . my-fill-column-setup)
  :custom
  (org-catch-invisible-edits 'show-and-error)
  (org-cycle-separator-lines 1)
  (org-hide-emphasis-markers t)
  (org-fontify-done-headline nil)
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

(defun my-project--from-local-command ()
  "Return non-nil if `this-command' should respect project local
 identifiers."
  (and this-command
       my-project-local-commands
       (memq this-command my-project-local-commands)))

(defun my-project--use-local ()
  "Return non-nil if local project identifiers should be used to
find projects."
  (or
   ;; Either eglot is searching for a project
   (and (boundp 'eglot-lsp-context) eglot-lsp-context)
   ;; Or the current command should respect local identifiers
   (my-project--from-local-command)))

(defun my-project-try-local (dir)
  "Determine if DIR is a non-VC project.
DIR must include a .project file to be considered a project."
  (when-let (((my-project--use-local))
             (root (if (listp my-project-local-identifier)
                       (seq-some (lambda (n)
                                   (locate-dominating-file dir n))
                                 my-project-local-identifier)
                     (locate-dominating-file dir my-project-local-identifier))))
    (cons 'local root)))

(defun my-project-get-relative-path (path &optional project)
  "Return PATH relative to the root of PROJECT. If PROJECT is nil,
use the current project."
  (let ((project (or project (project-current))))
    (file-relative-name path (project-root project))))

(defun my-copy-project-relative-path-as-kill (&optional path)
  "Copy PATH relative to the current project's path to the
 kill ring. If PATH is nil, use `buffer-file-name'."
  (interactive)
  (when-let ((path (or path buffer-file-name))
             (rel-path (my-project-get-relative-path path)))
    (message "%s" rel-path)
    (kill-new rel-path)))

(defun my-dired-copy-project-relative-path-as-kill ()
  "Copy the project relative path of the current line of a dired
 buffer to the kill ring."
  (interactive)
  (when-let ((path (dired-get-filename)))
    (my-copy-project-relative-path-as-kill path)))

(use-package project
  :bind-keymap ("s-p" . project-prefix-map)
  :bind (nil
         :map project-prefix-map
         ("w" . my-copy-project-relative-path-as-kill))
  :config
  (add-hook 'project-find-functions 'my-project-try-local -10)
  (cl-defmethod project-root ((project (head local)))
    "Return root directory of current PROJECT."
    (cdr project))

  (with-eval-after-load 'dired
    (bind-key "W" #'my-dired-copy-project-relative-path-as-kill
              dired-mode-map)))

(use-package puni
  :disabled t
  :straight t
  :hook ((emacs-lisp-mode) . puni-disable-puni-mode)
  :bind (nil
         :map puni-mode-map
         ("C-s-t" . puni-syntactic-forward-punct)
         ("C-s-s" . puni-syntactic-backward-punct)
         :repeat-map puni-mode-repeat-map
         ("f" . puni-forward-sexp)
         ("b" . puni-backward-sexp)
         ("a" . puni-beginning-of-sexp)
         ("e" . puni-end-of-sexp)
         ("t" . puni-syntactic-forward-punct)
         ("s" . puni-syntactic-backward-punct)
         ("T" . puni-syntactic-backward-punct))
  :init
  (puni-global-mode))

(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode-enable))

(use-package rst-mode
  :defer t
  :hook (rst-mode . my-fill-column-setup))

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

(use-package toml-mode
  :straight t
  :defer t)

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
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 5000)
  (vterm-timer-delay .025)
  (vterm-toggle-scope 'project)
  :bind (nil
         :map vterm-mode-map
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

(use-package yaml-mode
  :straight t
  :defer t)

(defun my-yasnippet-snippet-mode-hook ()
  (setq-local require-final-newline nil))

(use-package yasnippet
  :straight t
  :hook (snippet-mode . my-yasnippet-snippet-mode-hook)
  :init
  (yas-global-mode 1))

(provide 'init-misc)
