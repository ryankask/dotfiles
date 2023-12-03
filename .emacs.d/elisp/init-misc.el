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
  :elpaca t
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
  :elpaca t
  :bind (("C-o C-o" . ace-window)
         ("s-o" . ace-window))
  :custom
  (aw-keys '(?n ?e ?i ?o ?t ?s ?r ?a))
  :config
  (pcase-dolist (`(,oldkey . ,item) '((?n ?f aw-flip-window)
                                      (?o ?k delete-other-windows "Delete Other Windows")))
    (assq-delete-all oldkey aw-dispatch-alist)
    (add-to-list 'aw-dispatch-alist item)))

(use-package css-mode
  :preface
  (my-try-treesit-lang 'css 'css-mode 'css-ts-mode)
  :defer t
  :custom
  (css-indent-offset 2))

(defun my-copilot-post-command ()
  "Clear the overlay"
  (when (and this-command
             (not (and (symbolp this-command)
                       (or
                        (s-starts-with-p "copilot-" (symbol-name this-command))
                        (member this-command copilot-clear-overlay-ignore-commands)
                        (copilot--self-insert this-command)))))
    (copilot-clear-overlay)))

(use-package copilot
  :elpaca (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :bind (nil
         :map copilot-mode-map
         ("C-<tab>" . copilot-complete)
         ("C-c c" . copilot-complete)
         ("C-c a" . copilot-panel-complete)
         :map copilot-completion-map
         ("C-g" . copilot-clear-overlay)
         ("C-f" . copilot-accept-completion)
         ("M-f" . copilot-accept-completion-by-word)
         ("C-e" . copilot-accept-completion-by-line)
         ("M-n" . copilot-next-completion)
         ("M-p" . copilot-previous-completion))
  :config
  (advice-add #'copilot-complete :before #'corfu-quit)
  (advice-add #'copilot--post-command :override #'my-copilot-post-command))

(use-package csv-mode
  :elpaca t
  :defer t)

(use-package direnv
  :elpaca t
  :custom
  (direnv-always-show-summary nil)
  :init
  (direnv-mode))

(use-package deadgrep
  :elpaca t
  :bind ("C-c u" . deadgrep))

(use-package dockerfile-ts-mode
  :if (and (fboundp 'dockerfile-ts-mode)
           (treesit-language-available-p 'dockerfile))
  :mode "[/\\]\\(?:Containerfile\\|Dockerfile\\)\\(?:\\.[^/\\]*\\)?\\'")

(use-package ediff
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

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
  :elpaca t
  :hook (elfeed-show-mode . my-elfeed-show-mode-hook)
  :bind ("C-c r" . elfeed))

(use-package expand-region
  :elpaca t
  :bind ("C-=" . er/expand-region))

(use-package gcmh
  :elpaca t
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

(use-package gptel
  :elpaca t
  :custom
  (gptel-model "gpt-4")
  :bind (("C-c #" . gptel)
         ("C-c RET" . gptel-send)))

;; internal
(use-package hcl-ts-mode
  :mode "\\.hcl\\'")

(use-package helpful
  :elpaca t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)
         ("C-c C-d" . helpful-at-point)))

(use-package js-mode
  :preface
  (my-try-treesit-lang 'javascript 'javascript-mode 'js-ts-mode)
  :defer t
  :custom
  (js-indent-level 2))

(use-package json-ts-mode
  :if (and (fboundp 'json-ts-mode)
           (treesit-language-available-p 'json))
  :mode "\\.json\\'")

(use-package just-mode
  :elpaca t
  :defer t)

(defun my-lispy-emacs-lisp-mode-hook ()
  "Enable lispy-mode in any Emacs lisp buffer except for the scratch buffer."
  (when (not (string= (buffer-name) "*scratch*"))
    (lispy-mode 1)))

(use-package lispy
  :elpaca t
  :hook ((emacs-lisp-mode . my-lispy-emacs-lisp-mode-hook)
         (lispy-mode . turn-off-smartparens-mode))
  :bind (nil
         :map lispy-mode-map-lispy
         ("C-s-," . lispy-mark))
  :custom
  (lispy-close-quotes-at-end-p t)
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
  :elpaca t
  :defer t)

(use-package magit
  :elpaca t
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
  :elpaca t
  :hook (markdown-mode . my-fill-column-setup)
  :custom
  (markdown-command "pandoc"))

(use-package nix-mode
  :elpaca t
  :defer t)

(defun my-org-open-line-after-meta-data ()
  "Open a new line after the the following metadata."
  (interactive)
  (org-end-of-meta-data t)
  (open-line 1)
  (while (org-previous-line-empty-p)
    (next-line -1)))

(use-package org
  :elpaca t
  :bind (nil
         :map org-mode-map
         ("C-'" . nil)
         ("C-c #" . nil)
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
  (org-babel-load-languages '((emacs-lisp . t)
                              (sql . t)))
  (org-hide-emphasis-markers t)
  (org-fontify-done-headline nil)
  (org-insert-heading-respect-content t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-pretty-entities t)
  (org-special-ctrl-a/e t)
  (org-use-sub-superscripts "{}"))

(use-package org-modern
  :elpaca t
  :after org
  :custom-face
  (org-modern-label ((t (:height 0.85))))
  :init
  (global-org-modern-mode))

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
  :elpaca t
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
  :elpaca t
  :hook
  (prog-mode . rainbow-delimiters-mode-enable))

(use-package rst-mode
  :defer t
  :hook (rst-mode . my-fill-column-setup))

(use-package sh-mode
  :defer t
  :custom
  (sh-basic-offset 2)
  (sh-indent-for-continuation sh-basic-offset)
  :config
  (dolist (capf (list #'sh-completion-at-point-function
                      #'comint-completion-at-point))
    (advice-add capf :around
                (lambda (orig)
                  (cape-wrap-properties orig :exclusive 'no)))))

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
  :elpaca (tempel :host github :repo "minad/tempel")
  :bind (:map tempel-map
         ("s-]" . tempel-next)
         ("s-[" . tempel-previous)
         ("s-<return>" . tempel-done)
         ("C-s-[" . my-tempel-immediate-done-beginning)
         ("C-s-]" . my-tempel-immediate-done-end))
  :hook ((prog-mode text-mode) . tempel-setup-capf))

(use-package toml-ts-mode
  :if (and (fboundp 'toml-ts-mode)
           (treesit-language-available-p 'toml))
  :mode "\\.toml\\'")

(defun my-typescript-mode-hook ()
  "Set up a Typescript base mode managed buffer"
  (my-lsp-ensure)
  (setq-local my-format-with-lsp nil)
  (apheleia-mode))

(use-package typescript-ts-mode
  :if (and (fboundp 'typescript-ts-mode)
           (treesit-language-available-p 'typescript))
  :mode ("\\.ts\\'" ("\\.tsx\\'" . tsx-ts-mode))
  :hook (typescript-ts-base-mode . my-typescript-mode-hook))

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
  :elpaca t
  :hook (vterm-mode . my-vterm-mode-hook)
  :custom
  (vterm-always-compile-module t)
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 5000)
  (vterm-timer-delay 0.025)
  (vterm-toggle-scope 'project)
  (vterm-tramp-shells '(("ssh" "/usr/bin/zsh")
                        ("docker" "/bin/sh")))
  :bind (nil
         :map vterm-mode-map
         ("C-k" . my-vterm-send-C-k)
         ("C-o" . nil)
         ("C-o C-t" . vterm-copy-mode)
         ("C-o C-g" . vterm-send-C-g)
         ("C-o C-u" . vterm-send-C-u)
         ("C-o C-l" . vterm-clear-scrollback)
         :map vterm-copy-mode-map
         ("C-o C-t" . vterm-copy-mode))
  :init
  (add-to-list 'display-buffer-alist
               '("\\*vterm\\*"
                 (display-buffer-reuse-window display-buffer-below-selected)
                 (window-min-height . 28))))

(use-package vterm-toggle
  :elpaca t
  :bind ("C-o C-v" . vterm-toggle))

(use-package which-key
  :elpaca t
  :custom
  (which-key-use-C-h-commands nil)
  :init
  (which-key-mode))

(use-package yaml-ts-mode
  :if (and (fboundp 'yaml-ts-mode)
           (treesit-language-available-p 'yaml))
  :mode "\\.ya?ml\\'")

(defun my-yasnippet-snippet-mode-hook ()
  (setq-local require-final-newline nil))

(use-package yasnippet
  :elpaca t
  :hook (snippet-mode . my-yasnippet-snippet-mode-hook)
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets/" user-emacs-directory)
                          "~/iCloud/config/emacs/snippets"))
  :init
  (yas-global-mode 1))

(provide 'init-misc)
