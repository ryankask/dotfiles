;;; Load miscellaneous packages -*- lexical-binding: t; -*-

(use-package 1password
  :commands (1p-item-get 1p-read))

(defun my-avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act)))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(use-package avy
  :ensure t
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
  :ensure t
  :bind (("C-o C-o" . ace-window)
         ("s-o" . ace-window))
  :custom
  (aw-keys '(?n ?e ?i ?o ?t ?s ?r ?a))
  :config
  (pcase-dolist (`(,oldkey . ,item) '((?n ?f aw-flip-window)
                                      (?o ?k delete-other-windows "Delete Other Windows")))
    (assq-delete-all oldkey aw-dispatch-alist)
    (add-to-list 'aw-dispatch-alist item)))

(use-package casual
  :ensure t
  :defer t)

(use-package casual-bookmarks
  :after bookmark
  :bind (nil
         :map bookmark-bmenu-mode-map
         ("s-h" . casual-bookmarks-tmenu)))

(use-package casual-calc
  :after calc
  :bind (nil
         :map calc-mode-map
         ("s-h" . casual-calc-tmenu)
         :map calc-alg-map
         ("s-h" . casual-calc-tmenu)))

(use-package casual-calendar
  :after calendar
  :bind (nil
         :map calendar-mode-map
         ("s-h" . casual-calendar-tmenu)))

(use-package casual-dired
  :after dired
  :bind (nil
         :map dired-mode-map
         ("s-h" . casual-dired-tmenu)
         ("s" . casual-dired-sort-by-tmenu)
         ("/" . casual-dired-search-replace-tmenu)))

(use-package casual-ibuffer
  :after ibuffer
  :bind (nil
         :map ibuffer-mode-map
         ("s-h" . casual-ibuffer-tmenu)
         ("F" . casual-ibuffer-filter-tmenu)
         ("s" . casual-ibuffer-sortby-tmenu)))

(use-package casual-image
  :after image
  :bind (nil
         :map image-mode-map
         ("s-h" . casual-image-tmenu)))

(use-package casual-info
  :after info
  :bind (nil
         :map Info-mode-map
         ("s-h" . casual-info-tmenu)))

(use-package casual-isearch
  :after isearch
  :bind (nil
         :map isearch-mode-map
         ("s-h" . casual-info-tmenu)))

(use-package casual-re-builder
  :after re-builder
  :bind (nil
         :map reb-mode-map
         ("s-h" . casual-re-builder-tmenu)
         :map reb-lisp-mode-map
         ("s-h" . casual-re-builder-tmenu)))

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
  :disabled t
  :ensure (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
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
  :ensure t
  :defer t)

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind (nil
         :prefix-map my-denote-map
         :prefix "C-o n"
         ("n" . denote)
         ("r" . denote-rename-file)
         ("i" . denote-link)
         ("b" . denote-backlinks)
         ("d" . denote-dired)
         ("g" . denote-grep))
  :custom
  (denote-directory (expand-file-name "~/Sync/notes"))
  (denote-known-keywords '("home" "work" "tech"))
  (denote-save-buffer-after-creation t)
  :config
  (denote-rename-buffer-mode 1))

(use-package disproject
  :ensure (:host github :repo "aurtzy/disproject")
  :after project
  :bind (("s-p" . disproject-dispatch)
         :map ctl-x-map
         ("p" . disproject-dispatch))
  :custom
  (disproject-find-line-command #'consult-line-multi)
  (disproject-switch-to-buffer-command #'consult-buffer)
  :config
  (transient-append-suffix 'disproject-dispatch '(-2 -1)
    ["Extra"
     ("W" "Kill path" my-copy-project-relative-path-as-kill)]))

(use-package deadgrep
  :ensure t
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
  (if-let* ((class (dom-attr dom 'class))
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

(defvar my-elfeed--last-refresh nil
  "The last time `my-elfeed' was called.")

(defun my-elfeed (&optional max-age)
  "Enter elfeed and refresh all feeds if the database was last updated
 more than MAX-AGE minutes ago.

If MAX-AGE is nil, default to 15 minutes."
  (interactive)
  (elfeed)
  (when (> (- (float-time) (or my-elfeed--last-refresh 0)) (* 60 (or max-age 15)))
    (elfeed-update)
    (setq my-elfeed--last-refresh (float-time))))

(use-package elfeed
  :ensure t
  :hook (elfeed-show-mode . my-elfeed-show-mode-hook)
  :bind ("C-o y" . my-elfeed))

(use-package envrc
  :ensure t
  :hook (elpaca-after-init . envrc-global-mode)
  :custom
  (envrc-remote t)
  :config
  (bind-key "C-o v" envrc-command-map 'envrc-mode-map))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package gcmh
  :ensure t
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

;; internal
(use-package hcl-ts-mode
  :mode "\\.hcl\\'")

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)
         ("C-o C-h" . helpful-at-point)))

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
  :ensure t
  :defer t)

(defun my-lispy-emacs-lisp-mode-hook ()
  "Enable lispy-mode in any Emacs lisp buffer except for the scratch buffer."
  (when (not (string= (buffer-name) "*scratch*"))
    (lispy-mode 1)))

(use-package lispy
  :ensure t
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
  (lispy-define-key lispy-mode-map "v" 'lispy-mark-car)

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

(use-package lua-ts-mode
  :if (and (fboundp 'lua-ts-mode)
           (treesit-language-available-p 'lua))
  :mode "\\.lua\\'")

(use-package magit
  :ensure t
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
  :ensure t
  :hook (markdown-mode . my-fill-column-setup)
  :custom
  (markdown-command "pandoc"))

(use-package mermaid-ts-mode
  :if (treesit-language-available-p 'mermaid)
  :mode "\\.mmd\\'"
  :ensure (:host github :repo "JonathanHope/mermaid-ts-mode" :files ("mermaid-ts-mode.el")))

(use-package nix-mode
  :ensure t
  :defer t)

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
  (when-let* (((my-project--use-local))
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
  (when-let* ((path (or path buffer-file-name))
              (rel-path (my-project-get-relative-path path)))
    (kill-new rel-path)))

(defun my-dired-copy-project-relative-path-as-kill ()
  "Copy the project relative path of the current line of a dired
 buffer to the kill ring."
  (interactive)
  (when-let* ((path (dired-get-filename)))
    (my-copy-project-relative-path-as-kill path)))

(use-package project
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

(use-package rainbow-delimiters
  :ensure t
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
  (when-let* ((pos (tempel--beginning)))
    (when (> (point) pos)
      (goto-char pos))
    (tempel-done)))

(defun my-tempel-immediate-done-end ()
  "If required, move to the last field and then finish the
session."
  (interactive)
  (when-let* ((pos (tempel--end)))
    (when (< (point) pos)
      (goto-char pos))
    (tempel-done)))

(use-package tempel
  :disabled t
  :ensure (:host github :repo "minad/tempel")
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

(use-package verb
  :ensure t
  :after org
  :init
  (keymap-set org-mode-map "C-c C-r" verb-command-map))

(defun my-vterm-mode-hook ()
  (setq-local confirm-kill-processes nil))

(defun my-vterm-send-C-k ()
  "Send `C-k' to libvterm."
  (interactive)
  (kill-ring-save (point) (vterm-end-of-line))
  (vterm-send-key "k" nil nil t))

(use-package vterm
  :ensure t
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
  :ensure t
  :bind ("C-o C-v" . vterm-toggle))

(defun my-yaml-ts-mode-setup ()
  "Hook used for yaml-ts-mode"
  (setq-local tab-width 2))

(use-package yaml-ts-mode
  :if (and (fboundp 'yaml-ts-mode)
           (treesit-language-available-p 'yaml))
  :mode "\\.ya?ml\\'"
  :hook (yaml-ts-mode . my-yaml-ts-mode-setup))

(defun my-yasnippet-snippet-mode-hook ()
  (setq-local require-final-newline nil))

(use-package yasnippet
  :ensure t
  :hook ((elpaca-after-init . yas-global-mode)
         (snippet-mode . my-yasnippet-snippet-mode-hook))
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets/" user-emacs-directory)
                          (expand-file-name "cloud/snippets" user-emacs-directory))))

(provide 'init-misc)
