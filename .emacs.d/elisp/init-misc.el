;;; Load miscellaneous packages -*- lexical-binding: t; -*-

;;; Productivity

(use-package avy
  :straight t
  :bind (("C-'" . avy-goto-char-timer)
         :map goto-map
         ("l" . avy-goto-line)
         ("w" . avy-goto-word-0)
         ("M-w" . avy-goto-word-1))
  :custom
  (avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
  (avy-timeout-seconds 0.3))

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

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package flycheck
  :straight t
  :diminish flycheck-mode
  :custom (flycheck-keymap-prefix (kbd "C-o f")))

(use-package gcmh
  :straight t
  :diminish gcmh-mode
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
  :diminish lispy-mode
  :hook (emacs-lisp-mode . my-lispy-emacs-lisp-mode-hook)
  :bind (:map lispy-mode-map-lispy
              ("s-," . lispy-mark))
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

(use-package org
  :straight t
  :bind (:map org-mode-map
              ("C-'" . nil)
              ("s-<return>" . org-meta-return)
              ("s-<left>" . org-metaleft)
              ("s-<right>" . org-metaright)
              ("s-<up>" . org-metaup)
              ("s-<down>" . org-metadown)
              ("M-t o" . consult-org-heading))
  :hook (org-mode . my-fill-column-setup)
  :custom
  (org-cycle-separator-lines 1)
  (org-log-done 'time))

(use-package project
  :bind-keymap ("s-p" . project-prefix-map))

(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode-enable))

;; internal
(use-package straight-helpers)

(defun tempel-setup-capf ()
  (add-hook 'completion-at-point-functions #'tempel-expand -1 'local))

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
  :straight (tempel :type git :host github :repo "minad/tempel")
  :bind (:map tempel-map
         ("s-]" . tempel-next)
         ("s-[" . tempel-previous)
         ("s-<return>" . tempel-done)
         ("C-s-[" . my-tempel-immediate-done-beginning)
         ("C-s-]" . my-tempel-immediate-done-end))
  :hook ((prog-mode text-mode) . tempel-setup-capf))

(use-package vterm
  :disabled t
  :straight t
  :bind (:map vterm-mode-map
         ("C-o" . nil)
         ("C-o C-t" . vterm-copy-mode)))

(use-package which-key
  :straight t
  :diminish which-key-mode
  :custom
  (which-key-use-C-h-commands nil)
  :init
  (which-key-mode))

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
  :mode "\\.zsh\\'"
  :defer t
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2))

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
