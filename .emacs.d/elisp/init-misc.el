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
  :bind ("C-o C-o" . ace-window))

(use-package company
  :straight t
  :diminish company-mode
  :bind (:map company-active-map
              ("TAB"))
  :custom
  (company-backends '(company-capf
                      company-files
                      company-dabbrev-code
                      company-dabbrev))
  (company-idle-delay 0.3)
  (company-tooltip-limit 10)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-dabbrev-other-buffers nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  :init
  (add-hook 'after-init-hook 'global-company-mode))

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
  :custom (flycheck-keymap-prefix (kbd "C-o e")))

(use-package gcmh
  :straight t
  :diminish gcmh-mode
  :hook (emacs-startup . gcmh-mode)
  :custom
  (gcmh-idle-delay 5)
  (gcmh-high-cons-threshold (* 16 1024 1024))
  (gcmh-verbose nil))

(defun my-lispy-emacs-lisp-mode-hook ()
  "Enable lispy-mode in any Emacs lisp buffer except for the scratch buffer."
  (when (not (string= (buffer-name) "*scratch*"))
    (lispy-mode 1)))

(use-package lispy
  :straight t
  :diminish lispy-mode
  :hook (emacs-lisp-mode . my-lispy-emacs-lisp-mode-hook)
  :bind (("M-s m" . lispy-mark))
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
  :custom (magit-define-global-key-bindings nil)
  :bind (("s-m m" . magit-status)
         ("s-m d" . magit-file-dispatch)
         ("s-m l" . magit-log)
         ("s-m f" . magit-log-buffer-file)
         ("s-m b" . magit-blame))
  :hook (git-commit-setup . git-commit-turn-on-flyspell))

(use-package org
  :straight t
  :bind (:map org-mode-map
              ("s-<return>" . org-meta-return)
              ("s-<left>" . org-metaleft)
              ("s-<right>" . org-metaright)
              ("s-<up>" . org-metaup)
              ("s-<down>" . org-metadown))
  :hook (org-mode . my-fill-column-setup)
  :custom
  (org-cycle-separator-lines 1)
  (org-log-done 'time))

(use-package project
  :defer)

(use-package projectile
  :straight t
  :diminish projectile-mode
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-mode +1))

(use-package rainbow-mode
  :straight t
  :custom
  (rainbow-html-colors-major-mode-list
   '(html-mode css-mode php-mode nxml-mode xml-mode scss-mode)))

;; internal
(use-package straight-helpers)

(use-package which-key
  :straight t
  :diminish which-key-mode
  :init
  (which-key-mode))

(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  (add-hook 'snippet-mode-hook
            (lambda ()
              (set (make-local-variable require-final-newline) nil))))

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
