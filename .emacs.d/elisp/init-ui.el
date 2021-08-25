;;; -*- lexical-binding: t; -*-

(menu-bar-mode -1)
(set-frame-font "Menlo Nerd Font-13" nil t)

(when (display-graphic-p)
  (add-to-list 'default-frame-alist (cons 'width 102))
  (add-to-list 'default-frame-alist (cons 'height 56))
  (set-frame-position nil 604 0)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (if (version< emacs-version "26")
      (use-package nlinum
        :straight t
        :config
        (global-nlinum-mode 1))
    (progn
      (setq display-line-numbers-grow-only t)
      (global-display-line-numbers-mode))))

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t
      use-dialog-box nil
      initial-scratch-message nil
      confirm-kill-emacs 'y-or-n-p
      enable-recursive-minibuffers t
      ;; Do not allow the cursor in the minibuffer prompt
      minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(defun my-modus-themes-custom-faces ())

(use-package modus-themes
  :straight t
  :custom
  (modus-themes-bold-constructs nil)
  (modus-themes-italic-constructs t)
  (modus-themes-mode-line '(3d))
  (modus-themes-region '(bg-only))
  (modus-themes-paren-match '(intense))
  (modus-themes-completions 'opinionated)
  :bind ("C-o w" . modus-themes-toggle)
  :init
  (add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces)
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi))

(use-package minions
  :straight t
  :demand t
  :custom (minions-mode-line-lighter "")
  :bind ("C-o =" . minions-minor-modes-menu)
  :config (minions-mode 1))

(provide 'init-ui)
