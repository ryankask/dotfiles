;;; -*- lexical-binding: t; -*-

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(set-frame-font "Menlo Nerd Font-13" nil t)

(when (display-graphic-p)
  (add-to-list 'default-frame-alist (cons 'width 102))
  (add-to-list 'default-frame-alist (cons 'height 56))
  (set-frame-position nil 604 0))

(use-package display-line-numbers
  :custom
  (display-line-numbers-grow-only t)
  :init
  (global-display-line-numbers-mode))

;; mode line settings
(column-number-mode t)
(size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq idle-update-delay 1.0
      inhibit-startup-message t
      use-dialog-box nil
      initial-scratch-message nil
      confirm-kill-emacs 'y-or-n-p
      enable-recursive-minibuffers t
      ;; Do not allow the cursor in the minibuffer prompt
      minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(use-package window
  :bind
  ("s-`" . delete-window)
  ("s-1" . delete-other-windows)
  ("s-2" . split-window-below)
  ("s-3" . split-window-right))

(defvar my-theme-frame-settings nil
  "Alist that contains frame settings specific to the current theme")

(defun my-theme-get-ns-frame-parameters ()
  `((ns-transparent-titlebar . t)
    (ns-appearance . ,(alist-get 'background-mode my-theme-frame-settings))))

(defun my-theme-update-frame-defaults ()
  (pcase-dolist (`(,property . ,value) (my-theme-get-ns-frame-parameters))
    (setf (alist-get property default-frame-alist) value)))

(defun my-theme-update-frame-titlebar (frame &rest _)
  "Based on https://github.com/purcell/ns-auto-titlebar/blob/master/ns-auto-titlebar.el"
  (when-let (display-graphic-p frame)
    (modify-frame-parameters frame (my-theme-get-ns-frame-parameters))))

(defun my-theme-update-all-frame-titlebars ()
  (mapc #'my-theme-update-frame-titlebar (frame-list)))

(defun my-theme-configure-frames ()
  (my-theme-update-all-frame-titlebars)
  (my-theme-update-frame-defaults))

(defun my-modus-themes-setup ()
  (setf (alist-get 'background-mode my-theme-frame-settings)
        (pcase (modus-themes--current-theme)
          ('modus-operandi 'light)
          ('modus-vivendi 'dark)
          (theme (error "'%s' is not a Modus theme" theme))))
  (my-theme-configure-frames))

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
  (add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-setup)
  (modus-themes-load-themes)
  (modus-themes-load-vivendi))

(use-package minions
  :straight t
  :demand t
  :custom (minions-mode-line-lighter "")
  :bind ("C-o =" . minions-minor-modes-menu)
  :init
  (minions-mode 1))

(use-package goggles
  :straight t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(provide 'init-ui)
