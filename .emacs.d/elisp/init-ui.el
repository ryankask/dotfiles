;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(unless noninteractive
  (set-face-attribute 'default nil :family "Iosevka" :height 140)
  (set-face-attribute 'fixed-pitch nil :family "Iosevka")
  (set-face-attribute 'fixed-pitch-serif nil :family "Iosevka Slab")
  (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
  (set-fontset-font t 'unicode "Symbols Nerd Font Mono" nil 'append)
  (set-fontset-font t 'emoji "Noto Color Emoji" nil 'append)
  (push '(width . (text-pixels . 700)) default-frame-alist)
  (push '(height . (text-pixels . 843)) default-frame-alist)
  (set-frame-position nil 720 0))

;; mode line settings
(column-number-mode t)
(size-indication-mode t)

;; Most of these are from doom-ui.el
(setq confirm-kill-emacs 'yes-or-no-p
      ring-bell-function #'ignore
      visible-bell nil
      frame-resize-pixelwise t
      window-resize-pixelwise nil
      use-dialog-box nil
      enable-recursive-minibuffers t
      use-short-answers t
      ;; Do not allow the cursor in the minibuffer prompt
      minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(defun my-toggle-window-dedication ()
  "Toggles window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p
   (selected-window)
   (not (window-dedicated-p (selected-window)))))

(use-package window
  :bind
  ("s-`" . delete-window)
  ("s-1" . delete-other-windows)
  ("s-2" . split-window-below)
  ("s-3" . split-window-right)
  ("s-w d" . my-toggle-window-dedication))

(use-package windmove
  :bind (("C-s-h" . windmove-left)
         ("C-s-n" . windmove-down)
         ("C-s-e" . windmove-up)
         ("C-s-i" . windmove-right)
         ("s-w h" . windmove-left)
         ("s-w n" . windmove-down)
         ("s-w e" . windmove-up)
         ("s-w i" . windmove-right)
         :repeat-map my-windmove-repeat-map
         ("h" . windmove-left)
         ("n" . windmove-down)
         ("e" . windmove-up)
         ("i" . windmove-right)))

(use-package tab-bar
  :hook (elpaca-after-init . tab-bar-history-mode)
  :custom
  (tab-bar-history-limit 100)
  :bind (("M-s-ESC" . tab-bar-history-back)
         ("C-M-s-]" . tab-bar-history-forward)
         ("C-c <left>" . nil)
         ("C-c <right>" . nil)))

(use-package display-line-numbers
  :custom
  (display-line-numbers-grow-only t)
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode))

(defun my-theme-get-ns-appearance ()
  (pcase (modus-themes--current-theme)
    ('modus-operandi 'light)
    ('modus-vivendi 'dark)
    (theme (error "'%s' is not a Modus theme" theme))))

(defun my-theme-get-ns-frame-parameters ()
  `((ns-transparent-titlebar . t)
    (ns-appearance . ,(my-theme-get-ns-appearance))))

(defun my-theme-update-frame-defaults (frame-parameters)
  (pcase-dolist (`(,property . ,value) frame-parameters)
    (setf (alist-get property default-frame-alist) value)))

(defun my-theme-update-all-frame-titlebars (frame-parameters)
  (mapc (lambda (frame)
          (when (display-graphic-p frame)
            (modify-frame-parameters frame frame-parameters)))
        (frame-list)))

(defun my-theme-configure-frames ()
  (let ((frame-parameters (my-theme-get-ns-frame-parameters)))
    (my-theme-update-all-frame-titlebars frame-parameters)
    (my-theme-update-frame-defaults frame-parameters)))

(defun my-modus-themes-setup ()
  (my-theme-configure-frames))

(defun my-modus-themes-init ()
  (require 'modus-themes)
  (modus-themes-load-theme 'modus-operandi))

(use-package modus-themes
  :ensure (:host github :repo "protesilaos/modus-themes")
  :custom
  (modus-themes-custom-auto-reload nil)
  (modus-themes-bold-constructs nil)
  (modus-themes-italic-constructs t)
  (modus-themes-common-palette-overrides
   `((bg-region bg-lavender)
     (fg-region unspecified)
     (bg-paren-match bg-magenta-intense)
     (border-mode-line-active bg-mode-line-active)
     (border-mode-line-inactive bg-mode-line-inactive)))
  :bind ("C-o w" . modus-themes-toggle)
  :hook ((elpaca-after-init . my-modus-themes-init)
         (modus-themes-after-load-theme . my-modus-themes-setup)))

(defun my-ef-themes-get-ns-appearance ()
  (cond
   ((memq (ef-themes--current-theme) ef-themes-dark-themes) 'dark)
   (t 'light)))

(defun my-ef-themes-setup ()
  (cl-letf (((symbol-function 'my-theme-get-ns-appearance)
             #'my-ef-themes-get-ns-appearance))
    (my-theme-configure-frames)))

(defun my-ef-themes-init ()
  (require 'ef-themes)
  (ef-themes-select 'ef-dream))

(use-package ef-themes
  :ensure (:host github :repo "protesilaos/ef-themes")
  :hook ((ef-themes-post-load . my-ef-themes-setup)))

(defcustom my-theme-update-external-themes t
  "Determines whether changing a modus-theme or ef-themes theme also
updates other software's themes like kitty."
  :type 'boolean)

(defvar my-theme-update-external-themes-ready nil)

(defun my-theme-match-current-theme (current-theme)
  (if (null my-theme-update-external-themes-ready)
      (setq my-theme-update-external-themes-ready t)
    (when my-theme-update-external-themes
      (themegen-activate-kitty-theme current-theme))))

(defun my-theme-match-current-modus-theme ()
  (my-theme-match-current-theme (modus-themes--current-theme)))

(defun my-theme-match-current-ef-theme ()
  (my-theme-match-current-theme (ef-themes--current-theme)))

(use-package themegen
  :commands themegen-activate-kitty-theme
  :hook ((modus-themes-after-load-theme . my-theme-match-current-modus-theme)
         (ef-themes-post-load . my-theme-match-current-ef-theme)))

(use-package which-key
  :custom
  (which-key-use-C-h-commands nil)
  :init
  (which-key-mode))

(use-package transient
  :ensure t)

(use-package minions
  :ensure t
  :custom
  (minions-mode-line-lighter "󰍜")
  :bind ("C-c =" . minions-minor-modes-menu)
  :init
  (minions-mode 1))

(use-package goggles
  :ensure t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package posframe
  :ensure t)

(provide 'init-ui)
