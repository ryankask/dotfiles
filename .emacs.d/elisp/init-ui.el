;;; -*- lexical-binding: t; -*-

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(when (display-graphic-p)
  (setq my-font (font-spec :family "Menlo" :size 13))
  (set-fontset-font t 'unicode my-font)
  (set-fontset-font t 'unicode (font-spec :family "Symbols Nerd Font" :size 13) nil 'append)
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'append)
  (push (cons 'font (font-xlfd-name my-font)) default-frame-alist)
  (push '(width . 102) default-frame-alist)
  (push '(height . 56) default-frame-alist)
  (set-frame-position nil 604 0))

(use-package display-line-numbers
  :custom
  (display-line-numbers-grow-only t)
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode))

;; mode line settings
(column-number-mode t)
(size-indication-mode t)

;; Most of these are from doom-start.el
(setq idle-update-delay 1.0
      inhibit-startup-message t
      use-dialog-box nil
      highlight-nonselected-windows nil
      redisplay-skip-fontification-on-input t
      frame-resize-pixelwise t
      window-resize-pixelwise nil
      initial-scratch-message nil
      use-short-answers t
      confirm-kill-emacs 'yes-or-no-p
      enable-recursive-minibuffers t
      ;; Do not allow the cursor in the minibuffer prompt
      minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(setq-default cursor-in-non-selected-windows nil)

(use-package window
  :bind
  ("s-`" . delete-window)
  ("s-1" . delete-other-windows)
  ("s-2" . split-window-below)
  ("s-3" . split-window-right))

(use-package winner
  :preface (defvar winner-dont-bind-my-keys t)
  :hook (after-init . winner-mode)
  :bind (:map winner-mode-map
              ("C-c [" . winner-undo)
              ("C-c ]" . winner-redo))
  :config
  (setq winner-boring-buffers
        (append winner-boring-buffers
                '("*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
                  "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
                  "*esh command on file*"))))

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

(use-package modus-themes
  :straight t
  :custom
  (modus-themes-bold-constructs nil)
  (modus-themes-italic-constructs t)
  (modus-themes-mode-line '(3d))
  (modus-themes-region '(bg-only))
  (modus-themes-paren-match '(intense))
  (modus-themes-completions '((matches . (background intense))
                              (selection . (intense))
                              (popup . (accented intense))))
  :bind ("C-o w" . modus-themes-toggle)
  :init
  (add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-setup)
  (modus-themes-load-themes)
  (modus-themes-load-vivendi))

(use-package minions
  :straight t
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
