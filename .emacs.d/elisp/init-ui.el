;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'transient)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(when (or (display-graphic-p) (daemonp))
  (setq my-font (font-spec :family "Menlo" :size 13))
  (set-fontset-font t 'unicode my-font)
  (set-fontset-font t 'unicode (font-spec :family "Symbols Nerd Font Mono" :size 13) nil 'append)
  (set-fontset-font t 'unicode "Noto Color Emoji" nil 'append)
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

(defconst my-rectangle-actions
  (list "left-half" "right-half" "center-half" "top-half" "bottom-half"
        "top-left" "top-right" "bottom-left" "bottom-right"
        "first-third" "center-third" "last-third"
        "first-two-thirds" "last-two-thirds"
        "maximize" "almost-maximize" "maximize-height"
        "smaller" "larger" "center" "restore"
        "next-display" "previous-display"
        "move-left" "move-right" "move-up" "move-down"
        "first-fourth" "second-fourth" "third-fourth" "last-fourth"
        "first-three-fourths" "last-three-fourths"
        "top-left-sixth" "top-center-sixth" "top-right-sixth"
        "bottom-left-sixth" "bottom-center-sixth" "bottom-right-sixth"
        "specified" "reverse-all"
        "top-left-ninth" "top-center-ninth" "top-right-ninth"
        "middle-left-ninth" "middle-center-ninth" "middle-right-ninth"
        "bottom-left-ninth" "bottom-center-ninth" "bottom-right-ninth"
        "top-left-third" "top-right-third"
        "bottom-left-third" "bottom-right-third"
        "top-left-eighth" "top-center-left-eighth" "top-center-right-eighth"
        "top-right-eighth" "bottom-left-eighth" "bottom-center-left-eighth"
        "bottom-center-right-eighth" "bottom-right-eighth"
        "tile-all" "cascade-all"))

(defun my-rectangle-exec-action (name)
  "Execute Rectangle app action NAME using the open command. NAME
 must be one the values in `my-rectangle-app-actions'."
  (interactive (list (completing-read "Action: " my-rectangle-app-actions)))
  (call-process
   "open" nil 0 nil "-g" (format "rectangle://execute-action?name=%s" name)))

(dolist (action my-rectangle-actions)
  (defalias (intern (format "my-rectangle-exec-action--%s" action))
    (lambda ()
      (interactive)
      (my-rectangle-exec-action action))
    (format "Execute Rectangle app action `%s'" action)))

(transient-define-prefix my-rectangle-dispatch ()
  "Rectangle app dispatcher"
  [["Halves"
    ("l" "left half" my-rectangle-exec-action--left-half)
    ("r" "right half" my-rectangle-exec-action--right-half)
    ("c" "center half" my-rectangle-exec-action--center-half)
    ("th" "top half" my-rectangle-exec-action--top-half)
    ("bh" "bottom half" my-rectangle-exec-action--bottom-half)]
   ["Corners"
    ("tl" "top left" my-rectangle-exec-action--top-left)
    ("tr" "top right" my-rectangle-exec-action--top-right)
    ("bl" "bottom left" my-rectangle-exec-action--bottom-left)
    ("br" "bottom right" my-rectangle-exec-action--bottom-right)]
   ["Thirds"
    ("3l" "first third" my-rectangle-exec-action--first-third)
    ("3c" "center third" my-rectangle-exec-action--center-third)
    ("3r" "last third" my-rectangle-exec-action--last-third)
    ("," "first two thirds" my-rectangle-exec-action--first-two-thirds)
    ("." "last two thirds" my-rectangle-exec-action--last-two-thirds)]
   ["Edges"
    ("el" "move left" my-rectangle-exec-action--move-left)
    ("er" "move right" my-rectangle-exec-action--move-right)
    ("eu" "move up" my-rectangle-exec-action--move-up)
    ("ed" "move down" my-rectangle-exec-action--move-down)]
   ["Size"
    ("m" "maximise" my-rectangle-exec-action--maximize)
    ("a" "almost maximise" my-rectangle-exec-action--almost-maximize)
    ("u" "maximise height" my-rectangle-exec-action--maximize-height)
    ("-" "smaller" my-rectangle-exec-action--smaller)
    ("+" "larger" my-rectangle-exec-action--larger)
    ("=" "center" my-rectangle-exec-action--center)
    ("v" "restore" my-rectangle-exec-action--restore)]])

(bind-key "s-w" #'my-rectangle-dispatch)

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

(defun my-modus-themes-init ()
  (require 'modus-themes)
  (modus-themes-load-theme 'modus-vivendi))

(use-package modus-themes
  :straight t
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
  :hook ((after-init . my-modus-themes-init)
         (modus-themes-after-load-theme . my-modus-themes-setup)))

(defun my-ef-themes-get-ns-appearance ()
  (cond
   ((memq (ef-themes--current-theme) ef-themes-dark-themes) 'dark)
   (t 'light)))

(defun my-ef-themes-setup ()
  (cl-letf (((symbol-function 'my-theme-get-ns-appearance)
             #'my-ef-themes-get-ns-appearance))
    (my-theme-configure-frames)))

(use-package ef-themes
  :straight t
  :hook (ef-themes-post-load . my-ef-themes-setup))

(use-package minions
  :straight t
  :custom
  (minions-mode-line-lighter "󰍜")
  :bind ("C-c =" . minions-minor-modes-menu)
  :init
  (minions-mode 1))

(use-package goggles
  :straight t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(provide 'init-ui)
