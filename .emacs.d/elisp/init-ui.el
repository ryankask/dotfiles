;;; -*- lexical-binding: t; -*-

(menu-bar-mode -1)

(when (display-graphic-p)
  (add-to-list 'default-frame-alist (cons 'width 120))
  (add-to-list 'default-frame-alist (cons 'height (/ (- (x-display-pixel-height) 100)
                                                     (frame-char-height))))
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (if (version< emacs-version "26")
      (use-package nlinum
        :ensure t
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
      show-paren-mode t)

(use-package column-marker
  :ensure t
  :defer t)

(defun my-solarized-theme-customise ()
  (solarized-with-color-variables 'dark
    (custom-theme-set-faces
     'solarized-dark
     `(ivy-current-match ((,class (:weight bold :background ,base02))))
     `(ivy-subdir ((,class (:foreground ,blue))))
     `(ivy-virtual ((,class (:foreground ,cyan)))))))

(use-package solarized-theme
  :disabled
  :ensure t
  :init
  (setq solarized-use-variable-pitch nil)
  (load-theme 'solarized-dark t)
  (my-solarized-theme-customise))

(defmacro my-doom-theme-set-faces (theme-name &rest faces)
  `(custom-theme-set-faces
    ,theme-name
    ,@(mapcar #'doom-themes--build-face faces)))

(defun my-doom-one-theme-customise ()
  (my-doom-theme-set-faces
   'doom-one
   ;; base
   (cursor :background fg)
   (minibuffer-prompt :foreground fg)
   (font-lock-variable-name-face :foreground fg)
   (line-number-current-line :inherit 'line-number)
   ;; dired
   (dired-directory :foreground blue)
   ;; avy
   (avy-goto-char-timer-face :background blue :foreground base0)
   (avy-lead-face :background yellow :foreground base0)
   (avy-lead-face-0 :background red :foreground base0)
   (avy-lead-face-1 :background orange :foreground base0)
   (avy-lead-face-2 :background violet :foreground base0)
   ;; column-marker
   (column-marker-1 :background dark-blue)
   (column-marker-2 :background blue)
   ;; ivy
   (ivy-subdir :foreground blue)
   (ivy-virtual :foreground (doom-darken fg 0.15))
   ;; org
   (org-level-1 :foreground blue :background base3 :bold t :height 1.0)))

(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-one t)
  (my-doom-one-theme-customise))

(provide 'init-ui)
