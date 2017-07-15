(menu-bar-mode -1)

(when (display-graphic-p)
  (add-to-list 'default-frame-alist (cons 'width 120))
  (add-to-list 'default-frame-alist (cons 'height (/ (- (x-display-pixel-height) 100)
                                                     (frame-char-height))))
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (use-package nlinum
    :ensure t
    :config
    (global-nlinum-mode 1)))

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t
      use-dialog-box nil
      initial-scratch-message nil
      show-paren-mode t)

(use-package column-marker
  :ensure t
  :defer t)

(defun my-solarized-theme-swap ()
  "If solarized-dark is the current theme, switch to the light version, and vice versa."
  (interactive)
  (if (eql (position 'solarized-dark custom-enabled-themes) 0)
      (load-theme 'solarized-light t)
    (load-theme 'solarized-dark t)))

(defun my-solarized-theme-customise ()
  (solarized-with-color-variables 'dark
    (custom-theme-set-faces
     'solarized-dark
     `(ivy-current-match ((,class (:weight bold :background ,base02))))
     `(ivy-subdir ((,class (:foreground ,blue))))
     `(ivy-virtual ((,class (:foreground ,cyan)))))))

(use-package solarized-theme
  :ensure t
  :bind ("C-c w" . my-solarized-theme-swap)
  :init
  (setq solarized-use-variable-pitch nil)
  (load-theme 'solarized-dark t)
  (my-solarized-theme-customise))

(provide 'init-ui)
