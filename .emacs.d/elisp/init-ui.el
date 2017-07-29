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

(defun my-solarized-theme-customise ()
  (solarized-with-color-variables 'dark
    (custom-theme-set-faces
     'solarized-dark
     `(ivy-current-match ((,class (:weight bold :background ,base02))))
     `(ivy-subdir ((,class (:foreground ,blue))))
     `(ivy-virtual ((,class (:foreground ,cyan)))))))

(use-package solarized-theme
  :ensure t
  :disabled
  :init
  (setq solarized-use-variable-pitch nil)
  (load-theme 'solarized-dark t)
  (my-solarized-theme-customise))

(defmacro my-doom-one-set-faces (&rest faces)
  `(custom-theme-set-faces
    'doom-one
    ,@(mapcar #'doom-themes--build-face faces)))

(defun my-doom-one-theme-customise ()
  (my-doom-one-set-faces
   ;; ivy
   (ivy-current-match :weight 'bold :background base4)
   (ivy-minibuffer-match-face-2 :foreground magenta)
   (ivy-minibuffer-match-face-3 :foreground green)
   (ivy-minibuffer-match-face-4 :foreground yellow)
   (ivy-subdir :foreground blue)
   (ivy-virtual :foreground teal)))

(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-one t)
  (my-doom-one-theme-customise))

(provide 'init-ui)
