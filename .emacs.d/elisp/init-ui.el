(defun get-height-for-display ()
  (if (display-graphic-p)
      (/ (- (x-display-pixel-height) 100)
         (frame-char-height))
    50))

(add-to-list 'default-frame-alist (cons 'height  (get-height-for-display)))
(add-to-list 'default-frame-alist (cons 'width 90))

(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (global-linum-mode 1))

(setq show-paren-mode t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
;; Don't show the start up screen
(setq inhibit-startup-message t)
;; no graphic dialogs
(setq use-dialog-box nil)
;; Don't insert instructions in the *scratch* buffer
(setq initial-scratch-message nil)
;; No beep in OS X
(when (eq system-type 'darwin)
  (setq ring-bell-function 'ignore)
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(define-key my-kbs-map (kbd "C-o o") 'ace-window)

;; Set color theme
(setq solarized-use-variable-pitch nil)
(load-theme 'solarized-dark t) ;; This theme doesn't work with emacsclient

(defun solarized-theme-swap ()
  "If solarized-dark is the current theme, switch to the light version, and vice versa."
  (interactive)
  (if (eql (position 'solarized-dark custom-enabled-themes) 0)
    (load-theme 'solarized-light t)
  (load-theme 'solarized-dark t)))
(define-key my-kbs-map (kbd "C-c w") 'solarized-theme-swap)

(provide 'init-ui)
