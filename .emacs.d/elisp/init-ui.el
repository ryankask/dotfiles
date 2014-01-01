(defun get-height-for-display ()
  "Calculate a height a bit smaller than the height of the maximum height of the display. TODO: Make this work with emacsclient."
  (if (display-graphic-p)
      (/ (- (x-display-pixel-height) 100)
         (frame-char-height))
    50))

;; (add-to-list 'default-frame-alist (cons 'height  (get-height-for-display)))
;; (add-to-list 'default-frame-alist (cons 'width 90))

(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

;; no graphic dialogs
(setq use-dialog-box nil)
;; Don't insert instructions in the *scratch* buffer
(setq initial-scratch-message nil)
;; No beep in OS X
(if (eq system-type 'darwin)
    (setq ring-bell-function 'ignore))

;; Set color theme
(load-theme 'solarized-dark t) ;; This theme doesn't work with emacsclient

(defun solarized-theme-swap ()
  "If solarized-dark is the current theme, switch to the light version, and vice versa."
  (interactive)
  (if (eql (position 'solarized-dark custom-enabled-themes) 0)
    (load-theme 'solarized-light t)
  (load-theme 'solarized-dark t)))
(define-key my-kbs-map (kbd "C-c w") 'solarized-theme-swap)

(provide 'init-ui)
