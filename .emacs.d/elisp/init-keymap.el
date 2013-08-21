(defvar my-kbs-map (make-keymap) "My custom keybindings.")

(define-minor-mode my-kbs-minor-mode
  "A minor mode for my custom keybindings."
  t           ;; Enable by default
  " my-kbs"   ;; name in mode line
  my-kbs-map)

;; Turn off in the minibuffer
(add-hook 'minibuffer-setup-hook (lambda () (my-kbs-minor-mode 0)))

(provide 'init-keymap)
