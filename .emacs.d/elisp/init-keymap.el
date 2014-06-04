(defvar my-kbs-map (make-sparse-keymap) "My custom keybindings.")

(define-prefix-command 'my-ctl-o-map)
(define-key my-kbs-map (kbd "C-o") 'my-ctl-o-map)

(define-minor-mode my-kbs-minor-mode
  "A minor mode for my custom keybindings."
  t           ;; Enable by default
  " K"        ;; name in mode line
  my-kbs-map)

;; Turn off in the minibuffer
(add-hook 'minibuffer-setup-hook (lambda () (my-kbs-minor-mode 0)))

(provide 'init-keymap)
