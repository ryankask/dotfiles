(global-auto-revert-mode t)

(setq tab-always-indent 'complete)

(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(sp-use-smartparens-bindings)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
(setq uniquify-ignore-buffers-re "^\\*")

(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

(require 'midnight)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "s-y") 'browse-kill-ring)

;; get rid of trailing whitespace
(defvar do-delete-trailing-whitespace t)
(add-hook 'before-save-hook
          (lambda()
            (unless (or (eq major-mode 'org-mode) (not do-delete-trailing-whitespace))
              (delete-trailing-whitespace))))

(provide 'init-editor)
