(global-auto-revert-mode t)

(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil
              tab-width 4)

(add-hook 'text-mode-hook 'abbrev-mode)

(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(sp-use-smartparens-bindings)
(setq sp-autoescape-string-quote nil)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
(setq uniquify-ignore-buffers-re "^\\*")

(require 'midnight)

(require 're-builder)
(setq reb-re-syntax 'string)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(define-key my-kbs-map (kbd "s-y") 'browse-kill-ring)

;; get rid of trailing whitespace
(defvar do-delete-trailing-whitespace t)
(add-hook 'before-save-hook
          (lambda()
            (unless (or (eq major-mode 'org-mode) (not do-delete-trailing-whitespace))
              (delete-trailing-whitespace))))

(define-key my-kbs-map (kbd "C-s-/") 'undo-tree-redo)
(global-undo-tree-mode)

(require 'volatile-highlights)
(volatile-highlights-mode t)

(provide 'init-editor)
