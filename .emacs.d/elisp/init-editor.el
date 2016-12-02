(global-auto-revert-mode t)

(setq tab-always-indent 'complete
      tab-stop-list (number-sequence 4 200 4)
      require-final-newline t
      text-mode-hook '(turn-on-auto-fill
                       text-mode-hook-identify
                       abbrev-mode))
(setq-default indent-tabs-mode nil
              tab-width 4)
(setq completion-ignored-extensions
      (append completion-ignored-extensions
              '(".DS_Store")))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

;; get rid of trailing whitespace
(defvar do-delete-trailing-whitespace t)
(add-hook 'before-save-hook
          (lambda()
            (unless (or (eq major-mode 'org-mode) (not do-delete-trailing-whitespace))
              (delete-trailing-whitespace))))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (sp-use-smartparens-bindings)
  (setq sp-autoescape-string-quote nil))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(use-package midnight)

(use-package re-builder
  :init
  (setq reb-re-syntax 'string))

(use-package browse-kill-ring
  :ensure t
  :bind ("s-y" . browse-kill-ring)
  :config
  (browse-kill-ring-default-keybindings))

(use-package undo-tree
  :ensure
  :diminish undo-tree-mode
  :bind ("C-s-/" . undo-tree-redo)
  :config
  (global-undo-tree-mode))

(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(provide 'init-editor)
