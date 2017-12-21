;;; -*- lexical-binding: t; -*-

(global-auto-revert-mode t)

(setq-default indent-tabs-mode nil
              tab-width 4)
(setq tab-always-indent 'complete
      tab-stop-list (number-sequence 4 200 4)
      require-final-newline t
      completion-ignored-extensions
      (append completion-ignored-extensions '(".DS_Store")))

(add-hook 'text-mode-hook 'abbrev-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(bind-key "s-=" 'text-scale-increase)
(bind-key "s--" 'text-scale-decrease)
(bind-key "s-\\" "`")

;; get rid of trailing whitespace
(defcustom my-should-delete-trailing-whitespace t
  "Should trailing whitespace be deleted from files."
  :type 'boolean
  :safe 'booleanp)

(defun my-delete-trailing-whitespace ()
  "Delete trailing whitespace in certain conditions."
  (unless (or (eq major-mode 'org-mode) (not my-should-delete-trailing-whitespace))
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'my-delete-trailing-whitespace)

(use-package subword
  :diminish subword-mode)

(use-package smartparens-config
  :ensure smartparens
  :diminish smartparens-mode
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
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
