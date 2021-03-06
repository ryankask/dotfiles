;;; -*- lexical-binding: t; -*-

(setq-default indent-tabs-mode nil
              tab-width 4)
(setq tab-always-indent 'complete
      tab-stop-list (number-sequence 4 200 4)
      require-final-newline t
      completion-ignored-extensions
      (append completion-ignored-extensions '(".DS_Store")))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(bind-key "s-=" 'text-scale-increase)
(bind-key "s--" 'text-scale-decrease)
(unbind-key "<C-wheel-up>")
(unbind-key "<C-wheel-down>")
(bind-key "s-\\" "`")
(bind-key "M-\"" "€")

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

(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode t))

(use-package abbrev
  :diminish abbrev-mode
  :hook (text-mode . abbrev-mode))

(use-package flyspell
  :diminish flyspell-mode
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package eldoc
  :diminish eldoc-mode)

(use-package subword
  :diminish subword-mode)

(use-package smartparens-config
  :straight smartparens
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
  :straight t
  :bind ("s-y" . browse-kill-ring)
  :config
  (browse-kill-ring-default-keybindings))

(use-package undo-tree
  :straight t
  :diminish undo-tree-mode
  :bind ("C-s-/" . undo-tree-redo)
  :config
  (global-undo-tree-mode))

(use-package volatile-highlights
  :straight t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(provide 'init-editor)
