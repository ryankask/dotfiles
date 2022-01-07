;;; -*- lexical-binding: t; -*-

(setq-default indent-tabs-mode nil
              tab-width 4)
(setq sentence-end-double-space nil
      require-final-newline t
      kill-do-not-save-duplicates t)

(bind-key "M-u" 'upcase-dwim)
(bind-key "M-l" 'downcase-dwim)
(bind-key "M-c" 'capitalize-dwim)
(bind-key "M-g" #'transpose-words)
(bind-key "M-t" goto-map)
(bind-key "M-\"" "€")
(bind-key "s-=" 'text-scale-increase)
(bind-key "s--" 'text-scale-decrease)
(bind-key "C-o e" #'eval-expression)
(unbind-key "<C-wheel-up>")
(unbind-key "<C-wheel-down>")

(put 'narrow-to-region 'disabled nil)

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

(defun my-fill-column-setup ()
  (setq fill-column 88))

(use-package isearch
  :bind (:map search-map
         ("s" . isearch-forward)))

(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode t))

(use-package abbrev
  :diminish abbrev-mode
  :hook (text-mode . abbrev-mode))

(use-package ispell
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--run-together")))

(use-package flyspell
  :diminish flyspell-mode
  :bind (:map flyspell-mode-map
              ("C-." . nil)
              ("C-," . nil)
              ("C-c $" . nil))
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom
  (flyspell-use-meta-tab nil)
  (flyspell-issue-welcome-flag nil)
  (flyspell-issue-message-flag nil))

(use-package flyspell-correct
  :straight t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper)))

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
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-package re-builder
  :custom
  (reb-re-syntax 'string))

(use-package browse-kill-ring
  :straight t
  :bind ("s-y" . browse-kill-ring)
  :init
  (browse-kill-ring-default-keybindings))

(use-package undo-tree
  :straight t
  :diminish undo-tree-mode
  :bind ("C-s-/" . undo-tree-redo)
  :init
  (global-undo-tree-mode))

(provide 'init-editor)
