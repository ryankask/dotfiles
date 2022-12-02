;;; -*- lexical-binding: t; -*-

(setq-default indent-tabs-mode nil
              tab-width 4)
(setq sentence-end-double-space nil
      require-final-newline t
      kill-do-not-save-duplicates t
      next-error-find-buffer-function #'next-error-buffer-unnavigated-current)

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

(use-package repeat
  :init
  (repeat-mode))

(use-package repeat-help
  :after (repeat embark)
  :straight t
  :custom
  (repeat-help-popup-type 'embark)
  :init
  (repeat-help-mode))

(use-package autorevert
  :config
  (global-auto-revert-mode t))

(use-package abbrev
  :hook (text-mode . abbrev-mode)
  :custom
  (save-abbrevs 'silently))

(use-package ispell
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--run-together")))

(use-package flyspell
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

(use-package smartparens-config
  :straight smartparens
  :custom
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-max-prefix-length 25)
  (sp-max-pair-length 4)
  :init
  (show-paren-mode -1)
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

(use-package undo-fu
  :straight t
  :custom
  (undo-limit 400000)                   ; 400kb (default is 160kb)
  (undo-strong-limit 3000000)           ; 3mb   (default is 240kb)
  (undo-outer-limit 48000000)           ; 48mb  (default is 24mb)
  :init
  (define-minor-mode undo-fu-mode
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap redo] #'undo-fu-only-redo)
              (define-key map (kbd "C-s-/") #'undo-fu-only-redo)
              (define-key map (kbd "C-_") #'undo-fu-only-undo)
              (define-key map (kbd "M-_") #'undo-fu-only-redo)
              (define-key map (kbd "C-M-_") #'undo-fu-only-redo-all)
              map)
    :init-value nil
    :global t)
  (undo-fu-mode))

;;; From crux.el: https://github.com/bbatsov/crux/blob/master/crux.el

(defun crux-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (insert "\n")
  (if electric-indent-inhibit
      ;; We can't use `indent-according-to-mode' in languages like Python,
      ;; as there are multiple possible indentations with different meanings.
      (let* ((indent-end (progn (crux-move-to-mode-line-start) (point)))
             (indent-start (progn (move-beginning-of-line nil) (point)))
             (indent-chars (buffer-substring indent-start indent-end)))
        (forward-line -1)
        ;; This new line should be indented with the same characters as
        ;; the current line.
        (insert indent-chars))
    ;; Just use the current major-mode's indent facility.
    (forward-line -1)
    (indent-according-to-mode)))

(bind-key "s-i" 'crux-smart-open-line-above)

(provide 'init-editor)
