;;; -*- lexical-binding: t; -*-

(setq-default indent-tabs-mode nil
              tab-width 4)
(setq sentence-end-double-space nil
      require-final-newline t
      kill-do-not-save-duplicates t
      next-error-find-buffer-function #'next-error-buffer-unnavigated-current)

;; Custom prefixes
(bind-key "C-o" nil)
(bind-key "C-z" nil)
(bind-key "C-\\" nil)
;; Keybindings
(bind-key "M-u" #'upcase-dwim)
(bind-key "M-l" #'downcase-dwim)
(bind-key "M-c" #'capitalize-dwim)
(bind-key "M-t" goto-map)
(bind-key "M-g" #'transpose-words)
(bind-key "s-k" #'kill-current-buffer)
(bind-key "s-c" (cond ((fboundp 'ns-copy-including-secondary) #'ns-copy-including-secondary)
                      (t #'kill-ring-save)))
(bind-key "s-v" #'yank)
(bind-key "s-=" #'text-scale-increase)
(bind-key "s--" #'text-scale-decrease)
(bind-key "s-<" #'beginning-of-buffer)
(bind-key "s->" #'end-of-buffer)
(bind-key "s-," #'xref-go-back)
(bind-key "<home>" #'beginning-of-buffer)
(bind-key "<end>" #'end-of-buffer)
(bind-key "C-o e" #'eval-expression)
(unbind-key "<C-wheel-up>")
(unbind-key "<C-wheel-down>")

(put 'narrow-to-region 'disabled nil)

;; Buffers

(setq my-protected-buffers '("*scratch*" "*Messages*"))

(defun my-kill-buffer-query ()
  "Protect some special buffers from getting killed."
  (not (member (buffer-name (current-buffer)) my-protected-buffers)))

(add-hook 'kill-buffer-query-functions 'my-kill-buffer-query)

;; Backups
(defconst my-backup-directory (expand-file-name "backups/" user-emacs-directory))
(make-directory my-backup-directory t)
(setq make-backup-files t
      vc-make-backup-files t
      backup-by-copying t
      backup-directory-alist (list (cons "." my-backup-directory))
      delete-old-versions t
      kept-new-versions 8
      kept-old-versions 4
      version-control t)

;; Autosaves
(defconst my-autosave-directory
  (expand-file-name "autosave/" user-emacs-directory))
(make-directory my-autosave-directory t)
(setq auto-save-default t
      auto-save-include-big-deletions t
      auto-save-list-file-prefix my-autosave-directory
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

;; Lockfiles
(defconst my-lockfile-directory (expand-file-name "lockfiles/" user-emacs-directory))
(make-directory my-lockfile-directory t)
(setq create-lockfiles t
      lock-file-name-transforms (list (list ".*" my-lockfile-directory t)))

(use-package savehist
  :custom
  (savehist-file (expand-file-name "savehist" user-emacs-directory))
  (savehist-autosave-interval 60)
  (savehist-additional-variables
   '(kill-ring
     register-alist
     mark-ring global-mark-ring
     search-ring regexp-search-ring))
  (savehist-mode)
  :init
  (savehist-mode))

(defun my-recentf-save-list ()
  "Save the recentf file list but don't output a message."
  (let ((save-silently t))
    (recentf-save-list)))

(use-package recentf
  :init
  (setq recentf-auto-cleanup 'never
        recentf-auto-save-timer (run-with-idle-timer 300 t 'my-recentf-save-list)
        recentf-exclude (list (format "\\`%s\\(?:elpa\\|backups\\)/"
                                      (expand-file-name user-emacs-directory))
                              "recentf\\'"
                              "COMMIT_EDITMSG\\'")
        recentf-max-menu-items 15
        recentf-max-saved-items 1000
        recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (recentf-mode))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (unbind-key "C-o" ibuffer-mode-map))

(use-package nerd-icons-ibuffer
  :elpaca t
  :after ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package dired
  :bind (("C-o C-d" . dired-jump)
         :map dired-mode-map
         ("C-o" . nil)
         ("r" . dired-up-directory))
  :custom
  (dired-listing-switches "-aBhl -v --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask)
  (dired-clean-confirm-killing-deleted-buffers nil))

(use-package dired-x
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  :config
  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store\\'"
                "\\|^.project\\(?:ile\\)?\\'"
                "\\|^.\\(svn\\|git\\)\\'"
                "\\|^.ccls-cache\\'"
                "\\|^\\.DS_Store\\'"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")))

(use-package nerd-icons-dired
  :elpaca t
  :after (nerd-icons dired)
  :hook
  (dired-mode . nerd-icons-dired-mode))

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

(defun my-text-mode-setup ()
  (visual-line-mode)
  (remove-hook 'completion-at-point-functions #'ispell-completion-at-point t))

(use-package text-mode
  :hook (text-mode . my-text-mode-setup))

(use-package visual-fill-column
  :elpaca t)

(use-package isearch
  :bind (:map search-map
         ("s" . isearch-forward)
         ("M-s" . isearch-forward)))

(use-package repeat
  :init
  (repeat-mode))

(use-package repeat-help
  :after (repeat embark)
  :elpaca t
  :custom
  (repeat-help-key "C-h h")
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
  (ispell-extra-args '("--sug-mode=ultra"))
  :config
  (dolist (item '((":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
                  ("#\\+BEGIN_SRC" . "#\\+END_SRC")
                  ("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE")))
    (cl-pushnew item ispell-skip-region-alist :test #'equal)))

(use-package jinx
  :elpaca t
  :hook (emacs-startup . global-jinx-mode)
  :bind ("C-;" . jinx-correct))

(use-package smartparens-config
  :elpaca smartparens
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-max-prefix-length 25)
  (sp-max-pair-length 4)
  :init
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

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
  :elpaca t
  :custom
  (undo-limit 400000)                   ; 400kb (default is 160kb)
  (undo-strong-limit 3000000)           ; 3mb   (default is 240kb)
  (undo-outer-limit 48000000)           ; 48mb  (default is 24mb)
  :init
  (define-minor-mode undo-fu-mode
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (keymap-set map "<remap> <undo>" #'undo-fu-only-undo)
              (keymap-set map "<remap> <redo>" #'undo-fu-only-redo)
              (keymap-set map "C-s-/" #'undo-fu-only-redo)
              (keymap-set map "C-_" #'undo-fu-only-undo)
              (keymap-set map "M-_" #'undo-fu-only-redo)
              (keymap-set map "C-M-_" #'undo-fu-only-redo-all)
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

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package flymake
  :bind (nil
         :map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)
         ("C-c y" . flymake-show-buffer-diagnostics)
         :repeat-map my-flymake-mode-repeat-map
         ("n" . flymake-goto-next-error)
         ("p" . flymake-goto-prev-error)))

(use-package tramp
  :preface
  (defconst my-tramp-autosave-directory
    (expand-file-name "tramp-autosave/" user-emacs-directory))
  (make-directory my-tramp-autosave-directory t)
  :custom
  (tramp-auto-save-directory my-tramp-autosave-directory)
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Tree-sitter

(defun my-try-treesit-lang (lang old-mode ts-mode)
  "If tree-sitter is available and the tree-sitter grammar for
 LANG is available, map OLD-MODE to TS-MODE"
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p)
             (treesit-language-available-p lang))
    (push (cons old-mode ts-mode) major-mode-remap-alist)))

;; Formatting

(use-package apheleia
  :elpaca t
  :defer t)

(defcustom my-format-default-function #'apheleia-format-buffer
  "Default format function."
  :type 'function)

(defcustom my-format-lsp-function nil
  "Format function provided by an LSP provider"
  :type 'function)

(defvar my-format-with-lsp t
  "If non-nil, format with LSP formatter if it's available.")

(defun my-format-buffer (&optional arg)
  "If eglot can format and LSP formatting isn't disabled, format the buffer using `eglot-format-buffer'.
 Otherwise, use `apheleia-format-buffer'.

Derived from Doom's format module"
  (interactive "P")
  (call-interactively
   (if (and my-format-lsp-function
            my-format-with-lsp
            (my-lsp-enabled-in-buffer))
       my-format-lsp-function
     my-format-default-function)))

(bind-key "C-o f" #'my-format-buffer)

(provide 'init-editor)
