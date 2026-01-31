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
(bind-key "s-s" #'scratch-buffer)
(bind-key "s-v" #'yank)
(bind-key "s-=" #'text-scale-increase)
(bind-key "s--" #'text-scale-decrease)
(bind-key "s-<" #'beginning-of-buffer)
(bind-key "s->" #'end-of-buffer)
(bind-key "s-," #'xref-go-back)
(bind-key "<home>" #'beginning-of-buffer)
(bind-key "<end>" #'end-of-buffer)
(bind-key "C-o e" #'eval-expression)
(bind-key "C-o p" #'elpaca-fetch-all)
(unbind-key "<C-wheel-up>")
(unbind-key "<C-wheel-down>")
(dotimes (i 12)
  (unbind-key (format "<f%d>" (+ i 1))))

(put 'narrow-to-region 'disabled nil)

(defun my-keyboard-quit (&optional interactive)
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

Adapted from crux and doom"
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond
     ;; quit the minibuffer if open.
     ((minibuffer-window-active-p (minibuffer-window))
      (when interactive
        (setq this-command 'abort-recursive-edit))
      (abort-recursive-edit))
     ;; don't abort macros
     ((or defining-kbd-macro executing-kbd-macro)
      nil)
     ;; default
     (t
      (unwind-protect (keyboard-quit)
        (when interactive
          (setq this-command 'keyboard-quit)))))))

(bind-key [remap keyboard-quit] #'my-keyboard-quit)

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
  :hook ((elpaca-after-init . recentf-mode)
         (kill-emacs . recentf-cleanup))
  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"
                         (format "\\`%s\\(?:elpa\\|backups\\)/"
                                 (expand-file-name user-emacs-directory))
                         "recentf\\'"
                         "COMMIT_EDITMSG\\'"))
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 1000)
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory)))

(use-package ibuffer
  :hook (ibuffer-mode . hl-line-mode)
  :bind ("C-x C-b" . ibuffer)
  :config
  (unbind-key "C-o" ibuffer-mode-map))

(use-package nerd-icons-ibuffer
  :ensure t
  :after ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package dired
  :hook (dired-mode . hl-line-mode)
  :bind (("C-o C-d" . dired-jump)
         :map dired-mode-map
         ("C-o" . nil)
         ("r" . dired-up-directory))
  :custom
  (delete-by-moving-to-trash t)
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
        (concat "\\`[.]?#\\|\\`[.][.]?\\'"
                "\\|^.\\(?:_.+\\|DS_Store\\)\\'"
                "\\|^.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:|svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind (nil
         :map dired-mode-map
         ("e" . dired-subtree-toggle)
         ("E" . dired-subtree-remove))
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package nerd-icons-dired
  :ensure t
  :after (nerd-icons dired)
  :hook
  (dired-mode . nerd-icons-dired-mode)
  :init
  (with-eval-after-load 'dired-subtree
    (advice-add 'dired-subtree-remove :around #'nerd-icons-dired--refresh-advice)))

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
  (visual-line-mode))

(use-package text-mode
  :hook (text-mode . my-text-mode-setup)
  :custom
  (text-mode-ispell-word-completion nil))

(use-package visual-fill-column
  :ensure t)

(use-package isearch
  :bind (:map search-map
         ("s" . isearch-forward)
         ("M-s" . isearch-forward)))

(use-package repeat
  :init
  (repeat-mode))

(use-package repeat-help
  :after (repeat embark)
  :ensure t
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
  (save-abbrevs 'silently)
  :config
  (defun my-abbrev-get (symbol propname)
    "Always return 0 for the `:count' PROPNAME, otherwise get the value.
This allows abbrev_defs to be saved in a VCS without constantly changing."
    (cond
     ((eq propname :count) 0)
     (t (get symbol propname))))

  (defalias 'abbrev-get #'my-abbrev-get))

(use-package ispell
  :defer t
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra"))
  :config
  (dolist (item '((":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
                  ("#\\+BEGIN_SRC" . "#\\+END_SRC")
                  ("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE")))
    (cl-pushnew item ispell-skip-region-alist :test #'equal)))

(use-package jinx
  :ensure t
  :after repeat
  :hook (emacs-startup . global-jinx-mode)
  :bind (nil
         ("C-;" . jinx-correct)
         ("M-$" . jinx-correct)
         :map jinx-correct-map
         ("C-;" . jinx-previous)))

(use-package elec-pair
  :hook (elpaca-after-init . electric-pair-mode))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-package re-builder
  :defer t
  :custom
  (reb-re-syntax 'string))

(use-package undo-fu
  :ensure t
  :hook (elpaca-after-init . undo-fu-mode)
  :custom
  (undo-limit 256000)                   ; 256kb (default is 160kb)
  (undo-strong-limit 2000000)           ; 2mb   (default is 240kb)
  (undo-outer-limit 36000000)           ; 36mb  (default is 24mb)
  :config
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
    :global t))

(use-package vundo
  :ensure t
  :bind ("C-c z" . vundo))

(defun my-insert-newline-above (n)
  "Insert a newline before the current line without moving the cursor.
With arg N, insert N newlines."
  (interactive "*p")
  (save-excursion
    (beginning-of-line)
    (newline n)))

(bind-key "s-Y" #'my-insert-newline-above)

(defun my-insert-newline-below (n)
  "Insert a newline after the current line without moving the cursor.
With arg N, insert N newlines."
  (interactive "*p")
  (save-excursion
    (end-of-line)
    (newline n)))

(bind-key "s-y" #'my-insert-newline-below)

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
  :preface
  (defun my-toggle-eldoc-doc-buffer ()
    "Toggle display of the ElDoc documentation buffer"
    (interactive)
    (if-let* ((win (get-buffer-window eldoc--doc-buffer)))
        (quit-window nil win)
      (eldoc-doc-buffer t)))
  :bind ("s-\\" . my-toggle-eldoc-doc-buffer)
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
  :defer t
  :custom
  (remote-file-name-inhibit-cache 60)
  (remote-file-name-inhibit-locks t)
  (tramp-auto-save-directory my-tramp-autosave-directory)
  (tramp-completion-reread-directory-timeout 60)
  (tramp-copy-size-limit (* 1024 1024))
  (tramp-default-method "ssh")
  (tramp-verbose 1)
  (vc-ignore-dir-regexp (format "%s\\|%s"
                                vc-ignore-dir-regexp
                                tramp-file-name-regexp))
  :config
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process)

  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package compile
  :hook (compilation-filter . ansi-color-compilation-filter)
  :bind ("C-o C-c" . recompile))

;; Tree-sitter

(defun my-try-treesit-lang (lang old-mode ts-mode)
  "If tree-sitter is available and the tree-sitter grammar for
 LANG is available, map OLD-MODE to TS-MODE"
  (when (and (fboundp 'treesit-available-p)
             (treesit-available-p)
             (treesit-language-available-p lang))
    (push (cons old-mode ts-mode) major-mode-remap-alist)))

(defun my-treesit-update-lanague-grammars ()
  "Upgrade the language grammars for all items in
 `treesit-language-source-alist'"
  (interactive)
  (dolist (item treesit-language-source-alist)
    (treesit-install-language-grammar (car item))))

(use-package treesit
  :init
  (setq treesit-language-source-alist
        '((css "https://github.com/tree-sitter/tree-sitter-css")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (hcl "https://github.com/tree-sitter-grammars/tree-sitter-hcl")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
          (make "https://github.com/tree-sitter-grammars/tree-sitter-make")
          (mermaid "https://github.com/monaqa/tree-sitter-mermaid")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
          (typst "https://github.com/uben0/tree-sitter-typst")
          (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml"))))

;; Formatting

(use-package apheleia
  :ensure t
  :defer t
  :custom
  (apheleia-remote-algorithm 'local)
  :config
  (setf (alist-get 'sqlfluff apheleia-formatters)
        '("sqlfluff" "format" "--stdin-filename" filepath "-"))
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff)
        (alist-get 'sql-mode apheleia-mode-alist) '(sqlfluff)))

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
