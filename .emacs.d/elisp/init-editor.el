;;; -*- lexical-binding: t; -*-

(setq-default indent-tabs-mode nil
              tab-width 4)
(setq sentence-end-double-space nil
      require-final-newline t
      kill-do-not-save-duplicates t
      next-error-find-buffer-function #'next-error-buffer-unnavigated-current)

;; Custom prefixes
(bind-key "C-o" nil)
(bind-key "s-h" nil)
(bind-key "s-m" nil)
;; Keybindings
(bind-key "M-u" #'upcase-dwim)
(bind-key "M-l" #'downcase-dwim)
(bind-key "M-c" #'capitalize-dwim)
(bind-key "M-g" #'transpose-words)
(bind-key "M-t" goto-map)
(bind-key "s-=" #'text-scale-increase)
(bind-key "s--" #'text-scale-decrease)
(bind-key "C-o e" #'eval-expression)
(bind-key "C-s-." #'xref-go-back)
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
      version-control t
      tramp-backup-directory-alist backup-directory-alist)

;; Autosaves
(defconst my-autosave-directory
  (expand-file-name "autosave/" user-emacs-directory))
(make-directory my-autosave-directory t)
(defconst my-tramp-autosave-directory
  (expand-file-name "tramp-autosave/" user-emacs-directory))
(make-directory my-tramp-autosave-directory t)
(setq auto-save-default t
      auto-save-list-file-prefix my-autosave-directory
      tramp-auto-save-directory my-tramp-autosave-directory
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

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
  (ispell-extra-args '("--sug-mode=ultra" "--run-together"))
  :config
  (dolist (item '((":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
                  ("#\\+BEGIN_SRC" . "#\\+END_SRC")
                  ("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE")))
    (cl-pushnew item ispell-skip-region-alist :test #'equal)))

(use-package spellcheck
  :bind (("C-c # c" . spellcheck-correct)))

(defvar my-spell-fu-mode-map (make-sparse-keymap)
  "Custom keymap for Spell Fu")

;; Excluded faces Adapted from Doom's configuration:
;; https://github.com/doomemacs/doomemacs/blob/master/modules/checkers/spell/config.el#L85

(defvar my-spell-fu-excluded-faces-alist
  '((markdown-mode
     markdown-code-face markdown-html-attr-name-face
     markdown-html-attr-value-face markdown-html-tag-name-face
     markdown-inline-code-face markdown-link-face
     markdown-markup-face markdown-plain-url-face
     markdown-reference-face markdown-url-face)
    (org-mode
     org-block org-block-begin-line org-block-end-line org-code
     org-cite org-cite-key org-date org-footnote org-formula
     org-latex-and-related org-link org-meta-line org-property-value
     org-ref-cite-face org-special-keyword org-tag org-todo
     org-todo-keyword-done org-todo-keyword-habt
     org-todo-keyword-kill org-todo-keyword-outd
     .org-todo-keyword-todo org-todo-keyword-wait org-verbatim)
    (latex-mode
     font-latex-math-face font-latex-sedate-face
     font-lock-function-name-face font-lock-keyword-face
     font-lock-variable-name-face))
  "Faces in certain major modes that spell-fu will not spellcheck.")

(defun my-spell-fu-mode-setup ()
  "Set `spell-fu-faces-exclude' according to
 `my-spell-fu-excluded-faces-alist'."
  (when-let (excluded (cdr (cl-find-if #'derived-mode-p
                                       my-spell-fu-excluded-faces-alist
                                       :key #'car)))
    (setq-local spell-fu-faces-exclude excluded)))

(use-package spell-fu
  :straight t
  :hook (((text-mode prog-mode yaml-mode conf-mode) . spell-fu-mode)
         (spell-fu-mode . my-spell-fu-mode-setup))
  :bind (nil
         :map my-spell-fu-mode-map
         ("C-c # b" . spell-fu-buffer)
         ("C-c # n" . spell-fu-goto-next-error)
         ("C-c # p" . spell-fu-goto-previous-error)
         ("C-c # a" . spell-fu-word-add)
         ("C-c # d" . spell-fu-word-remove)
         ("C-c # r" . spell-fu-word-reset)
         :repeat-map my-spell-fu-mode-repeat-map
         ("c" . spellcheck-correct)
         ("n" . spell-fu-goto-next-error)
         ("p" . spell-fu-goto-previous-error)
         ("a" . spell-fu-word-add)
         ("d" . spell-fu-word-remove))
  :init
  ;; Add a minor map for the mode
  (cl-pushnew (cons 'spell-fu-mode my-spell-fu-mode-map)
              minor-mode-map-alist
              :test #'equal))

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

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(provide 'init-editor)
