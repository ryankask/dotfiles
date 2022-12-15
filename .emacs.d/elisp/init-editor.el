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
(bind-key "M-\"" "€")
(bind-key "s-=" #'text-scale-increase)
(bind-key "s--" #'text-scale-decrease)
(bind-key "C-o e" #'eval-expression)
(bind-key "C-s-." #'xref-go-back)
(unbind-key "<C-wheel-up>")
(unbind-key "<C-wheel-down>")

(put 'narrow-to-region 'disabled nil)

;; Modal editing

(defun my-ryo-modal-theme-setup ()
  (setq ryo-modal-cursor-color (modus-themes-color 'blue-intense)
        ryo-modal-default-cursor-color (face-attribute 'cursor :background)))

(use-package ryo-modal
  :straight t
  :after (modus-themes)
  :bind ("C-M-s-:" . ryo-modal-mode)
  :hook (modus-themes-after-load-theme . my-ryo-modal-theme-setup)
  :config
  (ryo-modal-key "x" ctl-x-map)
  (ryo-modal-keys
   ("." ryo-modal-repeat)
   ("h" "C-b")
   ("n" "C-n")
   ("e" "C-p")
   ("i" "C-f")
   ("w" "M-f")
   ("W" "M-b")
   ("t" "C-M-f")
   ("s" "C-M-b")
   ("v" "C-v")
   ("V" "M-v"))
  (ryo-modal-keys
   (:norepeat t)
   ("0" "M-0")
   ("1" "M-1")
   ("2" "M-2")
   ("3" "M-3")
   ("4" "M-4")
   ("5" "M-5")
   ("6" "M-6")
   ("7" "M-7")
   ("8" "M-8")
   ("9" "M-9"))
  (with-eval-after-load 'which-key-mode
    ;; which-key integration
    (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)))

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
