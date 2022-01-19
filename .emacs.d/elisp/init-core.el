;;; Core settings -*- lexical-binding: t; -*-

(setq user-full-name "Ryan Kaskel"
      user-mail-address "dev@ryankaskel.com"
      read-process-output-max (* 1024 1024))

;; Custom prefixes

(bind-key "C-o" nil)
(bind-key "s-h" nil)
(bind-key "s-m" nil)

;; Customisations

(setq custom-file (expand-file-name "elisp/custom.el" user-emacs-directory))

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
      backup-directory-alist `((".*" . ,my-backup-directory))
      delete-old-versions t
      kept-new-versions 8
      kept-old-versions 4
      version-control t)

;; Autosaves
(defconst my-autosave-directory (expand-file-name "autosave/" user-emacs-directory))
(make-directory my-autosave-directory t)
(setq auto-save-file-name-transforms
      `((".*" ,my-autosave-directory t)))

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
         ("[" . dired-up-directory))
  :custom
  (dired-listing-switches "-aBhl -v --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask))

(use-package dired-x
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  (dired-clean-confirm-killing-deleted-buffers nil)
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

(provide 'init-core)
