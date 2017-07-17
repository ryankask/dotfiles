;; Core settings

(setq user-full-name "Ryan Kaskel"
      user-mail-address "dev@ryankaskel.com"
      gc-cons-threshold 50000000)

;; Keybindings
(global-unset-key (kbd "C-o"))
(global-unset-key (kbd "s-m"))

;; Local variables
(setq safe-local-variable-values '((do-delete-trailing-whitespace)))

;; C-h behaves like C-h in readline
(bind-key "C-h" 'delete-backward-char)
(bind-key "s-h" 'help-command)

;; Backups
(defconst my-backup-directory (expand-file-name "backups/" dotemacs-dir))
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
(defconst my-autosave-directory (expand-file-name "autosave/" dotemacs-dir))
(make-directory my-autosave-directory t)
(setq auto-save-file-name-transforms
      `((".*" ,my-autosave-directory t)))

(use-package savehist
  :init
  (setq savehist-additional-variables '(search ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "savehist" dotemacs-dir))
  :config
  (savehist-mode 1))

(use-package recentf
  :init
  (setq recentf-auto-cleanup 'never
        recentf-exclude (list (format "\\`%s\\(?:elpa\\|backups\\)/" dotemacs-dir))
        recentf-max-menu-items 15
        recentf-max-saved-items 500
        recentf-save-file (expand-file-name "recentf" dotemacs-dir))
  :config
  (recentf-mode 1))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (unbind-key "C-o" ibuffer-mode-map))

(use-package dired
  :defer t
  :config
  (unbind-key "C-o" dired-mode-map)
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always))

(use-package dired-x
  :after dired
  :config
  (setq-default dired-omit-files-p t)
  (setq dired-omit-files (concat dired-omit-files "\\|\\`__pycache__\\'")))

;; Use Google Chrome to open links
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "open")

(provide 'init-core)
