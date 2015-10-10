;; Core settings

(setq user-full-name "Ryan Kaskel"
      user-mail-address "dev@ryankaskel.com"
      gc-cons-threshold 50000000)

;; Misc
(setq safe-local-variable-values '((do-delete-trailing-whitespace)))

;; C-h behaves like C-h in readline
(define-key my-kbs-map (kbd "C-h") 'delete-backward-char)
(define-key my-kbs-map (kbd "s-h") 'help-command)

;; Backups
(defvar my-backup-directory (expand-file-name "backups/" dotemacs-dir))
(make-directory my-backup-directory t)
(setq make-backup-files t
      vc-make-backup-files t
      backup-by-copying t
      backup-directory-alist `((".*" . ,my-backup-directory))
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 8
      version-control t)

;; Autosaves
(defvar my-autosave-directory (expand-file-name "autosave/" dotemacs-dir))
(make-directory my-autosave-directory t)
(setq auto-save-file-name-transforms
      `((".*" ,my-autosave-directory t)))

(require 'savehist)
(setq savehist-additional-variables '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      savehist-file (expand-file-name "savehist" dotemacs-dir))
(savehist-mode 1)

(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" dotemacs-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      recentf-auto-cleanup 'never)
(recentf-mode 1)

;; dired
(add-hook 'dired-load-hook (function (lambda () (load "dired-x"))))
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
              (seq "~" eol)                 ;; backup-files
              (seq bol "svn" eol)           ;; svn dirs
              (seq ".pyc" eol))))
(setq dired-omit-files-p t
      dired-recursive-deletes 'always
      dired-recursive-copies 'always)

;; Use Google Chrome to open links
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(provide 'init-core)
