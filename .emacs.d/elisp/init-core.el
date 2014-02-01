;; Core settings

(setq user-full-name "Ryan Kaskel"
      user-mail-address "dev@ryankaskel.com"
      tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)
      text-mode-hook '(turn-on-auto-fill text-mode-hook-identify)
      gc-cons-threshold 50000000
      require-final-newline t)

;; Text encoding
(define-coding-system-alias 'UTF-8 'utf-8)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq utf-translate-cjk-mode nil)


;; Misc
(setq safe-local-variable-values '((do-delete-trailing-whitespace)))


;; C-h behaves like C-h in readline
(define-key my-kbs-map (kbd "C-h") 'delete-backward-char)
(define-key my-kbs-map (kbd "s-h") 'help-command)


(defun quit-other-window ()
  "Quits the other window. Equivalent of C-x o q"
  (interactive)
  (quit-window (other-window 1)))
(define-key my-kbs-map (kbd "C-x 4 q") 'quit-other-window)


;; Save history
(savehist-mode 1)
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 4
      version-control t)

;; dired
(add-hook 'dired-load-hook (function (lambda () (load "dired-x"))))
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
              (seq "~" eol)                 ;; backup-files
              (seq bol "svn" eol)           ;; svn dirs
              (seq ".pyc" eol))))
(setq dired-omit-files-p t)


;; Use Google Chrome to open links
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(provide 'init-core)
