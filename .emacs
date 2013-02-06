;; Ryan Kaskel's .emacs custom configuration


;; User info
(setq user-full-name "Ryan Kaskel")
(setq user-mail-address "dev@ryankaskel.com")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-linum-mode 1)
 '(indent-tabs-mode nil)
 '(locale-coding-system (quote utf-8) t)
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(prefer-coding-system (quote utf-8))
 '(set-default-coding-system (quote utf-8))
 '(set-keyboard-coding-system (quote utf-8))
 '(set-language-environment (quote utf-8))
 '(set-selection-coding-system (quote utf-8))
 '(set-terminal-coding-system (quote utf-8))
 '(show-paren-mode t)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)))
 '(tab-width 4)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(utf-translate-cjk-mode nil))


;; Start emacs with 86x40 default size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 86))

(menu-bar-mode 0)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0)
  (scroll-bar-mode -1))

;; no graphic dialogs
(setq use-dialog-box nil)
;; Don't insert instructions in the *scratch* buffer
(setq initial-scratch-message nil)


;; Alias "utf-8" as "UTF-8"
(define-coding-system-alias 'UTF-8 'utf-8)


;; My keyboard customizations -- set up the minor mode's key map
(defvar my-kbs-map (make-keymap) "My custom keybindings.")


;; C-h behaves like C-h in readline
(define-key my-kbs-map (kbd "C-h") 'delete-backward-char)
(define-key my-kbs-map (kbd "s-h") 'help-command)


;; recentf - a list of recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(define-key my-kbs-map (kbd "C-x C-r") 'recentf-open-files)


;; Packages
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)


;; The auto installation from emacs prelude. See http://stackoverflow.com/a/10102154
(setq url-http-attempt-keepalives nil)

(defvar my-packages
  '(ac-nrepl ace-jump-mode auto-complete browse-kill-ring
             clojure-mode col-highlight column-marker csharp-mode
             crosshairs flymake-cursor go-mode hl-line+ lua-mode
             markdown-mode nrepl org popup python rainbow-mode
             scss-mode smex solarized-theme vline yasnippet)
  "A list of packages that must be installed.")

(defun my-packages-installed-p ()
  (loop for my-package in my-packages
        when (not (package-installed-p my-package)) do (return nil)
        finally (return t)))

(require 'cl)
(unless (my-packages-installed-p)
  (message "%s" "Refreshing package list...")
  (package-refresh-contents)
  (message "%s" " done.")
  (dolist (my-package my-packages)
    (when (not (package-installed-p my-package))
      (package-install my-package))))


;; Random customizations
;; Use "y or n" answers instead of full words "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)
;; Dont show the GNU splash screen
(setq inhibit-startup-message t)
;; will make the last line end in a carriage return.
(setq require-final-newline t)
;; get rid of trailing whitespace
(add-hook 'before-save-hook
          (lambda()
            (unless (eq major-mode 'org-mode)
              (delete-trailing-whitespace))))


;; Save history and put backups in ~/.emacs_backups
(require 'savehist)
(savehist-load)
(setq
   backup-by-copying t
   backup-directory-alist
    '(("." . "~/.emacs_backups"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
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


;; auto-complete  - http://cx4a.org/software/auto-complete/
(require 'auto-complete-config)
(ac-config-default)


;; Column marker - http://www.emacswiki.org/emacs/ColumnMarker
(require 'column-marker)
;; Highlight the 80th column in python-mode
(add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 80)))


;; Python Settings
(autoload 'python "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-hook 'python-mode-hook
          (lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))


(defun python-debug-insert-ipdb-set-trace ()
  "Insert ipdb trace call into buffer."
  (interactive)
  (insert "import ipdb; ipdb.set_trace()"))

(define-key my-kbs-map (kbd "C-c /") 'python-debug-insert-ipdb-set-trace)

;; PyFlakes
;; see http://www.emacswiki.org/emacs/PythonProgrammingInEmacs#toc7

(defun flymake-create-temp-in-system-tempdir (filename prefix)
  (make-temp-file (or prefix "flymake")))

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
     ; Make sure it's not a remote buffer or flymake would not work
     (when (if (fboundp 'tramp-list-remote-buffers)
               (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
             t)
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-in-system-tempdir)))
         (list "~/bin/my_pyflakes.py" (list temp-file)))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'python-mode-hook 'flymake-mode)

;; End Python Settings


;; Show flymake errors in the minibuffer
(load-library "flymake-cursor")


;; Don't use auto-fill-mode in html-mode
(add-hook 'html-mode-hook (lambda ()
                            (turn-off-auto-fill)
                            (set-fill-column 100)))


;; Lua mode: http://immerrr.github.com/lua-mode/
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


;; rst-mode
(add-hook 'rst-mode-hook (lambda () (set-fill-column 80)))

;; org-mode
(define-key my-kbs-map (kbd "C-c l") 'org-store-link)
(define-key my-kbs-map (kbd "C-c a") 'org-agenda)
(define-key my-kbs-map (kbd "C-c M-d") 'org-date-from-calendar)
(setq org-log-done t)
(setq org-completion-use-ido t)
(defun org-summary-todo (n-done n-not-don)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states) ; turn off logging
    (org-todo (if (= n not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
;; Set alternative "TODO" states
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "REQUEST(q)" "|" "FIXED(f)" "DONE(d)")
        (sequence "|" "CANCELED(c)" "DUPLICATE(2)" "MOVED(m)")))
(setq org-directory "~/org")
;; Capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key my-kbs-map (kbd "C-c c") 'org-capture)
;; Archive
(setq org-archive-location (concat org-directory "/archive/%s_archive::"))


;; Word count minor mode
;; http://taiyaki.org/elisp/word-count/src/word-count.el
(autoload 'word-count-mode "word-count"
  "Minor mode to count words." t nil)
(define-key my-kbs-map (kbd "M-+") 'word-count-mode)


;; Interactively Do Things instead...
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; fuzzy completion


;; Smex
(require 'smex)
(smex-initialize)
(define-key my-kbs-map (kbd "M-x") 'smex)
(define-key my-kbs-map (kbd "M-X") 'smex-major-mode-commands)
(define-key my-kbs-map (kbd "C-c M-x") 'execute-extended-command) ;; old M-x


;; Django stuff - http://garage.pimentech.net/libcommonDjango_django_emacs/
;; and http://metapundit.net/tech_blog/emacs_and_django
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


;; Browse kill-ring - from http://www.elliotglaysher.org/emacs/
(autoload 'browse-kill-ring "browse-kill-ring" "Browse the kill ring." t)
(define-key my-kbs-map (kbd "C-c k") 'browse-kill-ring)
(eval-after-load "browse-kill-ring"
  '(progn
     (setq browse-kill-ring-quit-action 'save-and-restore)))


;; C# Mode
(require 'csharp-mode)
(setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))


;; Javascript
(setq js-indent-level 2)


;; Rainbow Mode - http://julien.danjou.info/rainbow-mode.html
(require 'rainbow-mode)


;; SLIME - used via Quicklisp
(setq inferior-lisp-program "/usr/bin/sbcl")
(let ((quicklisp-helper "~/.quicklisp/slime-helper.el"))
  (if (file-exists-p quicklisp-helper)
      (load quicklisp-helper)))


;; Markdown support - http://jblevins.org/projects/markdown-mode/
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))


;; Ace jump mode - http://www.emacswiki.org/emacs-en/AceJump
(require 'ace-jump-mode)
(define-key my-kbs-map (kbd "C-c j c") 'ace-jump-mode)
(define-key my-kbs-map (kbd "C-c j l") 'ace-jump-line-mode)


;; crosshairs.el - http://www.emacswiki.org/emacs/CrosshairHighlighting
;; (require 'crosshairs)


;; go-mode - distributed with the src distribution
(require 'go-mode-load)


;; YASnippet
(require 'yasnippet)
(yas-global-mode 1)
(yas/load-directory "~/.emacs.d/my-snippets")


;; Clojure
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'nrepl-mode))
(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))


;; SCSS
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)
(setq css-indent-offset 2)


;; Set color theme
(load-theme 'solarized-dark t) ;; This theme doesn't work with emacsclient

(defun solarized-theme-swap ()
  "If solarized-dark is the current theme, switch to the light version, and vice versa."
  (interactive)
  (if (eql (position 'solarized-dark custom-enabled-themes) 0)
    (load-theme 'solarized-light t)
  (load-theme 'solarized-dark t)))
(define-key my-kbs-map (kbd "C-c w") 'solarized-theme-swap)


;; Define the minor mode for my keybindings and activate it
(define-minor-mode my-kbs-minor-mode
  "A minor mode for my custom keybindings."
  t           ;; Enable by default
  " my-kbs"   ;; name in mode line
  my-kbs-map)

;; Turn off in the minibuffer
(add-hook 'minibuffer-setup-hook (lambda () (my-kbs-minor-mode 0)))
