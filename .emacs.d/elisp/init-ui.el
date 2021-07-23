;;; -*- lexical-binding: t; -*-

(menu-bar-mode -1)
(set-frame-font "Menlo Nerd Font-13" nil t)

(when (display-graphic-p)
  (add-to-list 'default-frame-alist (cons 'width 102))
  (add-to-list 'default-frame-alist (cons 'height 56))
  (set-frame-position nil 604 0)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (if (version< emacs-version "26")
      (use-package nlinum
        :straight t
        :config
        (global-nlinum-mode 1))
    (progn
      (setq display-line-numbers-grow-only t)
      (global-display-line-numbers-mode))))

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t
      use-dialog-box nil
      initial-scratch-message nil
      confirm-kill-emacs 'y-or-n-p
      show-paren-mode t)

;; From https://github.com/minad/vertico
;; Add prompt indicator to `completing-read-multiple'.
(defun my-crm-indicator (args)
  (cons (concat "[CRM] " (car args)) (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'my-crm-indicator)

(setq enable-recursive-minibuffers t
      ;; Do not allow the cursor in the minibuffer prompt
      minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


(defun my-doom-one-theme-customise ()
  (interactive)
  (doom-themes-set-faces
   'user
   ;; base
   '(cursor :background fg)
   '(minibuffer-prompt :foreground fg)
   '(font-lock-variable-name-face :foreground fg)
   '(line-number-current-line :foreground base4 :background bg)
   ;; dired
   '(dired-directory :foreground blue)
   ;; avy
   '(avy-goto-char-timer-face :background blue :foreground base0)
   '(avy-lead-face :background yellow :foreground base0)
   '(avy-lead-face-0 :background red :foreground base0)
   '(avy-lead-face-1 :background orange :foreground base0)
   '(avy-lead-face-2 :background violet :foreground base0)
   ;; column-marker
   '(column-marker-1 :background dark-blue)
   '(column-marker-2 :background blue)
   ;; ivy
   ;; org
   '(org-level-1 :foreground blue :background base3 :bold t :height 1.0)
   ;; lsp-ui
   '(lsp-ui-doc-background :background bg-alt)
   '(lsp-ui-doc-header :background highlight :foreground bg)))

(use-package doom-themes
  :straight t
  :init
  (load-theme 'doom-one t)
  (my-doom-one-theme-customise))

(provide 'init-ui)
