;; Load miscellaneous packages

(require 'company)
(setq company-idle-delay 0.5
      company-tooltip-limit 10
      company-minimum-prefix-length 2)
(add-hook 'after-init-hook 'global-company-mode)

(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (expand-file-name "snippets/shnippet" dotemacs-dir))
(yas-global-mode 1)
(setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
(add-hook 'snippet-mode-hook (lambda () (setq require-final-newline nil)))

;; Don't use auto-fill-mode in html-mode
(add-hook 'html-mode-hook (lambda ()
                            (turn-off-auto-fill)
                            (set-fill-column 100)))

;; rst-mode
(add-hook 'rst-mode-hook (lambda () (set-fill-column 80)))

;; Javascript
(setq js-indent-level 2)

;; Rainbow Mode - http://julien.danjou.info/rainbow-mode.html
(setq rainbow-html-colors-major-mode-list
      '(html-mode css-mode php-mode nxml-mode xml-mode scss-mode))

;; Avy - https://github.com/abo-abo/avy
(setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o))
(define-key my-kbs-map (kbd "s-.") 'avy-goto-word-1)
(define-key my-kbs-map (kbd "s-/") 'avy-goto-line)

;; SCSS
(setq scss-compile-at-save nil)
(setq css-indent-offset 2)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(define-key my-kbs-map (kbd "C-c e n") 'flycheck-next-error)
(define-key my-kbs-map (kbd "C-c e p") 'flycheck-previous-error)

;; expand-region
(define-key my-kbs-map (kbd "C-=") 'er/expand-region)

;; magit
(define-key my-kbs-map (kbd "C-x g") 'magit-status)
(define-key my-kbs-map (kbd "C-x M-g") 'magit-dispatch-popup)
(define-key my-kbs-map (kbd "s-m m") 'magit-status)
(define-key my-kbs-map (kbd "s-m l") 'magit-log)
(define-key my-kbs-map (kbd "s-m f") 'magit-log-buffer-file)
(define-key my-kbs-map (kbd "s-m b") 'magit-blame)
(setq magit-completing-read-function 'magit-ido-completing-read)

;; Shell
(setq sh-basic-offset 2
      sh-indentation 2)

;; zsh
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;; ag.el
(setq ag-highlight-search t
      ag-reuse-buffers t)

;; Projectile
(setq projectile-keymap-prefix (kbd "C-c ;"))
(projectile-global-mode t)

(provide 'init-misc)
