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

;; Don't use auto-fill-mode in html-mode
(add-hook 'html-mode-hook (lambda ()
                            (turn-off-auto-fill)
                            (set-fill-column 100)))

;; rst-mode
(add-hook 'rst-mode-hook (lambda () (set-fill-column 80)))

;; ido
(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-work-file-list 250
      ido-max-dir-file-cache 250
      ido-save-directory-list-file (expand-file-name "ido.hist" dotemacs-dir))
(ido-mode t)
(ido-ubiquitous-mode t)

;; Smex
(setq smex-save-file (expand-file-name "smex.hist" dotemacs-dir))
(setq smex-history-length 250)
(define-key my-kbs-map (kbd "M-x") 'smex)
(define-key my-kbs-map (kbd "M-X") 'smex-major-mode-commands)
(define-key my-kbs-map (kbd "C-c M-x") 'execute-extended-command) ;; old M-x

;; Web mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2)

;; Javascript
(setq js-indent-level 2)

;; Rainbow Mode - http://julien.danjou.info/rainbow-mode.html
(setq rainbow-html-colors-major-mode-list
      '(html-mode css-mode php-mode nxml-mode xml-mode scss-mode))

;; Ace jump mode - http://www.emacswiki.org/emacs-en/AceJump
(define-key my-kbs-map (kbd "s-.") 'ace-jump-mode)
(define-key my-kbs-map (kbd "s-/") 'ace-jump-line-mode)

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
(define-key my-kbs-map (kbd "C-c g") 'magit-status)

;; Shell
(setq sh-basic-offset 2
      sh-indentation 2)

;; zsh
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(provide 'init-misc)
