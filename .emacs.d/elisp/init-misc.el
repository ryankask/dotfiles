;; Load miscellaneous packages

(require 'auto-complete-config)
(ac-config-default)

(require 'column-marker)
(require 'go-mode-load)

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
      ido-max-work-file-list 250
      ido-max-dir-file-cache 250
      ido-save-directory-list-file (expand-file-name "ido.hist" dotemacs-dir))
(ido-mode t)
(ido-ubiquitous-mode t)

;; Smex
(setq smex-save-file (expand-file-name "smex.hist" dotemacs-dir))
(setq smex-history-length 50)
(define-key my-kbs-map (kbd "M-x") 'smex)
(define-key my-kbs-map (kbd "M-X") 'smex-major-mode-commands)
(define-key my-kbs-map (kbd "C-c M-x") 'execute-extended-command) ;; old M-x

;; Javascript
(setq js-indent-level 2)

;; Rainbow Mode - http://julien.danjou.info/rainbow-mode.html
(setq rainbow-html-colors-major-mode-list
      '(html-mode css-mode php-mode nxml-mode xml-mode scss-mode))

;; Ace jump mode - http://www.emacswiki.org/emacs-en/AceJump
(define-key my-kbs-map (kbd "C-c j c") 'ace-jump-mode)
(define-key my-kbs-map (kbd "C-c j l") 'ace-jump-line-mode)

;; SCSS
(setq scss-compile-at-save nil)
(setq css-indent-offset 2)

;; Haskell
(add-hook 'haskell-mode-hook
          (lambda ()
            (subword-mode 1)
            (turn-on-haskell-doc)
            (turn-on-haskell-indentation)))

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(define-key my-kbs-map (kbd "C-c e n") 'flycheck-next-error)
(define-key my-kbs-map (kbd "C-c e p") 'flycheck-previous-error)

(provide 'init-misc)
