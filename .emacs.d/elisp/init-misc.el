;; Load miscellaneous packages

(require 'auto-complete-config)
(ac-config-default)

(require 'column-marker)
(require 'go-mode-load)

(require 'yasnippet)
(yas-global-mode 1)

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

;; Word count minor mode
;; http://taiyaki.org/elisp/word-count/src/word-count.el
(autoload 'word-count-mode "word-count"
  "Minor mode to count words." t nil)
(define-key my-kbs-map (kbd "M-+") 'word-count-mode)

;; ido
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-default-file-method 'selected-window
      ido-use-virtual-buffers t
      ido-everywhere t)

;; Smex
(define-key my-kbs-map (kbd "M-x") 'smex)
(define-key my-kbs-map (kbd "M-X") 'smex-major-mode-commands)
(define-key my-kbs-map (kbd "C-c M-x") 'execute-extended-command) ;; old M-x

;; C# Mode
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; Javascript
(setq js-indent-level 2)

;; Rainbow Mode - http://julien.danjou.info/rainbow-mode.html
(autoload 'rainbow-mode "rainbow-mode"
  "Colorize strings that represent colors." t)
(setq rainbow-html-colors-major-mode-list
      '(html-mode css-mode php-mode nxml-mode xml-mode scss-mode))

;; SLIME - used via Quicklisp
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq quicklisp-helper-on nil)
(let ((quicklisp-helper "~/.quicklisp/slime-helper.el"))
  (if (and quicklisp-helper-on (file-exists-p quicklisp-helper))
      (load quicklisp-helper)))

;; Markdown support - http://jblevins.org/projects/markdown-mode/
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Ace jump mode - http://www.emacswiki.org/emacs-en/AceJump
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(define-key my-kbs-map (kbd "C-c j c") 'ace-jump-mode)
(define-key my-kbs-map (kbd "C-c j l") 'ace-jump-line-mode)


;; SCSS
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)
(setq css-indent-offset 2)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(define-key my-kbs-map (kbd "C-c e n") 'flycheck-next-error)
(define-key my-kbs-map (kbd "C-c e p") 'flycheck-previous-error)

(provide 'init-misc)
