;;;; Load miscellaneous packages

;;; Productivity

(use-package ag
  :ensure t
  :init
  (setq ag-highlight-search t
        ag-reuse-buffers t))

(use-package avy
  :bind (("C-'" . avy-goto-char-2)
         ("M-g g" . avy-goto-line)
         ("M-g e" . avy-goto-word-0)
         ("M-g w" . avy-goto-word-1))
  :init
  (setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o)))

(use-package ace-window
  :ensure t
  :bind ("C-o C-o" . ace-window))

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq company-idle-delay 0.3
        company-tooltip-limit 10
        company-minimum-prefix-length 2)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :bind (("C-c e n" . flycheck-next-error)
         ("C-c e p" . flycheck-previous-error))
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
         ("s-m m" . magit-status)
         ("s-m l" . magit-log)
         ("s-m f" . magit-log-buffer-file)
         ("s-m b" . magit-blame))
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read))

(use-package org
  :ensure t
  :pin melpa-stable
  :defer t
  :init
  (setq org-log-done 'time))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-keymap-prefix (kbd "C-c ;"))
  :config
  (projectile-global-mode t))

(use-package rainbow-mode
  :ensure t
  :init
  (setq rainbow-html-colors-major-mode-list
        '(html-mode css-mode php-mode nxml-mode xml-mode scss-mode)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets/shnippet" dotemacs-dir))
  (yas-global-mode 1)
  (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
  (add-hook 'snippet-mode-hook (lambda () (setq require-final-newline nil))))

;;; Languages

(use-package css
  :defer t
  :init
  (setq css-indent-offset 2))

(use-package js-mode
  :mode ("\\.js\\'"
         "\\.json\\'")
  :init
  (setq js-indent-level 2))

(use-package lua-mode
  :ensure t
  :defer t)

(use-package scss-mode
  :ensure t
  :defer t
  :init
  (setq scss-compile-at-save nil))

(use-package sh-mode
  :mode "\\.zsh\\'"
  :init
  (setq sh-basic-offset 2
        sh-indentation 2))

;;; Formats

(use-package yaml-mode
  :ensure t
  :defer t)

;;; Writing

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package rst-mode
  :defer t
  :init
  (add-hook 'rst-mode-hook (lambda () (set-fill-column 80))))

(provide 'init-misc)
