;;; -*- lexical-binding: t; -*-

(use-package ido
  :straight t
  :init
  :config
  (setq ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-default-file-method 'selected-window
        ido-auto-merge-work-directories-length -1
        ido-use-virtual-buffers t
        ido-max-work-file-list 250
        ido-max-dir-file-cache 250
        ido-ignore-extensions t
        ido-save-directory-list-file (expand-file-name "ido.hist" user-emacs-directory)
        ido-ignore-files (append ido-ignore-files '("\\`__pycache__/")))
  (ido-mode t)
  (ido-everywhere t))

(use-package ido-completing-read+
  :straight t
  :config
  (ido-ubiquitous-mode t))

(use-package ido-vertical-mode
  :straight t
  :config
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))
  (ido-vertical-mode 1)

(use-package smex
  :straight t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c M-x" . execute-extended-command))
  :init
  (setq smex-save-file (expand-file-name "smex.hist" user-emacs-directory)
        smex-history-length 32))

(provide 'init-ido)
