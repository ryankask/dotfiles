;;; -*- lexical-binding: t; -*-

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-o C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-i" . ivy-immediate-done)
         ("RET" . ivy-alt-done)
         ("s-i" . ivy-insert-current)
         :map ivy-occur-grep-mode-map
         ("C-o" . nil))
  :init
  (setq ivy-count-format "(%d/%d) "
        ivy-extra-directories nil
        ivy-format-function 'ivy-format-function-arrow
        ivy-height 11
        ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'full)
  :config
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package smex
  :ensure t
  :defer t
  :init
  (setq smex-save-file (expand-file-name "smex.hist" dotemacs-dir)
        smex-history-length 32))

(defun my-counsel-ignore-regexp-builder (&rest regexp-units)
  (mapconcat
   (lambda (unit)
     (format "\\(?:%s\\)" unit))
   regexp-units
   "\\|"))

(defun my-counsel-ignore-extensions (&rest extensions)
  (format "\\`.*\\.\\(?:%s\\)\\'" (string-join extensions "\\|")))

(defun my-counsel-locate-cmd-mdfind (input)
  "Return a shell command based on INPUT."
  (format "mdfind -name '%s'" input))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c n" . counsel-rg)
         ("C-c i" . counsel-git)
         ("C-c o" . counsel-git-grep)
         ("C-c e" . counsel-locate)
         ("C-c m" . counsel-imenu))
  :init
  (setq counsel-find-file-ignore-regexp (my-counsel-ignore-regexp-builder
                                         "\\`\\."
                                         "\\`__pycache__/\\'"
                                         (my-counsel-ignore-extensions "pyc" "elc"))
        counsel-locate-cmd 'counsel-locate-cmd-mdfind))

(provide 'init-ivy)
