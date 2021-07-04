;;; -*- lexical-binding: t; -*-

(use-package ivy
  :straight t
  :diminish ivy-mode
  :bind (("C-o C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("RET" . ivy-alt-done)
         ("<C-return>" . ivy-immediate-done)
         ("s-n" . ivy-next-history-element)
         ("s-e" . ivy-yank-word)
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
  :straight t
  :bind (("C-s" . swiper)))

(use-package amx
  :straight t
  :defer t
  :init
  (setq amx-history-length 32))

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
  :straight t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c n" . counsel-rg)
         ("C-c e" . counsel-git)
         ("C-c i" . counsel-locate)
         ("C-c o" . counsel-git-grep)
         ("C-c m" . counsel-imenu))
  :init
  (setq counsel-find-file-ignore-regexp (my-counsel-ignore-regexp-builder
                                         "\\`\\."
                                         "\\`__pycache__/\\'"
                                         (my-counsel-ignore-extensions "pyc" "elc"))
        counsel-git-grep-skip-counting-lines t
        counsel-locate-cmd 'counsel-locate-cmd-mdfind))

(provide 'init-ivy)
