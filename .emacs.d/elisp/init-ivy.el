(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-j" . ivy-immediate-done)
         ("RET" . ivy-alt-done))
  :init
  (setq ivy-count-format "(%d/%d) "
        ivy-extra-directories nil
        ivy-height 11
        ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package smex
  :ensure t
  :init
  (setq smex-save-file (expand-file-name "smex.hist" dotemacs-dir)
        smex-history-length 32))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate))
  :init
  (setq counsel-find-file-ignore-regexp "^\\(?:__pycache__\\)\\|\\(?:.*\\.\\(?:pyc\\|elc\\)\\)$"))


(provide 'init-ivy)
