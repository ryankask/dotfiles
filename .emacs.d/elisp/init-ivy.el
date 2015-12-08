(use-package swiper
  :ensure t
  :diminish ivy-mode
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume))
  :init
  (setq ivy-use-virtual-buffers t
        ivy-display-style 'fancy)
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)))

(provide 'init-ivy)
