;;; -*- lexical-binding: t; -*-

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" "\\.mustache\\'")
  :init
  (setq web-mode-enable-auto-pairing nil
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(provide 'init-web)
