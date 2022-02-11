;;; -*- lexical-binding: t; -*-

(use-package web-mode
  :straight t
  :mode ("\\.html?\\'")
  :init
  (setq web-mode-enable-auto-pairing nil
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  :config
  (sp-with-modes '(web-mode)
    (sp-local-pair "%" "%"
                   :unless '(sp-in-string-p)
                   :post-handlers '(((lambda (&rest _ignored)
                                       (just-one-space)
                                       (save-excursion (insert " ")))
                                     "SPC" "=" "#")))
    (sp-local-pair "<% "  " %>" :insert "C-c %")
    (sp-local-pair "<%= " " %>" :insert "C-c =")
    (sp-local-pair "<%# " " %>" :insert "C-c #")
    (sp-local-tag "%" "<% "  " %>")
    (sp-local-tag "=" "<%= " " %>")
    (sp-local-tag "#" "<%# " " %>")))

(use-package typescript-mode
  :straight t
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode)))

(use-package tree-sitter
  :straight t
  :hook ((typescript-mode . tree-sitter-hl-mode)
         (typescript-tsx-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist
               '(typescript-tsx-mode . tsx)))

(provide 'init-web)
