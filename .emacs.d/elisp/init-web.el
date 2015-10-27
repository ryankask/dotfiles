(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'"
         "\\.jsx\\'"
         "\\.eex\\'")
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
    (sp-local-tag "#" "<%# " " %>"))

  (add-to-list 'company-dabbrev-code-modes 'web-mode))

(provide 'init-web)
