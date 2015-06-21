(require 'diminish)

(eval-after-load "init-keymap"
  '(diminish 'my-kbs-minor-mode))

(eval-after-load "flycheck"
  '(diminish 'flycheck-mode))

(eval-after-load "yasnippet"
  '(diminish 'yas-minor-mode))

(eval-after-load "smartparens"
  '(diminish 'smartparens-mode))

(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))

(eval-after-load "volatile-highlights"
  '(diminish 'volatile-highlights-mode))

(eval-after-load "company"
  '(diminish 'company-mode))

(eval-after-load "ruby-end"
  '(diminish 'ruby-end-mode))

(eval-after-load "alchemist"
  '(diminish 'alchemist-mode "alch"))

(eval-after-load "projectile"
  '(diminish 'projectile-mode))

(provide 'init-diminish)
