(require 'diminish)

(eval-after-load "my-kbs-minor-mode"
  '(diminish 'my-kbs-minor-mode))

(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))

(eval-after-load "volatile-highlights-mode"
  '(diminish 'volatile-highlights-mode))

(eval-after-load "company"
  '(diminish 'company-mode))

(eval-after-load "anaconda-mode"
  '(diminish 'anaconda-mode))

(provide 'init-diminish)
