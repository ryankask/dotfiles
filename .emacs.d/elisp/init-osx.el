(setq ns-function-modifier 'hyper
      ring-bell-function 'ignore)

(require 'exec-path-from-shell)

(setq exec-path-from-shell-variables
      (append exec-path-from-shell-variables '("GOPATH")))
(exec-path-from-shell-initialize)

;; Disable `ns-popup-font-panel', which causes emacs to sometimes freeze
(global-unset-key (kbd "s-t"))

(provide 'init-osx)
