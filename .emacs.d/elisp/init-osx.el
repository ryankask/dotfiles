(setq ns-function-modifier 'hyper
      ring-bell-function 'ignore)

(require 'exec-path-from-shell)

(setq exec-path-from-shell-variables
      (append exec-path-from-shell-variables '("GOPATH")))
(exec-path-from-shell-initialize)

(provide 'init-osx)
