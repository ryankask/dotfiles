;;; -*- lexical-binding: t; -*-

(setq ns-function-modifier 'hyper
      ring-bell-function 'ignore
      trash-directory "~/.Trash"
      native-comp-driver-options '("-Wl,-w"))

(use-package exec-path-from-shell
  :straight t
  :config
  (setq exec-path-from-shell-arguments nil
        exec-path-from-shell-variables (append exec-path-from-shell-variables
                                               (list "GOPATH" "DOTFILES")))
  (exec-path-from-shell-initialize))

;; Disable `ns-popup-font-panel', which causes emacs to sometimes freeze
(unbind-key "s-t")

(provide 'init-macos)
