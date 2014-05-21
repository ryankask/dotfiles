(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar my-packages
  '(ace-jump-mode
    ace-jump-buffer
    ace-window
    browse-kill-ring
    clojure-mode
    clojure-test-mode
    company
    dash
    diminish
    exec-path-from-shell
    expand-region
    flycheck
    go-mode
    haskell-mode
    ido-ubiquitous
    lua-mode
    markdown-mode
    cider
    org
    rainbow-mode
    scss-mode
    smartparens
    smex
    solarized-theme
    undo-tree
    volatile-highlights
    yasnippet)
  "A list of packages that must be installed.")

(defun install-my-packages ()
  "Install each package in ``my-packages`` if it isn't installed."
  (let ((package-contents-refreshed nil))
    (dolist (my-package my-packages)
      (unless (package-installed-p my-package)
        (unless package-contents-refreshed
          (package-refresh-contents)
          (setq package-contents-refreshed t))
        (package-install my-package)))))

(install-my-packages)

(provide 'init-packages)
