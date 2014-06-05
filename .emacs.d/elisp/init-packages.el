(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar my-packages
  '(ace-jump-mode
    ace-window
    browse-kill-ring
    clojure-mode
    clojure-test-mode
    column-marker
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
    magit
    markdown-mode
    cider
    org
    rainbow-mode
    s
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

(defun my-packages-print-upgradable-packages ()
  "Print the names of packages that can be upgraded."
  (interactive)
  (unless (derived-mode-p 'package-menu-mode)
    (error "The current buffer is not a Package menu"))
  (let ((upgrades (package-menu--find-upgrades)))
    (if (null upgrades)
        (message "No packages to upgrade.")
      (let ((package-names (mapcar
                            (lambda (info) (symbol-name (elt info 0)))
                            upgrades)))
        (if package-names
            (message "Upgradable packages: %s"
                     (mapconcat 'identity package-names ", "))
          (message "No packages to upgrade."))))))

(define-key my-kbs-map (kbd "C-c u") 'my-packages-print-upgradable-packages)

(provide 'init-packages)
