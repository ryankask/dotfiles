(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar my-packages
  '(ace-window
    alchemist
    avy
    browse-kill-ring
    clojure-mode
    column-marker
    company
    company-go
    dash
    diminish
    elixir-mode
    exec-path-from-shell
    expand-region
    flycheck
    go-mode
    haskell-mode
    ido-ubiquitous
    lua-mode
    magit
    markdown-mode
    nlinum
    cider
    org
    rainbow-mode
    ruby-end
    s
    scss-mode
    smartparens
    smex
    solarized-theme
    undo-tree
    volatile-highlights
    web-mode
    yaml-mode
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
  (let* ((package-buffer-name "*Packages*")
         (package-buffer-exists
          (member package-buffer-name
                  (mapcar (lambda (buffer)
                            (buffer-name buffer)) (buffer-list)))))
    (if package-buffer-exists
        (switch-to-buffer package-buffer-name)
      (list-packages)))
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
