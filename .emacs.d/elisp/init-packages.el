(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages '((cider . "melpa-stable")
                                  (clojure-mode . "melpa-stable")
                                  (haskell-mode . "melpa-stable"))))

(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar my-packages
  '(ace-window
    ag
    alchemist
    avy
    browse-kill-ring
    clojure-mode
    column-marker
    company
    company-go
    company-jedi
    dash
    diminish
    elixir-mode
    exec-path-from-shell
    expand-region
    f
    flycheck
    go-mode
    haskell-mode
    ;; helm
    ;; helm-ag
    ;; helm-descbinds
    ;; helm-projectile
    ido-ubiquitous
    ido-vertical-mode
    lua-mode
    magit
    markdown-mode
    nlinum
    cider
    org
    projectile
    rainbow-mode
    ruby-end
    s
    scss-mode
    smartparens
    smex
    solarized-theme
    undo-tree
    virtualenvwrapper
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
