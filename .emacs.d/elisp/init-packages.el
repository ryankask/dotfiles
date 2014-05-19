(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar my-packages '(ac-nrepl ace-jump-mode auto-complete
  browse-kill-ring clojure-mode clojure-test-mode col-highlight
  column-marker crosshairs exec-path-from-shell expand-region
  flycheck go-mode haskell-mode hl-line+ ido-ubiquitous lua-mode
  markdown-mode cider org popup rainbow-mode scss-mode
  smartparens smex solarized-theme tuareg vline yasnippet)
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
