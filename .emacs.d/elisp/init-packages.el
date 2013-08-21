(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar my-packages
  '(ac-nrepl ace-jump-mode auto-complete browse-kill-ring
             clojure-mode col-highlight column-marker csharp-mode
             crosshairs flycheck go-mode hl-line+ lua-mode
             markdown-mode nrepl org popup python rainbow-mode
             scss-mode smartparens smex solarized-theme vline yasnippet)
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
