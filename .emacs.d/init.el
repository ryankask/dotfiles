;;;; Ryan Kaskel's emacs configuration

;;; Paths

(defconst dotemacs-dir (file-name-directory load-file-name))
(defconst elisp-dir (expand-file-name "elisp" dotemacs-dir))
(add-to-list 'load-path elisp-dir)

;;; Packaging

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

(setq url-http-attempt-keepalives nil
      gnutls-verify-error nil)

(defconst my-packages
  '(use-package
    diminish)
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

;;; Initialization

;; Base packages

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Init packages

(use-package init-core)
(use-package init-editor)
(use-package init-ui)
(use-package init-osx
  :if (eq system-type 'darwin))
(use-package init-ivy
  :disabled t)
(use-package init-misc)
(use-package init-ido)
(use-package init-python)
(use-package init-clojure
  :disabled t)
(use-package init-haskell
  :disabled t)
(use-package init-go)
(use-package init-elixir)
(use-package init-web)
