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

;; Redefine the function so no custom variables are set
(defun package--save-selected-packages (&optional value)
  "Set and save `package-selected-packages' to VALUE."
  (when value
    (setq package-selected-packages value))
  (if after-init-time
    (add-hook 'after-init-hook #'package--save-selected-packages)))

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
(require 'seq)
(require 'subr-x)

;; Init packages

(use-package init-core)
(use-package init-editor)
(use-package init-ui)
(use-package init-osx
  :if (eq system-type 'darwin))
(use-package init-misc)
(use-package init-ido)
(use-package init-python)
(use-package init-go)
(use-package init-elixir)
(use-package init-web)
