;;;; Ryan Kaskel's emacs configuration -*- lexical-binding: t; -*-

;;; Paths

(defconst my-elisp-dir (concat user-emacs-directory "elisp"))
(add-to-list 'load-path my-elisp-dir)
(setq default-directory "~/")

;;; Packaging

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'diminish)

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
(use-package init-macos
  :if (eq system-type 'darwin))
(use-package init-misc)
(use-package init-completion)
(use-package init-lsp)
(use-package init-python)
(use-package init-go)
(use-package init-rust)
(use-package init-web)

;; Customisations

(when (file-exists-p custom-file)
  (load custom-file))
