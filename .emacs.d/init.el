;;;; Ryan Kaskel's emacs configuration -*- lexical-binding: t; -*-

(setq user-full-name "Ryan Kaskel"
      user-mail-address "dev@ryankaskel.com")

;;; Paths

(defconst my-elisp-dir (concat user-emacs-directory "elisp"))
(add-to-list 'load-path my-elisp-dir)
(setq default-directory "~/")

;;; Packaging

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (setopt use-package-compute-statistics nil)
  (elpaca-use-package-mode))

(elpaca-wait)

;;; Initialisation

(use-package init-editor)
(use-package init-ui)
(use-package init-macos
  :if (eq system-type 'darwin))
(use-package init-misc)
(use-package init-org)
(use-package init-completion)
(use-package init-lsp)
(use-package init-python)
(use-package init-go)
(use-package init-rust)
(use-package init-haskell)
(use-package init-logic)
(use-package init-web)
(use-package init-ai)

;; Customisations

(defun my-host-custom-file-name ()
  "Return custom filename for current host or nil if host cannot
 be determined."
  (when-let* ((hostname (car (split-string (system-name) "\\."))))
    (concat hostname ".el")))

(defun my-load-host-custom-file ()
  "Load host-specific custom file if it exists."
  (setq custom-file
        (expand-file-name
         (concat "cloud/custom/"
                 (or (my-host-custom-file-name)
                     (format "unknown-%s.el" system-type)))
         user-emacs-directory))
  (load custom-file 'noerror))

(add-hook 'elpaca-after-init-hook #'my-load-host-custom-file)
