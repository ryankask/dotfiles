;;; Ryan Kaskel's emacs configuration

;; Set up paths
(defvar dotemacs-dir (file-name-directory load-file-name))
(defvar elisp-dir (expand-file-name "elisp" dotemacs-dir))
(add-to-list 'load-path elisp-dir)

(require 'init-keymap)
(require 'init-core)
(require 'init-packages)
(require 'init-editor)
(require 'init-ui)
(require 'init-python)
(require 'init-org)
(require 'init-clojure)
(require 'init-haskell)
(require 'init-go)
(require 'init-misc)
(when (eq system-type 'darwin)
  (require 'init-osx))
(require 'init-diminish)
