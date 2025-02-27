;;; early-init.el -*- lexical-binding: t; -*-
;; Copied from
;; https://github.com/hlissner/doom-emacs/blob/develop/early-init.el
;; Optimisations fall into two groups, global and interactive. Each is
;; tagged with its related doom source file.

;; doomemacs/early-init.el
(setq gc-cons-threshold most-positive-fixnum
      load-prefer-newer noninteractive)

;; doomemacs/lisp/doom.el
(setq ad-redefinition-action 'accept
      warning-suppress-types '((defvaralias) (lexical-binding))
      debug-on-error init-file-debug
      jka-compr-verbose init-file-debug
      package-enable-at-startup nil)

;;  Silence obsoletion warnings about (if|when)-let in >=31.
(put 'if-let 'byte-obsolete-info nil)
(put 'when-let 'byte-obsolete-info nil)

(unless noninteractive
  ;; doomemacs/lisp/doom.el
  (setq frame-inhibit-implied-resize t
        inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        initial-major-mode 'fundamental-mode
        initial-scratch-message nil)

  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (advice-add #'display-startup-screen :override #'ignore)

  (unless init-file-debug
    ;; The most important optimisation
    (setq-default inhibit-redisplay t
                  inhibit-message t)
    (defun my-reset-inhibited-vars ()
      (setq-default inhibit-redisplay nil
                    inhibit-message nil)
      (remove-hook 'post-command-hook #'my-reset-inhibited-vars))
    (add-hook 'post-command-hook #'my-reset-inhibited-vars)

    (unless (eq system-type 'darwin)
      (setq command-line-ns-option-alist nil))
    (unless (memq initial-window-system '(x pgtk))
      (setq command-line-x-option-alist nil)))

  ;; doomemacs/lisp/doom-start.el
  (setq auto-mode-case-fold nil
        bidi-inhibit-bpa t
        highlight-nonselected-windows nil
        fast-but-imprecise-scrolling t
        inhibit-compacting-font-caches t
        read-process-output-max (* 64 1024)
        redisplay-skip-fontification-on-input t)

  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right
                cursor-in-non-selected-windows nil)

  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  (setq menu-bar-mode nil
        tool-bar-mode nil
        scroll-bar-mode nil)

  (set-language-environment "UTF-8")
  (setq default-input-method nil))
