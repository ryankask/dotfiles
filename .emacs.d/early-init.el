;;; early-init.el -*- lexical-binding: t; -*-
;; Copied from https://github.com/hlissner/doom-emacs/blob/develop/early-init.el

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

(setq gc-cons-threshold most-positive-fixnum
      native-comp-deferred-compilation nil
      package-enable-at-startup nil
      load-prefer-newer noninteractive
      frame-inhibit-implied-resize t)

(unless (or (daemonp) noninteractive)
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay))))

(set-language-environment "UTF-8")
(setq default-input-method nil)
