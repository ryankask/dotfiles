;;; early-init.el -*- lexical-binding: t; -*-
;; Copied from https://github.com/hlissner/doom-emacs/blob/develop/early-init.el

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

(setq gc-cons-threshold most-positive-fixnum
      package-enable-at-startup nil
      native-comp-async-report-warnings-errors nil
      load-prefer-newer noninteractive
      frame-inhibit-implied-resize t
      read-process-output-max (* 1024 1024))

(unless (or (daemonp) noninteractive)
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'after-init-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redraw-frame))))

(set-language-environment "UTF-8")
(setq default-input-method nil)
