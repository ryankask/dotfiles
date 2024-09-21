;;; early-init.el -*- lexical-binding: t; -*-
;; Copied from https://github.com/hlissner/doom-emacs/blob/develop/early-init.el

;; Performance optimisations adopted from Doom Emacs.
;; TODO: Measure and verify the impact of these enhancements.

(setq gc-cons-threshold most-positive-fixnum
      package-enable-at-startup nil
      native-comp-async-report-warnings-errors nil
      load-prefer-newer noninteractive
      frame-inhibit-implied-resize t
      bidi-inhibit-bpa t
      highlight-nonselected-windows nil
      fast-but-imprecise-scrolling t
      read-process-output-max (* 64 1024)
      redisplay-skip-fontification-on-input t)

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              cursor-in-non-selected-windows nil)

;; This *does* make a big difference
(unless (or (daemonp) noninteractive)
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redraw-frame))))

(set-language-environment "UTF-8")
(setq default-input-method nil)
