;;; early-init.el -*- lexical-binding: t; -*-
;; Copied from
;; https://github.com/doomemacs/core/blob/master/early-init.el
;; Optimisations fall into two groups, global and interactive. Each is
;; tagged with its related doom source file.

;; doomemacs/early-init.el
(setq gc-cons-threshold (* 128 1024 1024)
      load-prefer-newer t)

(defun my-reset-startup-gc ()
  "Restore GC if package initialization has not done so."
  (when (= gc-cons-threshold (* 128 1024 1024))
    (setq gc-cons-threshold (* 16 1024 1024))))

(add-hook 'emacs-startup-hook #'my-reset-startup-gc 100)

;; doomemacs/lisp/doom.el
(setq ad-redefinition-action 'accept
      warning-suppress-types '((defvaralias) (lexical-binding))
      debug-on-error init-file-debug
      jka-compr-verbose init-file-debug
      package-enable-at-startup nil)

;; My additions

(when (and (boundp 'load-path-filter-function)
           (fboundp 'load-path-filter-cache-directory-files))
  (setq load-path-filter-function #'load-path-filter-cache-directory-files))

(defvar my-macos-initial-frame nil
  "Startup frame to position before making it visible on macOS.")

(defun my-macos-record-initial-frame (frame)
  "Remember the first graphical FRAME created during startup."
  (when (display-graphic-p frame)
    (setq my-macos-initial-frame frame)
    (remove-hook 'after-make-frame-functions #'my-macos-record-initial-frame)))

(defun my-macos-reveal-initial-frame (&rest _)
  "Reveal the startup frame, including when entering the debugger."
  (when (frame-live-p my-macos-initial-frame)
    (make-frame-visible my-macos-initial-frame))
  (advice-remove #'debug #'my-macos-reveal-initial-frame))

(defun my-macos-position-initial-frame ()
  "Fit the startup frame to the right half of its monitor's work area."
  (unwind-protect
      (when (and (frame-live-p my-macos-initial-frame)
                 (display-graphic-p my-macos-initial-frame))
        (when-let* ((workarea (alist-get 'workarea
                                         (frame-monitor-attributes
                                          my-macos-initial-frame))))
          (pcase-let* ((`(,x ,y ,width ,height) workarea)
                       (half (/ width 2))
                       (outer (alist-get 'outer-size
                                         (frame-geometry my-macos-initial-frame)))
                       ;; `set-frame-size' sizes the text area, not the outer window.
                       (extra-width (- (car outer)
                                       (frame-text-width my-macos-initial-frame)))
                       (extra-height (- (cdr outer)
                                        (frame-text-height my-macos-initial-frame))))
            (set-frame-size my-macos-initial-frame
                            (- width half extra-width)
                            (- height extra-height) t)
            ;; Explicit offsets also work for monitors with negative coordinates.
            (modify-frame-parameters my-macos-initial-frame
                                     `((left . (+ ,(+ x half)))
                                       (top . (+ ,y)))))))
    ;; Never leave the frame hidden if sizing fails or monitor data is absent.
    (unwind-protect
        (my-macos-reveal-initial-frame)
      (setq my-macos-initial-frame nil))))

(unless noninteractive
  (when (and (eq system-type 'darwin)
             (eq initial-window-system 'ns)
             (not (daemonp)))
    (setq frame-resize-pixelwise t)
    (add-hook 'after-make-frame-functions #'my-macos-record-initial-frame)
    (add-hook 'window-setup-hook #'my-macos-position-initial-frame 90)
    ;; Hide only the startup frame; daemon/client and later frames are unaffected.
    ;; Keep it visible with --debug-init, and reveal it before any debugger opens.
    (unless init-file-debug
      (advice-add #'debug :before #'my-macos-reveal-initial-frame)
      (setf (alist-get 'visibility initial-frame-alist) nil)))

  ;; doomemacs/lisp/doom.el
  (setq frame-inhibit-implied-resize t
        inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name
        initial-major-mode 'fundamental-mode
        initial-scratch-message nil)

  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (advice-add #'display-startup-screen :override #'ignore)

  (unless init-file-debug
    (unless (eq system-type 'darwin)
      (setq command-line-ns-option-alist nil))
    (unless (memq initial-window-system '(x pgtk))
      (setq command-line-x-option-alist nil)))

  ;; doomemacs/lisp/doom-emacs.el
  (setq auto-mode-case-fold nil
        bidi-inhibit-bpa t
        highlight-nonselected-windows nil
        fast-but-imprecise-scrolling t
        inhibit-compacting-font-caches t
        redisplay-skip-fontification-on-input t)

  (setq-default bidi-display-reordering t
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
