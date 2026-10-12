;;; -*- lexical-binding: t; -*-

(defun my-shr-strong-tag (dom)
  "Improve the styling of a <strong> tag if it appears to come
from a GCP release notes entry."
  (if-let* ((class (dom-attr dom 'class))
            ((string-match-p "\\brelease-note-product-title\\b" class)))
      (shr-tag-h2 dom)
    (shr-tag-strong dom)))

(defun my-elfeed-show-mode-hook ()
  (setq-local shr-width 88
              shr-external-rendering-functions '((strong . my-shr-strong-tag)))
  (pcase-dolist (`(,face . ,height) '((shr-h1 . 2.074)
                                      (shr-h2 . 1.728)
                                      (shr-h3 . 1.44)
                                      (shr-h4 . 1.2)
                                      (shr-h5 . 1.0)
                                      (shr-h6 . 1.0)))
    (face-remap-add-relative face :height height :weight 'semi-bold)))

(defvar my-elfeed--last-refresh nil
  "The last time `my-elfeed' was called.")

(defun my-elfeed (&optional max-age)
  "Enter elfeed and refresh all feeds if the database was last updated
 more than MAX-AGE minutes ago.

If MAX-AGE is nil, default to 15 minutes."
  (interactive)
  (elfeed)
  (when (> (- (float-time) (or my-elfeed--last-refresh 0)) (* 60 (or max-age 15)))
    (elfeed-update)
    (setq my-elfeed--last-refresh (float-time))))

(use-package elfeed
  :ensure t
  :commands elfeed
  :hook (elfeed-show-mode . my-elfeed-show-mode-hook)
  :bind ("C-o y" . my-elfeed))

(use-package elfeed-protocol
  :ensure t
  :demand t
  :after elfeed
  :custom
  (elfeed-protocol-enabled-protocols '(freshrss))
  :config
  (elfeed-protocol-enable))

(use-package elfeed-protocol-freshrss
  :ensure t
  :demand t
  :after elfeed-protocol
  :config
  (elfeed-protocol-freshrss-register-protocol))

(provide 'init-rss)
