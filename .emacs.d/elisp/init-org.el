;;; -*- lexical-binding: t; -*-

(defun my-org-open-line-after-meta-data ()
  "Open a new line after the the following metadata."
  (interactive)
  (org-end-of-meta-data t)
  (open-line 1)
  (while (org-previous-line-empty-p)
    (next-line -1)))

(use-package org
  :ensure (:tag "release_9.7.16" :pin t)
  :bind (nil
         :map org-mode-map
         ("C-'" . nil)
         ("C-#" . nil)
         ("C-c #" . nil)
         ("C-c RET" . nil)
         ("s-<return>" . org-meta-return)
         ("s-<left>" . org-metaleft)
         ("s-[" . org-metaleft)
         ("C-M-s-h" . org-metaleft)
         ("s-<right>" . org-metaright)
         ("s-]" . org-metaright)
         ("C-M-s-i" . org-metaright)
         ("s-<up>" . org-metaup)
         ("C-M-s-e" . org-metaup)
         ("s-<down>" . org-metadown)
         ("C-M-s-n" . org-metadown)
         ("M-t o" . consult-org-heading)
         ("C-s-<return>" . my-org-open-line-after-meta-data)
         ("s-RET" . my-org-open-line-after-meta-data)
         :repeat-map my-org-motion-repeat-map
         :exit
         ("j" . org-goto)
         :continue
         ("u" . outline-up-heading)
         ("b" . org-backward-heading-same-level)
         ("f" . org-forward-heading-same-level)
         ("p" . org-previous-visible-heading)
         ("n" . org-next-visible-heading))
  :hook (org-mode . my-fill-column-setup)
  :custom
  (org-export-backends '(ascii md html icalendar))
  (org-catch-invisible-edits 'show-and-error)
  (org-blank-before-new-entry '((heading . nil) (plain-list-item . auto)))
  (org-cycle-separator-lines 1)
  (org-babel-load-languages '((emacs-lisp . t)
                              (sql . t)))
  (org-hide-emphasis-markers t)
  (org-fontify-done-headline nil)
  (org-insert-heading-respect-content t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-pretty-entities t)
  (org-special-ctrl-a/e t)
  (org-use-sub-superscripts "{}"))

(use-package org-modern
  :ensure t
  :after org
  :custom
  (org-modern-table nil)
  :custom-face
  (org-modern-label ((t (:height 0.85))))
  :init
  (global-org-modern-mode))

(provide 'init-org)
