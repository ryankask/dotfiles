;;; -*- lexical-binding: t; -*-

(defun my-org-mode-setup ()
  "Hook that runs when org-mode loads."
  (setq-local tab-width 8)
  (my-fill-column-setup))

(defun my-org-open-line-after-meta-data ()
  "Open a new line after the the following metadata."
  (interactive)
  (org-end-of-meta-data t)
  (open-line 1)
  (while (org-previous-line-empty-p)
    (next-line -1)))

(defun my-org-insert-people-meeting-headline (&optional date)
  "Insert a new headline above the child subtree with DATE"
  (interactive)
  (org-back-to-heading)
  (org-fold-show-children)
  (org-next-visible-heading 1)
  (org-insert-heading)
  (org-move-subtree-up)
  (insert (or date (format-time-string "%Y-%m-%d")))
  (org-return)
  (insert "- "))

(use-package org
  :ensure (:host github :repo "elpa-mirrors/org-mode" :tag "release_9.8" :depth 1)
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
         ("C-c m" . my-org-insert-people-meeting-headline))
  :hook (org-mode . my-org-mode-setup)
  :custom
  (org-export-backends '(ascii md html icalendar))
  (org-catch-invisible-edits 'show-and-error)
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
  (org-use-sub-superscripts "{}")
  :config
  (add-to-list 'org-src-lang-modes '("json" . json-ts))
  (add-to-list 'org-src-lang-modes '("yaml" . yaml-ts)))

(use-package org-modern
  :ensure t
  :after org
  :custom
  (org-modern-table nil)
  :custom-face
  (org-modern-label ((t (:height 0.85))))
  :config
  (global-org-modern-mode))

(provide 'init-org)
