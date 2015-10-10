(define-key my-kbs-map (kbd "C-c l") 'org-store-link)
(define-key my-kbs-map (kbd "C-c a") 'org-agenda)
(define-key my-kbs-map (kbd "C-c M-d") 'org-date-from-calendar)

(setq org-log-done t
      org-completion-use-ido t
      org-directory "~/Dropbox/org"
      org-directory-as-directory (file-name-as-directory org-directory))

(defun my-org-path (relpath)
  (concat org-directory-as-directory relpath))

(defun org-summary-todo (n-done n-not-don)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states) ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; Set alternative "TODO" states
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "REQUEST(q)" "|" "FIXED(f)" "DONE(d)")
        (sequence "|" "CANCELED(c)" "DUPLICATE(2)" "MOVED(m)")))

;; Agenda
(setq org-agenda-files `(,(my-org-path "agenda.org")))

;; Capture
(setq org-default-notes-file (my-org-path "notes.org"))
(define-key my-kbs-map (kbd "C-c c") 'org-capture)

;; Archive
(setq org-archive-location (my-org-path "archives/%s_archive::"))

(provide 'init-org)
