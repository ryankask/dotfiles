(define-key my-kbs-map (kbd "C-c l") 'org-store-link)
(define-key my-kbs-map (kbd "C-c a") 'org-agenda)
(define-key my-kbs-map (kbd "C-c M-d") 'org-date-from-calendar)

(setq org-log-done t)
(setq org-completion-use-ido t)
(setq org-agenda-files '("~/org/todo.org"))

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

(setq org-directory "~/org")

;; Capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key my-kbs-map (kbd "C-c c") 'org-capture)

;; Archive
(setq org-archive-location (concat org-directory "/archive/%s_archive::"))

(provide 'init-org)
