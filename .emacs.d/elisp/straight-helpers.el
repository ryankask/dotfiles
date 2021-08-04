;;; -*- lexical-binding: t; -*-

(defun my-count-git-commits-behind (repo-dir)
  (with-temp-buffer
    (call-process "git" nil  t nil
                  "-C" repo-dir "rev-list" "HEAD..origin" "--count")
    (string-to-number (buffer-string))))

(defun my-list-straight-packages-with-update-counts ()
  (let ((package-counts nil))
    (straight--map-repos
     (lambda (recipe)
       (straight--with-plist recipe
           (package local-repo)
         (let ((commits-behind (my-count-git-commits-behind
                                (straight--repos-dir local-repo))))
           (when (> commits-behind 0)
             (push (cons package commits-behind) package-counts))))))
    (sort package-counts (lambda (a b) (funcall #'string< (car a) (car b))))))

(defcustom my-outdated-straight-packages-buffer-name "*outdated-straight-packages*"
  "Name of the buffer used to output outdated Striaght packages")

(defun my-print-straight-packages-with-updates ()
  "Print the list of straight.el packages that have available updates."
  (interactive)
  (let* ((updated (my-list-straight-packages-with-update-counts))
         (updated-count (length updated)))
    (with-output-to-temp-buffer my-outdated-straight-packages-buffer-name
      (princ (format "------Outdated Straight packages: %d-----\n" updated-count))
      (if (> updated-count 0)
          (pcase-dolist
              (`(,package . ,count) updated)
            (princ (format "%s: %s\n" package count)))))))

(defun my-fetch-and-print-straight-packages-with-updates ()
  "Fetch all repos managed by straight.el, then print those with available updates."
  (interactive)
  (straight-fetch-all)
  (my-print-straight-packages-with-updates))

(defun my-pull-straight-package-at-point ()
  (interactive)
  (when-let* ((symbol (symbol-at-point))
              (package (symbol-name symbol)))
    (straight-pull-package package)))

(provide 'straight-helpers)
