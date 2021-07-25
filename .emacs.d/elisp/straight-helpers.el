;;; -*- lexical-binding: t; -*-

(defun my-count-git-commits-behind (repo-dir)
  (with-temp-buffer
    (call-process "git" nil  t nil
                  "-C" repo-dir "rev-list" "HEAD..origin" "--count")
    (string-to-number (buffer-string))))

(defun my-list-straight-packages-with-update-counts ()
  (let ((results nil))
    (pcase-dolist
        (`(,path . ,file-attrs)
         (directory-files-and-attributes (straight--repos-dir) t))
      (when (and (nth 0 file-attrs)
                 (file-directory-p (concat (file-name-as-directory path) ".git")))
        (let ((behind-count (my-count-git-commits-behind path)))
          (if (> behind-count 0)
              (push (cons (file-name-nondirectory path) behind-count) results)))))
    results))

(defun my-print-straight-packages-with-updates ()
  "Print the list of straight.el packages that have available updates."
  (interactive)
  (let* ((updated (my-list-straight-packages-with-update-counts))
         (updated-count (length updated)))
    (with-output-to-temp-buffer "*outdated-straight-packages*"
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

(provide 'straight-helpers)
