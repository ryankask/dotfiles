;;; -*- lexical-binding: t; -*-

(defun my-straight-helpers--count-git-commits-behind (repo-dir)
  (with-temp-buffer
    (call-process "git" nil  t nil
                  "-C" repo-dir "rev-list" "HEAD..origin" "--count")
    (string-to-number (buffer-string))))

(defun my-straight-helpers--git-commits-behind (recipe)
  "Return the number of commits behind the tracked branch of a
repo for a straight repository is."
  (straight--with-plist recipe
      (package local-repo)
    (my-straight-helpers--count-git-commits-behind (straight--repos-dir local-repo))))

(defun my-straight-helpers--list-git-commits-behind ()
  "Return an alist containing packages and how many commits behind
the upstream branch they are."
  (let ((package-counts nil))
    (straight--map-repos
     (lambda (recipe)
       (let ((commits-behind (my-straight-helpers--git-commits-behind recipe)))
         (when (> commits-behind 0)
           (push (cons (plist-get recipe :package) commits-behind)
                 package-counts)))))
    (sort package-counts (lambda (a b) (funcall #'string< (car a) (car b))))))

(defcustom my-straight-helpers--outdated-packages-buffer-name
  "*outdated-straight-packages*"
  "Name of the buffer used to output outdated Striaght packages")

(defun my-straight-helpers--replace-number-on-line (update)
  "Replace the next number found on the current line UPDATE. UPDATE
takes should take the found number and returns the updated value.
This function moves the point and it is expected save-excursion
will used by the caller."
  (when-let* ((end (re-search-forward "[[:digit:]]+" (line-end-position)))
              (start (match-beginning 0))
              (current-count (string-to-number (buffer-substring-no-properties start end)))
              (next-count (funcall update current-count)))
    (let ((inhibit-read-only t))
      (replace-match (number-to-string next-count)))))

(defun my-straight-helpers--update-outdated-count (update)
  "Update the outdated package count using UPDATE. UPDATE takes
should take the current count and return the next count."
  (save-excursion
    (goto-char (point-min))
    (my-straight-helpers--replace-number-on-line update)))

(defun straight-helpers-display-packages-with-updates ()
  "Print the list of straight.el packages that have available
updates."
  (interactive)
  (let* ((updated (my-straight-helpers--list-git-commits-behind))
         (updated-count (length updated))
         (buffer (get-buffer-create my-straight-helpers--outdated-packages-buffer-name t)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "------Outdated Straight packages: %d-----\n" updated-count))
        (pcase-dolist (`(,package . ,count) updated)
          (insert (format "%s: %s\n" package count))))
      (my-straight-packages-mode)
      (display-buffer buffer))))

(defun my-straight-helpers-fetch-and-display-packages-with-updates ()
  "Fetch all repos managed by straight.el, then display those with
available updates."
  (interactive)
  (straight-fetch-all)
  (my-straight-helpers-display-packages-with-updates))

(defun my-straight-helpers--line-package ()
  "Return the straight package on the current line."
  (save-excursion
    (let ((line-end-pos (line-end-position)))
      (beginning-of-line)
      (when-let ((start (or (and (looking-at "[[:alpha:]]") (point))
                            (1- (re-search-forward "[[:alpha:]]" line-end-pos t))))
                 (end (1- (search-forward ":" line-end-pos t))))
        (buffer-substring-no-properties start end)))))

(defun my-straight-helpers--delete-current-line ()
  "Delete the whole line, including the newline."
  (let ((inhibit-read-only t))
    (delete-region (line-beginning-position) (1+ (line-end-position)))))

(defun my-straight-helpers--pull-line-package ()
  "Pull the latest changes for the package on the current line."
  (interactive)
  (when-let (package (my-straight-helpers--line-package))
    (straight-pull-package package)
    (let* ((recipe (straight--convert-recipe (intern package)))
           (behind-count (my-straight-helpers--git-commits-behind recipe)))
      (if (> behind-count 0)
          (save-excursion
            (goto-char (line-beginning-position))
            (my-straight-helpers--replace-number-on-line
             (lambda (&rest args) behind-count)))
        (my-straight-helpers--delete-current-line)
        (my-straight-helpers--update-outdated-count #'1-)))))

(defun my-straight-helpers--pull-all-outdated-packages ()
  "Pull the latest changes for all packages in the outdated
buffer."
  (interactive)
  (my-straight-helpers-display-packages-with-updates))

(defvar my-straight-packages-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "u") #'my-straight-helpers--pull-line-package)
    (define-key map (kbd "U") #'my-straight-helpers--pull-all-outdated-packages)
    map)
  "Keymap for managing straight packages.")

(define-derived-mode my-straight-packages-mode special-mode "Straight Packages"
  "Major mode for managing straight packages.

\\{my-straight-packages-mode-map}")

(provide 'straight-helpers)
