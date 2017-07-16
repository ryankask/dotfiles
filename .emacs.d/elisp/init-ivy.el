(defun my-ivy-dir-p (filename)
  (string-match-p "/\\'" filename))

(defun my-ivy-dir-match (a b)
  (string-match-p b a))

(defun my-ivy-dir-recentf-index (f)
  (seq-position recentf-list (concat ivy--directory f) #'my-ivy-dir-match))

(defun my-ivy-sort-file-function (x y)
  ;; (message "debug:\n  x=%s\n  fx=%s\n  x-dirp=%s\n  x-my-dir-p=%s\n  y=%s\n  y-dirp=%s\n  y-my-dir-p=%s"
  ;;          x
  ;;          (concat ivy--directory x)
  ;;          (get-text-property 0 'dirp x)
  ;;          (my-ivy-dir-p x)
  ;;          y
  ;;          (concat ivy--directory y)
  ;;          (get-text-property 0 'dirp y)
  ;;          (my-ivy-dir-p y))
  (if (my-ivy-dir-p x)
      (if (my-ivy-dir-p y)
          (let* ((x-recentf-index (my-ivy-dir-recentf-index x))
                 (y-recentf-index (and x-recentf-index (my-ivy-dir-recentf-index y))))
            (cond ((and x-recentf-index y-recentf-index) (< x-recentf-index y-recentf-index))
                  ((or x-recentf-index y-recentf-index) (and x-recentf-index t))
                  (t (string< x y))))
        t)
    (if (my-ivy-dir-p y)
        nil
      ;; Sort files by mtime
      (time-less-p (nth 5 (file-attributes y))
                   (nth 5 (file-attributes x))))))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-j" . ivy-immediate-done)
         ("RET" . ivy-alt-done))
  :init
  (setq ivy-count-format "(%d/%d) "
        ivy-extra-directories nil
        ivy-format-function 'ivy-format-function-arrow
        ivy-height 11
        ivy-use-virtual-buffers t)
  :config
  ;;(add-to-list 'ivy-sort-functions-alist '(read-file-name-internal . my-ivy-sort-file-function))
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package smex
  :ensure t
  :defer t
  :init
  (setq smex-save-file (expand-file-name "smex.hist" dotemacs-dir)
        smex-history-length 32))

(defun my-counsel-ignore-regexp-builder (&rest regexp-units)
  (format "\\`%s\\'" (mapconcat
                      (lambda (unit)
                        (format "\\(?:%s\\)" unit))
                      regexp-units
                      "\\|")))

(defun my-counsel-ignore-extensions (&rest extensions)
  (format ".*\\.\\(?:%s\\)" (string-join extensions "\\|")))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate))
  :init
  (setq counsel-find-file-ignore-regexp
        (my-counsel-ignore-regexp-builder
         "\\.git"
         "__pycache__"
         "\\.DS_Store"
         "\\.#.*"
         (my-counsel-ignore-extensions "pyc" "elc"))))

(provide 'init-ivy)
