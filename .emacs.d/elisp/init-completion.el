;;; -*- lexical-binding: t; -*-

(setq completion-ignored-extensions
      (nconc completion-ignored-extensions '(".DS_Store" "__pycache__/")))

(use-package vertico
  :straight t
  :init
  (vertico-mode)
  ;; Make Vertico extensions available
  (add-to-list 'load-path
               (concat (file-name-directory (locate-library "vertico")) "extensions/")))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :straight t
  :custom (completion-styles '(orderless))
  :init
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))

(defun my-consult-fd-builder (input)
  (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler arg 'extended)))
    (when re
      (list :command (append
                      (list "fd" "--color=never" "--hidden" "--full-path"
                            (consult--join-regexps re 'extended))
                      opts)
            :highlight hl))))

(defun my-consult-fd (&optional dir initial)
  (interactive "P")
  (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
         (default-directory (cdr prompt-dir)))
    (find-file
     (consult--find (car prompt-dir) #'my-consult-fd-builder initial))))

(defun my-consult-mdfind-builder (input)
  (pcase-let ((`(,arg . ,opts) (consult--command-split input)))
    `(:command ("mdfind" "-name" ,arg ,@opts))))

(defun my-consult-mdfind (&optional initial)
  (interactive)
  (find-file
   (consult--find "Mdfind: " #'my-consult-mdfind-builder initial)))

(use-package consult
  :straight t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         ("C-c n" . consult-ripgrep)
         ("C-c e" . my-consult-fd)
         ("C-c i" . my-consult-mdfind)
         ("C-c o" . consult-git-grep)
         ("C-c b" . consult-bookmark)
         ("s-r s-r" . consult-register)
         ("s-r s-t" . consult-register-load)
         ("s-r s-s" . consult-register-store)
         ("s-l" . consult-goto-line)
         :map goto-map
         ("e" . consult-compile-error)
         ("f" . consult-flymake)
         ("g" . consult-goto-line)
         ("o" . consult-outline)
         ("m" . consult-mark)
         ("k" . consult-global-mark)
         ("i" . consult-imenu)
         ("I" . consult-project-imenu)
         :map search-map
         ("f" . consult-find)
         ("g" . consult-grep)
         ("G" . consult-git-grep)
         ("L" . consult-line-multi)
         ("m" . consult-multi-occur)
         ("k" . consult-keep-lines)
         ("u" . consult-focus-lines)
         ("e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch) ;; orig. isearch-edit-string
         ("M-s l" . consult-line))
  :custom
  (consult-project-root-function
   (lambda ()
     (when-let (project (project-current))
       (car (project-roots project)))))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :config
  (setq consult-ripgrep-args (concat consult-ripgrep-args " --hidden"))
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(defun my-open-file-in-finder (file)
  "Reveal FILE in finder. If FILE is a directory, open it directly in Finder so its contents are displayed instead of revealing it."
  (let ((expanded-file (expand-file-name file)))
    (call-process
     "open" nil 0 nil
     (if (not (file-directory-p expanded-file))
         "-R"
       "")
     expanded-file)))

(use-package embark
  :straight t
  :bind (("C-." . embark-act)
         ("s-." . embark-dwim)
         :map embark-file-map
         ("X" . my-open-file-in-finder))
  :custom (embark-indicators '(embark-which-key-indicator
                               embark-highlight-indicator
                               embark-isearch-highlight-indicator))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :straight t
  :after (embark consult))

(provide 'init-completion)
