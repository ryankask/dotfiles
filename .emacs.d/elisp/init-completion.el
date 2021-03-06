;;; -*- lexical-binding: t; -*-

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(eval-and-compile
  (defun my-vertico-extensions-load-path ()
    (concat (file-name-directory (locate-library "vertico")) "extensions/")))

(use-package vertico-directory
  :load-path (lambda () (list (my-vertico-extensions-load-path)))
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

(defun my-consult-find-fd (&optional dir initial)
  (interactive "P")
  (let ((consult-find-command "fd --color=never --hidden --full-path ARG OPTS"))
    (consult-find dir initial)))

(defun my-consult-find-git (&optional dir initial)
  (interactive "P")
  (let ((consult-find-command "git ls-files --full-name OPTS -- *ARG*"))
    (consult-find dir initial)))

(defun my-consult-locate-mdfind (&optional initial)
  (interactive "P")
  (let ((consult-locate-command "mdfind -name OPTS ARG"))
    (consult-locate initial)))

(use-package consult
  :straight t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         ("C-c n" . consult-ripgrep)
         ("C-c e" . my-consult-find-git)
         ("C-c i" . my-consult-locate-mdfind)
         ("C-c o" . consult-git-grep)
         ("C-c b" . consult-bookmark)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ("M-s g" . consult-grep)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch) ;; orig. isearch-edit-string
         ("M-s l" . consult-line))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        register-preview-delay 0
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(provide 'init-completion)
