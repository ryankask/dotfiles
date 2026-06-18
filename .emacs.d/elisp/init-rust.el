;;; -*- lexical-binding: t; -*-

(defun my-rustic-mode-setup ()
  (abbrev-mode 1)
  (subword-mode 1)
  (setq-local buffer-save-without-query t)
  (my-yasnippet-capf-install)
  (setq eglot-workspace-configuration
        '(:rust-analyzer (:check (:command "clippy"))))
  (eglot-ensure))

(use-package rust-mode
  :ensure t
  :defer t)

(defun my-remove-rust-mode-from-auto-mode-alist ()
  "Remove rust-mode from `auto-mode-alist' after Elpaca fetches packages.

This prevents rust-mode's autoload from overriding rustic-mode for Rust
files."
  (let ((entry '("\\.rs\\'" . rust-mode)))
    (when (member entry auto-mode-alist)
      (setq auto-mode-alist (remove entry auto-mode-alist)))))

(use-package rustic
  :ensure (:host github :repo "emacs-rustic/rustic")
  :after rust-mode
  :bind (:map rustic-compilation-mode-map
              ("C-o" . nil))
  :hook (rustic-mode . my-rustic-mode-setup)
  :custom
  (rustic-lsp-setup-p nil)
  :config
  (add-hook 'elpaca-post-queue-hook
            #'my-remove-rust-mode-from-auto-mode-alist))

(defun my-append-decimal-to-integers (start end)
  "Append .0 to all integer literals in the region."
  (interactive "*r")
  (save-excursion
    (goto-char start)
    (save-restriction
      (narrow-to-region start end)
      (while (re-search-forward "\\(-?\\b[0-9]+\\b\\)\\([^.0-9]\\|\\'\\)" nil t)
        (unless (eq (char-before (match-beginning 1)) ?.)
          (replace-match "\\1.0\\2"))))))

(provide 'init-rust)
