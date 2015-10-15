(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-work-file-list 250
      ido-max-dir-file-cache 250
      ido-ignore-extensions t
      ido-save-directory-list-file (expand-file-name "ido.hist" dotemacs-dir))
(ido-mode t)
(ido-ubiquitous-mode t)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(setq smex-save-file (expand-file-name "smex.hist" dotemacs-dir))
(setq smex-history-length 250)
(define-key my-kbs-map (kbd "M-x") 'smex)
(define-key my-kbs-map (kbd "M-X") 'smex-major-mode-commands)
(define-key my-kbs-map (kbd "C-c M-x") 'execute-extended-command) ;; old M-x

(provide 'init-ido)
