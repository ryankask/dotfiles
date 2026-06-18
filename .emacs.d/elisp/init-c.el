;; -*- lexical-binding: t; -*-

(defcustom my-c-ts-indent-style-google-continuation-offset 4
  "Continuation indent width for the Google C style.
Mirrors clang-format's `ContinuationIndentWidth' (4 for BasedOnStyle: Google).")

(defun my-c-ts-indent-style-google ()
  "Return indentation rules matching clang-format Google for C.
Built on `c-ts-mode's k&r base rules (its attached-brace style) with the
Google-specific deltas prepended so they take precedence.  The result is a
`treesit-simple-indent-rules' rule list without the leading language symbol,
suitable as the value of `c-ts-mode-indent-style'.

Inheriting the base keeps this list small and tracks upstream grammar-quirk
handling; the oracle test suite guards against any drift.  Note that `gnu'
and `k&r' produce identical base rules in `c-ts-mode'."
  (append
   ;; Google deltas, prepended (highest precedence first).
   `(;; switch: IndentCaseLabels=true, IndentCaseBlocks=false.
     ((node-is "case_statement") standalone-parent c-ts-indent-offset)
     ((n-p-gp nil "case_statement" nil) standalone-parent c-ts-indent-offset)
     ;; A closing brace aligns with the statement that owns it; ahead of the
     ;; continuation rules so initializer-list closing braces don't shift.
     ((node-is "}") c-ts-mode--standalone-parent 0)
     ;; Non-aligned continuations use ContinuationIndentWidth (4).
     ((parent-is "init_declarator")
      c-ts-mode--standalone-parent my-c-ts-indent-style-google-continuation-offset)
     ((parent-is "assignment_expression")
      c-ts-mode--standalone-parent my-c-ts-indent-style-google-continuation-offset)
     ((parent-is "initializer_list")
      c-ts-mode--standalone-parent my-c-ts-indent-style-google-continuation-offset))
   ;; k&r base rules from c-ts-mode (strip the leading language symbol).
   (alist-get 'c (c-ts-mode--simple-indent-rules 'c 'k&r))))

(defun my-c-ts-indent-style-google-setup ()
  "Apply the Google C indentation style in the current buffer.
Add this to `c-ts-mode-hook'."
  (setq-local indent-tabs-mode nil)
  (setq-local c-ts-indent-offset 2)
  (c-ts-mode-set-style #'my-c-ts-indent-style-google))

(defun my-c-ts-mode-hook ()
  (my-c-ts-indent-style-google-setup)
  (setq-local eglot-ignored-server-capabilities
              '(:documentOnTypeFormattingProvider))
  (eglot-ensure))

(use-package c-ts-mode
  :defer t
  :hook (c-ts-mode . my-c-ts-mode-hook))

(provide 'init-c)
