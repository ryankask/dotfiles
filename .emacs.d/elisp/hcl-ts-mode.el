;;; hcl-ts-mode.el --- Major mode for HCL, the HashiCorp Configuration Language  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ryan Kaskel

;; Author: Ryan Kaskel <dev@ryankaskel.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This mode uses emacs's native tree sitter support to provide syntax
;; highlighting

;; Most of the tree-sitter queries are borrowed from
;; https://github.com/nvim-treesitter/nvim-treesitter/blob/master/queries/hcl/highlights.scm

;; Some non-tree-sitter configuration is borrowed from
;; https://github.com/purcell/emacs-hcl-mode

;;; Code:

(require 'treesit)

(defcustom hcl-ts-mode-indent-offset 2
  "Indentation offset for `hcl-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe #'integerp
  :group 'hcl)

(defconst hcl-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (prog1
        table
      (modify-syntax-entry ?_ "_" table)
      (modify-syntax-entry ?- "_" table)
      (modify-syntax-entry ?= "." table)

      ;; Comments
      ;; Single line comment
      (modify-syntax-entry ?# "< b" table)
      (modify-syntax-entry ?\n "> b" table)

      ;; multiple line comment(/* ... */) taken from `go-mode'
      (modify-syntax-entry ?/ ". 124b" table)
      (modify-syntax-entry ?* ". 23" table)))
  "Syntax table for `hcl-ts-mode'.")

(defvar hcl-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'hcl
   :feature 'general                    ; TODO: categorise by feature
   '(["!" "\*" "/" "%" "\+" "-" ">" ">=" "<" "<=" "==" "!=" "&&" "||"] @font-lock-operator-face
     ["{" "}" "[" "]" "(" ")"] @font-lock-bracket-face
     ["." ".*" "," "[*]"] @font-lock-delimiter-face
     [(ellipsis) "\?" "=>"] @font-lock-punctuation-face
     [":" "="] @font-lock-misc-punctuation-face
     ["for" "endfor" "in"] @font-lock-operator-face
     ["if" "else" "endif"] @font-lock-operator-face
     [(quoted_template_start)
      (quoted_template_end)
      (template_literal)] @font-lock-string-face
     [(heredoc_identifier)
      (heredoc_start)] @font-lock-delimiter-face
     [(template_interpolation_start)
      (template_interpolation_end)
      (template_directive_start)
      (template_directive_end)
      (strip_marker)] @font-lock-punctuation-face
     (numeric_lit) @font-lock-number-face
     (bool_lit) @font-lock-constant-face
     (null_lit) @font-lock-constant-face
     (comment) @font-lock-comment-face
     (identifier) @font-lock-variable-name-face
     (body (block (identifier) @font-lock-keyword-face))
     (body (block (body (block (identifier) @font-lock-type-face))))
     (function_call (identifier) @font-lock-function-call-face)
     (attribute (identifier) @font-lock-property-name-face)
     (object_elem key: (expression (variable_expr (identifier) @font-lock-property-name-face)))
     (expression (variable_expr (identifier) @font-lock-builtin-face)
                 (get_attr (identifier) @font-lock-variable-use-face)))

   :language 'hcl
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face)))

(defvar hcl-ts-mode--indent-rules
  `((hcl
     ((node-is "block_end") parent-bol 0)
     ((node-is "object_end") parent-bol 0)
     ((node-is "tuple_end") parent-bol 0)
     ((parent-is "block") parent-bol ,hcl-ts-mode-indent-offset)
     ((parent-is "object") parent-bol ,hcl-ts-mode-indent-offset)
     ((parent-is "tuple") parent-bol ,hcl-ts-mode-indent-offset)
     ((parent-is "function_call") parent-bol ,hcl-ts-mode-indent-offset)
     (no-node parent-bol 0))))

(defvar-keymap hcl-ts-mode-map
  :doc "Keymap used for the tree-sitter powered HCL mode")

;;;###autoload
(define-derived-mode hcl-ts-mode prog-mode "HCL"
  "Major mode for editing HashiCorp Configuration Language (HCL) files

\\{hcl-ts-mode-map}"
  :group 'hcl
  :syntax-table hcl-ts-mode--syntax-table

  (cond
   ((treesit-ready-p 'hcl)
    ;; tree-sitter setup
    (treesit-parser-create 'hcl)
    (setq-local treesit-font-lock-settings hcl-ts-mode--font-lock-settings
                treesit-font-lock-feature-list '((general error))
                treesit-simple-indent-rules hcl-ts-mode--indent-rules)
    (treesit-major-mode-setup)

    ;; Other
    (setq-local comment-start "#"
                comment-start-skip "\\(//+\\|/\\*+\\)\\s *"))
   (t
    (message "tree-sitter is not ready for HCL"))))

(provide 'hcl-ts-mode)
;;; hcl-ts-mode.el ends here
