;;; use-package-helpers.el --- My use-package helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Ryan Kaskel

;; Author: Ryan Kaskel <dev@ryankaskel.com>
;; Keywords: convenience

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

;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defun uph--in-custom-section-p ()
  "Return t if the current point is in a :custom section"
  (save-excursion
    (ignore-error scan-error
      (cl-loop until (bobp)
               do (backward-sexp)
               if (looking-at ":\\([[:alnum:]]+\\)$")
                 return (string= (match-string 1) "custom")))))

(defun uph-custom-eval-last-sexp ()
  "Eval setopt on the preceding sexp in a :custom section. Return
nil if the user isn't in a :custom section."
  (interactive)
  (when-let* ((expr (and (uph--in-custom-section-p)
                         `(setopt ,@(elisp--preceding-sexp)))))
    (message (format "Evaluating `%S`" expr))
    (eval expr)
    t))

(defun uph-eval-last-sexp-advice (&rest args)
  "Wrap `uph-custom-eval-last-sexp' for :before-until advice."
  (uph-custom-eval-last-sexp))

(provide 'use-package-helpers)
;;; use-package-helpers.el ends here
