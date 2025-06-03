;;; 1password.el --- Use 1Password from emacs        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ryan Kaskel

;; Author: Ryan Kaskel <dev@ryankaskel.com>
;; Keywords: tools

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

;; This package mirrors the 1Password CLI commands and subcommands to
;; enable access from emacs.

;; Only `op item get` and `op read` are supported at the moment.

;;; Code:

(require 'memoize)

(defun 1p--executable ()
  "Return the path to the 1Password CLI program"
  (or (executable-find "op")
      (error "The 1Password CLI tool is required but wasn't found.")))

(memoize '1p--executable nil)

(defun 1p--run (&rest args)
  "Run the 1Password CLI tool with ARGS and return the output as a string."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process (1p--executable) nil t nil args)))
      (if (> exit-code 0)
          (error "1Password CLI error: %s" (buffer-string))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun 1p--run-json (&rest args)
  "Run the 1Password CLI tool with ARGS, parsing the result as JSON."
  (json-parse-string (apply #'1p--run args)))

(defun 1p-item-get (name &optional fields vault)
  "Get an item from 1Password by NAME.

NAME can be an item's name, ID, or a share link. FIELDS is an alist
 where the key is either the symbol `label' or `type' and the value is a
 string. This is used to specify which fields are returned."
  (interactive)
  (apply #'1p--run-json
         (append (list "item" "get" "--format" "json" "--reveal" name)
                 (when fields
                   (list "--fields"
                         (string-join
                          (mapcar
                           (lambda (entry)
                             (format "%s=%s" (car entry) (cdr entry)))
                           fields)
                          ",")))
                 (when vault
                   (list "--vault" vault)))))

(memoize #'1p-item-get "1 hour")

(defun 1p-read (ref)
  "Read the value from REF from 1Password given the secret reference."
  (interactive)
  (1p--run "read" "--no-newline" ref))

(memoize #'1p-read "1 hour")

(provide '1password)
;;; 1password.el ends here
