;;;golink.el --- Interface into the golink service  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Ryan Kaskel

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

;;

;;; Code:

(require 'cl-lib)

(defcustom golink-base-url "http://go"
  "Base URL to golink server"
  :type 'string)

(defvar golink--history nil
  "History of selections made via `golink--select'")

(defun golink--read-json (api-path)
  (with-temp-buffer
    (url-insert-file-contents (concat golink-base-url api-path))
    (beginning-of-buffer)
    (json-parse-buffer)))

(defun golink--read-jsonl (api-path)
  (let ((items))
    (with-temp-buffer
      (url-insert-file-contents (concat golink-base-url api-path))
      (beginning-of-buffer)
      (while (not (eobp))
        (if-let* ((line (buffer-substring-no-properties (pos-bol) (pos-eol)))
                  (object (json-parse-string line)))
            (push object items))
        (forward-line))
      items)))

(defun golink--export ()
  (golink--read-jsonl "/.export"))

(defun golink--detail (short-name)
  (ignore-error file-error
    (golink--read-json (concat "/.detail/" short-name))))

(defun golink--request-data (form-data)
  (mapconcat
   (pcase-lambda (`(,key . ,value))
     (concat (url-hexify-string key) "=" (url-hexify-string value)))
   form-data
   "&"))

(defun golink--create (short-name target-url)
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("content-type" . "application/x-www-form-urlencoded")
           ("sec-fetch-site" . "same-origin")))
        (url-request-data (golink--request-data `(("short" . ,short-name)
                                                  ("long" . ,target-url)))))
    (golink--read-json "/")))

(defun golink--short-name (link)
  (gethash "short" link))

(defun golink--target-url (link)
  (gethash "long" link))

(defun golink--export-by-short-name ()
  (cl-reduce
   (lambda (table link)
     (puthash (golink--short-name link) link table)
     table)
   (golink--export)
   :initial-value (make-hash-table :test 'equal)))

(defun golink--completion-table ()
  (let* ((links (golink--export-by-short-name))
         (collection (hash-table-keys links))
         (annotate (lambda (string)
                     (format "\t%s"
                             (golink--target-url (gethash string links)))))
         (metadata `(metadata
                     (annotation-function . ,annotate))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          metadata
        (complete-with-action action collection string pred)))))

(defun golink--select (&optional prompt)
  (completing-read (or prompt "Go: ")
                   (golink--completion-table)
                   nil t nil 'golink--history))

(defun golink-url (short-name)
  "Given a SHORT-NAME, return a full golink."
  (concat golink-base-url "/" short-name))

(defun golink-open (short-name)
  "Open the URL for golink SHORT-NAME.

When called interactively, a selection is made from all available
short names."
  (interactive (list (golink--select)))
  (browse-url (golink-url short-name)))

(defun golink-create (url short-name)
  "Create a golink to URL using SHORT-NAME as its short
 identifier.

The arguments are flipped to support better integration with
Embark."
  (interactive "sURL: \nsShort name: ")
  (golink--create short-name url)
  (message "%s => %s" short-name url))

(provide 'golink)
;;; golink.el ends here
