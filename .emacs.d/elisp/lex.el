;;; lex.el --- Utilities for words, phrases, and sentences  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ryan Kaskel

;; Author: Ryan Kaskel <dev@ryankaskel.com>
;; Keywords: text

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

;; Lexical utilities to assist with writing. Some are powered by LLMs
;; via gptel.

;;; Code:
(require 'gptel)
(require 'thingatpt)

(defun lex--extract-surrunding-text (marker num-lines max-chars)
  "Return NUM-LINES of text before and after marker, up to a maximum of
MAX-CHARS."
  (save-excursion
    (goto-char marker)
    (let* ((beg (progn (forward-line (- num-lines)) (point)))
           (end (progn (goto-char marker)
                       (forward-line num-lines)
                       (end-of-line)
                       (point)))
           (marker-pos (- marker beg))
           (text (buffer-substring-no-properties beg end)))
      (if (> (length text) max-chars)
          (let* ((half (/ max-chars 2))
                 (beg (max 0 (- marker-pos half)))
                 (end (min (length text) (+ marker-pos half))))
            (substring text beg end))
        text))))

(defun lex--build-context (marker &optional num-lines max-chars)
  "Return a plist containing contextual information about MARKER."
  (if (not (markerp marker))
      (error "Expected a marker, got %S" marker)
    (let ((buf (marker-buffer marker)))
      (with-current-buffer buf
        (list :buffer-name (buffer-name buf)
              :filename (buffer-file-name buf)
              :major-mode major-mode
              :surrounding-text (lex--extract-surrunding-text
                                 marker
                                 (or num-lines 3)
                                 (or max-chars 1000)))))))

(defun lex--context-to-string (context)
  "Convert a context plist to a string."
  (cl-loop for (key val) on context by #'cddr
           concat (format "%s: %s\n" key val)))

(defun lex--generate-alternatives (text context callback)
  "Use an LLM to generate a list of alternatives for TEXT"
  (let ((gptel-backend (gptel-get-backend "Claude"))
        (gptel-model "claude-haiku-4-5-20251001"))
    (gptel-request (concat "Text: " text "\n\nContext: " context)
      :system "List 3-7 alternative phrasings for the given text (word, \
 phrase, or sentence). If context is provided, use it to generate \
 contextually appropriate alternatives. Output format: one alternative \
 per line, no bullets or numbering. Match the input's capitalization \
 style and preserve any surrounding punctuation. If no suitable \
 alternatives exist, return a single line containing only \".\""
      :callback (lambda (response info)
                  (funcall callback
                           (and response
                                (not (string= response "."))
                                (string-split response "\n" t)))))))

;;;###autoload
(defun lex-alternatives (bounds)
  (interactive
   (list
    (cond
     ((use-region-p) (cons (region-beginning) (region-end)))
     ((bounds-of-thing-at-point 'symbol))
     (t (user-error "no content at point")))))
  (when-let* ((_ bounds)
              (beg (copy-marker (car bounds)))
              (end (copy-marker (cdr bounds)))
              (text (buffer-substring-no-properties beg end))
              (context (lex--context-to-string (lex--build-context beg)))
              (orig-point (point)))
    (lex--generate-alternatives
     text
     context
     (lambda (results)
       (if-let* ((_ results)
                 (buf (marker-buffer beg))
                 (alternative (with-local-quit
                                (completing-read "Replace: " results nil t))))
           (with-current-buffer buf
             (delete-region beg end)
             (goto-char beg)
             (insert alternative)
             (when (= orig-point beg)
               (goto-char beg))
             (deactivate-mark)
             (set-marker beg nil)
             (set-marker end nil)
             ;; HACK: Prevent cursor blink issues caused by invoking
             ;; `completing-read' within event handlers, which
             ;; interferes with blink-cursor-mode.
             (when blink-cursor-mode
               (blink-cursor-start)))
         (message "Alternatives not found"))))))

(provide 'lex)
;;; lex.el ends here
