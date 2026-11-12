;;; md-preview.el --- Preview Markdown in eww via pandoc  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan Kaskel

;; Author: Ryan Kaskel <dev@ryankaskel.com>
;; Keywords: text, hypermedia

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

;; `md-preview-mode' renders the current buffer with pandoc and shows
;; the HTML in an eww buffer beside it.  The preview re-renders when
;; the buffer is saved, and keeps its scroll position.  Relative links
;; and images resolve against the Markdown file's directory.
;;
;; Nothing is written to disk: pandoc's output goes straight into eww.
;;
;; `md-preview' shows the preview, shows it again if its window was
;; closed, or closes it.  In the preview buffer, "g" re-renders from
;; the source buffer, including unsaved changes, and eww's "q" buries
;; the preview; it keeps rendering on save until `md-preview' shows it
;; again.

;;; Code:

(require 'eww)

(defgroup md-preview nil
  "Preview Markdown buffers in eww."
  :group 'text)

(defcustom md-preview-pandoc-program "pandoc"
  "The pandoc executable."
  :type 'string)

(defcustom md-preview-pandoc-args '("--from=gfm" "--to=html5")
  "Arguments passed to pandoc.  Markdown is read from stdin."
  :type '(repeat string))

(defcustom md-preview-display-action
  '((display-buffer-reuse-window display-buffer-in-direction)
    (direction . below)
    ;; Without a height, `display-buffer-in-direction' fits the window
    ;; to the preview, which is still empty when it is displayed.
    (window-height . 0.5)
    (inhibit-same-window . t))
  "The `display-buffer' action used to show the preview."
  :type 'sexp)

(defvar-local md-preview--buffer nil
  "In a source buffer, the eww buffer that previews it.")

(defvar-local md-preview--source nil
  "In a preview buffer, the Markdown buffer it previews.")

(defun md-preview--base-url ()
  "Return the URL that relative links in the current buffer resolve against."
  (concat "file://" (expand-file-name (or buffer-file-name default-directory))))

(defun md-preview--pandoc ()
  "Convert the current buffer with pandoc.
Return a unibyte buffer in the form `eww-render' expects: a
Content-type header, a blank line, then the HTML.  Return nil and
report the error if pandoc fails."
  (let ((html (generate-new-buffer " *md-preview html*"))
        (coding-system-for-write 'utf-8)
        (coding-system-for-read 'binary)
        status)
    (with-current-buffer html
      (set-buffer-multibyte nil)
      (insert "Content-type: text/html; charset=utf-8\n\n"))
    (save-restriction
      (widen)
      (setq status (apply #'call-process-region (point-min) (point-max)
                          md-preview-pandoc-program nil (list html nil) nil
                          md-preview-pandoc-args)))
    (if (eql status 0)
        html
      (kill-buffer html)
      (message "md-preview: %s exited with %s" md-preview-pandoc-program status)
      nil)))

(defun md-preview--tag-table (dom)
  "Render the table DOM as one table, laid out in the monospace font.

shr puts a table that has a header row inside a one-cell outer table,
and renders the inner table in a temporary buffer.  The preview's
buffer-local border settings then miss the inner table and frame the
outer one.  Lifting the rows out of thead, tbody and tfoot avoids the
nesting.

With proportional fonts, shr measures cells in pixels, and the
measurements can be off: long lines push their borders out of line,
and rulers stop short of their corners.  Turning `shr-use-fonts' off
lays the table out in characters of the default face instead.  shr's
widths are in pixels while fonts are on, so convert them."
  (let ((table (apply #'dom-node 'table (dom-attributes dom)
                      (seq-mapcat (lambda (child)
                                    (pcase (dom-tag child)
                                      ((or 'thead 'tbody 'tfoot)
                                       (dom-non-text-children child))
                                      ('tr (list child))))
                                  (dom-non-text-children dom)))))
    (if (not shr-use-fonts)
        (shr-tag-table table)
      (let* ((char-width (frame-char-width))
             (shr-internal-width (/ shr-internal-width char-width))
             (shr-indentation (/ shr-indentation char-width))
             (shr-table-separator-pixel-width 1)
             (shr-use-fonts nil))
        (shr-tag-table table)))))

(defun md-preview--tag-th (dom)
  "Render the table header cell DOM in bold, as GitHub does."
  (shr-fontize-dom dom 'bold))

(defun md-preview--render ()
  "Render the current buffer into its preview buffer.
Keep the scroll position of every window that shows the preview."
  (if (not (buffer-live-p md-preview--buffer))
      ;; The preview was killed, so stop previewing.
      (md-preview-mode -1)
    (when-let* ((preview md-preview--buffer)
                (url (md-preview--base-url))
                (html (md-preview--pandoc)))
      (let ((windows (mapcar (lambda (window)
                               (list window (window-start window) (window-point window)))
                             (get-buffer-window-list preview nil t)))
            (eww-auto-rename-buffer nil)
            ;; Let-bound, not buffer-local: shr renders table cells in
            ;; temporary buffers, which only see global values.
            (shr-external-rendering-functions
             (append '((table . md-preview--tag-table)
                       (th . md-preview--tag-th))
                     shr-external-rendering-functions)))
        ;; shr fills text to the selected window's width, so render from
        ;; a window that shows the preview.  `eww-render' reads the HTML
        ;; from the current buffer, draws it in PREVIEW, then kills the
        ;; HTML buffer.
        (with-selected-window (or (caar windows) (selected-window))
          (with-current-buffer html
            (eww-render nil url (with-current-buffer preview (point)) preview)))
        (pcase-dolist (`(,window ,start ,point) windows)
          (set-window-point window point)
          (set-window-start window start))))))

(defun md-preview--kill-preview ()
  "Delete the preview buffer of the current buffer, and its windows."
  (let ((preview md-preview--buffer))
    (setq md-preview--buffer nil)
    (when (buffer-live-p preview)
      (dolist (window (get-buffer-window-list preview nil t))
        (quit-restore-window window 'kill))
      (when (buffer-live-p preview)
        (kill-buffer preview)))))

;;;###autoload
(define-minor-mode md-preview-mode
  "Show a live eww preview of the current Markdown buffer.
The preview is rendered by pandoc and re-renders on each save."
  :lighter " Preview"
  (if (not md-preview-mode)
      (progn
        (remove-hook 'after-save-hook #'md-preview--render t)
        (remove-hook 'kill-buffer-hook #'md-preview--kill-preview t)
        (md-preview--kill-preview))
    (unless (executable-find md-preview-pandoc-program)
      (setq md-preview-mode nil)
      (user-error "Cannot find %s" md-preview-pandoc-program))
    (let ((source (current-buffer)))
      (setq md-preview--buffer
            (with-current-buffer (get-buffer-create
                                  (format "*md-preview: %s*" (buffer-name source)))
              (eww-mode)
              (md-preview-output-mode)
              (setq md-preview--source source)
              (current-buffer))))
    (add-hook 'after-save-hook #'md-preview--render nil t)
    (add-hook 'kill-buffer-hook #'md-preview--kill-preview nil t)
    ;; Display before rendering, so the text fills the preview's width.
    (display-buffer md-preview--buffer md-preview-display-action)
    (md-preview--render)))

;;;###autoload
(defun md-preview ()
  "Show a live eww preview of the current Markdown buffer, or close it.
If the preview is on but its window was closed, show it again."
  (interactive)
  (cond ((not md-preview-mode)
         (md-preview-mode 1))
        ((not (buffer-live-p md-preview--buffer))
         (md-preview-mode -1)
         (md-preview-mode 1))
        ((not (get-buffer-window md-preview--buffer t))
         (display-buffer md-preview--buffer md-preview-display-action)
         (md-preview--render))
        (t
         (md-preview-mode -1))))

(defun md-preview-refresh ()
  "Re-render the preview from the source buffer, including unsaved changes."
  (interactive)
  (let ((source (if (buffer-live-p md-preview--source)
                    md-preview--source
                  (current-buffer))))
    (with-current-buffer source
      (if md-preview-mode
          (md-preview--render)
        (user-error "No Markdown buffer to preview")))))

(defvar-keymap md-preview-output-mode-map
  :doc "Keymap for `md-preview-output-mode'."
  ;; eww's own reload would fetch the raw Markdown file.
  "g" #'md-preview-refresh)

(define-minor-mode md-preview-output-mode
  "Minor mode for eww buffers that show an `md-preview-mode' preview.
To draw table borders, set `shr-table-horizontal-line',
`shr-table-vertical-line' and `shr-table-corner' locally from this
mode's hook."
  :keymap md-preview-output-mode-map
  (when md-preview-output-mode
    ;; eww's header line would show "[untitled]" and the file's URL.
    (setq-local eww-header-line-format nil)))

(provide 'md-preview)
;;; md-preview.el ends here
