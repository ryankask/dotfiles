;;; themegen.el --- Derive Prot's themes for other software  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ryan Kaskel

;; Author: Ryan Kaskel <dev@ryankaskel.com>
;; Keywords: tools, tools, tools

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

;; Derive themes for other software using the colour palettes from the
;; Prot's modus-themes and ef-themes packages.

;;; Code:

(require 'cl-lib)
(require 'modus-themes)
(require 'ef-themes)

(defvar themegen-all-themes
  (append modus-themes-collection ef-themes-collection)
  "List of all of Prot's themes")

(defvar themegen--select-theme-history nil
  "Theme selection history")

(defun themegen--select-theme (&optional prompt)
  "Prompt the user for `themegen-all-themes'."
  (intern
   (completing-read
    (or prompt "Select theme: ")
    themegen-all-themes
    nil t nil
    'themegen--select-theme-history)))

(defun themegen--get-color-value-function (theme)
  "Determine which function to use to map named colours to hex
values for THEME."
  (let ((theme-name (symbol-name theme)))
    (cond
     ((string-prefix-p "modus-" theme-name) #'modus-themes-get-color-value)
     ((string-prefix-p "ef-" theme-name) #'ef-themes-get-color-value)
     (t (error "unknown theme type: %s" theme-name)))))

(defun themegen--get-theme-color (theme keys get-color-value-func)
  "Return the first key in KEYS that, when passed to the
GET-COLOR-VALUE-FUNC function along with THEME, does not yield
`unspecified'. If no such key is found, the function returns
`unspecified'."
  (or (cl-some (lambda (key)
                 (cond
                  ((stringp key) key)
                  (t (let ((color (funcall get-color-value-func key nil theme)))
                       (if (eq color 'unspecified) nil color)))))
               keys)
      'unspecified))

(defun themegen--build-theme-values (theme map)
  "Use MAP to translate the values of Prot's emacs THEME to an alist
of theme keys and values suitable for another application."
  (let ((get-color-value-func (themegen--get-color-value-function theme)))
    (mapcar
     (pcase-lambda (`(,kitty-key . ,theme-keys))
       (cons kitty-key
             (themegen--get-theme-color theme theme-keys get-color-value-func)))
     (or map themegen-kitty-colors-alist))))

(defun themegen--format-config (theme map config-format)
  "Generate a config string for THEME using translation MAP and
formatted with CONFIG-FORMAT.

CONFIG-FORMAT is a function that takes a key and value and returns a
 string formatted for an app's theming configuration."
  (let ((items (themegen--build-theme-values theme map)))
    (with-temp-buffer
      (pcase-dolist (`(,key . ,value) items)
        (when (not (eq value 'unspecified))
          (insert (funcall config-format key value))))
      (buffer-string))))

;; Kitty

(defvar themegen-kitty-themes-dir
  (expand-file-name ".config/kitty/themes"
                    (or (getenv "DOTFILES") (error "DOTFILES env var not found")))
  "Default directory to store generated kitty themes.")

(defvar themegen-kitty-theme-metadata
  "## author: Protesilaos Stavrou\n## license: GNU GPLv3"
  "Default metadata for kitty themes")

(defvar themegen-kitty-colors-alist
  '(("background" bg-main)
    ("foreground" fg-main)
    ("cursor" cursor)
    ("cursor_text_color" bg-main)
    ("selection_foreground" fg-region)
    ("selection_background" bg-region)
    ("macos_titlebar_color" "background")
    ;; black
    ("color0" "#000000")
    ;; light black
    ("color8" "#595959")
    ;; red
    ("color1" red)
    ;; light red
    ("color9" red-warmer)
    ;; green
    ("color2" green)
    ;; light green
    ("color2" green-cooler)
    ;; yellow
    ("color3" yellow)
    ;; light yellow
    ("color11" yellow-warmer)
    ;; blue
    ("color4" blue)
    ;; light blue
    ("color12" blue-warmer)
    ;; magenta
    ("color5" magenta)
    ;; light magenta
    ("color13" magenta-cooler)
    ;; cyan
    ("color6" cyan)
    ;; light cyan
    ("color14" cyan-cooler)
    ;; light grey
    ("color7" "#a6a6a6")
    ;; dark grey
    ("color15" "#ffffff"))
  "Map kitty theme keys to the colours of the Modus/EF theme palettes.")

(defun themegen--format-kitty-config (theme)
  "Generate a partial config file string for kitty with the appropriate
items for THEME."
  (themegen--format-config
   theme
   themegen-kitty-colors-alist
   (lambda (key value) (format "%s %s\n" key value))))

(defun themegen--save-kitty-theme-file (theme &optional dir)
  "Save the converted THEME to DIR. If DIR is nil,
 `themegen-kitty-themes-dir' is used."
  (write-region
   (format
    "%s\n\n%s"
    themegen-kitty-theme-metadata
    (themegen--format-kitty-config theme))
   nil
   (expand-file-name (format "%s.conf" theme) (or dir themegen-kitty-themes-dir))))

(defun themegen-generate-kitty-themes ()
  "Generate kitty theme files for all themes available in the
 modus-themes and ef-themes collections."
  (interactive)
  (dolist (theme themegen-all-themes)
    (themegen--save-kitty-theme-file theme)))

;; fzf

(defvar themegen-fzf-colors-alist
  '(("fg" fg-main)
    ("bg" bg-main)
    ("hl" accent-0)
    ("fg+" fg-main)
    ("bg+" bg-completion)
    ("hl+" accent-0)
    ("info" fg-prompt prompt)
    ("border" border)
    ("prompt" fg-prompt prompt)
    ("pointer" fg-prompt prompt)
    ("marker" fg-prompt prompt)
    ("spinner" comment)
    ("header" fg-dim)
    ("gutter" "-1"))
  "Map of fzf color keys to the colours of the Modus/EF theme palettes.")

(defun themegen-generate-fzf-color-option (theme &optional copy-to-clipboard)
  "Generate an FZF --color option for THEME."
  (interactive (list (themegen--select-theme) current-prefix-arg))
  (let* ((value (themegen--format-config
                 theme
                 themegen-fzf-colors-alist
                 (lambda (key value) (format "%s:%s," key value))))
         (option (format "--color=%s" (string-trim-right value ","))))
    (if copy-to-clipboard
        (gui-set-selection 'CLIPBOARD option)
      option)))

(defun themegen--extract-fzf-default-opts-export ()
  "Find and return the full shell export of `FZF_DEFAULT_OPTS'."
  (save-excursion
    (goto-char (point-min))
    (message "in here")
    (when (re-search-forward
           "export FZF_DEFAULT_OPTS=\\\"\\(?:[^\"]\\|\n\\)+\\\""
           nil t)
      (message "got match")
      (match-string-no-properties 0))))

(defun themegen-set-fzf-default-opts-color-option (theme &optional file)
  "Replace the --color option for the exported FZF_DEFAULT_OPTS
 environment variable in FILE withe value for THEME."
  (interactive
   (list
    (themegen--select-theme)
    (read-file-name "zshrc:" "~/" nil t ".zshrc")))
  (let ((option (themegen-generate-fzf-color-option theme))
        (rc (expand-file-name (or file "~/.zshrc"))))
    (with-temp-buffer
      (insert-file-contents rc nil nil nil t)
      (goto-char (point-min))
      (when (re-search-forward
             "\\(--color=\\(?:[a-z+]+:\\(?:#[A-Za-z0-9]\\{6\\}\\|-1\\)+,?\\)+\\)"
             nil t)
        (replace-match option)
        (write-file rc)
        (revert-buffer t t)
        ;; Extract and return full export
        (themegen--extract-fzf-default-opts-export)))))

;; Activation

(defvar themegen-kitty-socket "unix:/tmp/kitty"
  "Path to a kitty socket")

(defvar themegen-kitty-command-buffer-name " *kitty-command*"
  "Name of the buffer associated with kitty command processes")

(defun themegen--run-kitty-command (&rest args)
  (apply #'start-process
         `("kitty-cmd" ,themegen-kitty-command-buffer-name
           "kitty" "@" "--to" ,themegen-kitty-socket ,@args)))

(defun themegen-activate-kitty-theme (theme &optional no-export-fzf-opts)
  "Activate THEME using kitty's remote control feature.

If NO-EXPORT-FZF-OPTS is non-nil, `FZF_DEFAULT_OPTS' won't be
 exported in the active kitty window."
  (interactive (list (themegen--select-theme) current-prefix-arg))
  (themegen--run-kitty-command "kitten" "themes" (symbol-name theme))
  (when-let ((not no-export-fzf-opts)
             (var-export (themegen-set-fzf-default-opts-color-option theme)))
    (themegen--run-kitty-command "send-text" (concat var-export ""))))


(provide 'themegen)
;;; themegen.el ends here
