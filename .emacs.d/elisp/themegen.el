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
(require 'doric-themes)

(defvar themegen-all-themes
  (append modus-themes-collection ef-themes-collection doric-themes-collection)
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
     ((string-prefix-p "doric-" theme-name) #'my-doric-themes-get-color-value)
     (t (error "unknown theme type: %s" theme-name)))))

(defun themegen--get-theme-color (theme keys get-color-value-func)
  "Return the first key in KEYS that, when passed to the
GET-COLOR-VALUE-FUNC function along with THEME, does not yield
`unspecified'. If no such key is found, the function returns
`unspecified'."
  (or (cl-some (lambda (key)
                 (if (stringp key)
                     key
                   (let ((color (funcall get-color-value-func key nil theme)))
                     (unless (eq color 'unspecified) color))))
               keys)
      'unspecified))

(defun themegen--build-theme-values (theme map)
  "Use MAP to translate the values of Prot's emacs THEME to an alist
of theme keys and values suitable for another application.

Load THEME if it hasn't yet been loaded, otherwise its palette
won't be available."
  (unless (memq theme custom-known-themes)
    (load-theme theme :no-confirm :no-enable))
  (let ((get-color-value-func (themegen--get-color-value-function theme)))
    (mapcar
     (pcase-lambda (`(,conf-key . ,theme-keys))
       (cons conf-key
             (themegen--get-theme-color theme theme-keys get-color-value-func)))
     map)))

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
  "## author: Ryan Kaskel\n## license: GNU GPLv3\n## blurb: Powered by Protesilaos Stavrou's emacs themes"
  "Default metadata for kitty themes")

(defvar themegen-kitty-colors-alist
  '(("background" bg-main)
    ("foreground" fg-main)
    ("cursor" cursor)
    ("cursor_text_color" bg-main)
    ("selection_foreground" fg-region fg-shadow-intense fg-main)
    ("selection_background" bg-region bg-shadow-intense)
    ("macos_titlebar_color" "background")
    ;; black
    ("color0" "#000000")
    ;; light black
    ("color8" "#595959")
    ;; red
    ("color1" red fg-faint-red)
    ;; light red
    ("color9" red-warmer fg-faint-red)
    ;; green
    ("color2" green fg-faint-green)
    ;; light green
    ("color2" green-cooler fg-faint-green)
    ;; yellow
    ("color3" yellow fg-faint-yellow)
    ;; light yellow
    ("color11" yellow-warmer fg-faint-yellow)
    ;; blue
    ("color4" blue fg-faint-blue)
    ;; light blue
    ("color12" blue-warmer fg-faint-blue)
    ;; magenta
    ("color5" magenta fg-faint-magenta)
    ;; light magenta
    ("color13" magenta-cooler fg-faint-magenta)
    ;; cyan
    ("color6" cyan fg-faint-cyan)
    ;; light cyan
    ("color14" cyan-cooler fg-faint-cyan)
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

(defun themegen--generate-kitty-rc-set-colors-args (theme)
  "Generate a list of color options to send to kitty's `set-colors'
remote control command."
  (delq nil
        (mapcar
         (pcase-lambda (`(,key . ,color))
           (unless (or (eq color 'unspecified)
                       (equal color "background"))
             (format "%s=%s" key color)))
         (themegen--build-theme-values theme themegen-kitty-colors-alist))))

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
    ("hl" accent-0 fg-accent)
    ("fg+" fg-main)
    ("bg+" bg-completion bg-shadow-intense)
    ("hl+" accent-0 fg-accent)
    ("info" fg-prompt prompt fg-accent)
    ("border" border)
    ("prompt" fg-prompt prompt fg-accent)
    ("pointer" fg-prompt prompt fg-accent)
    ("marker" fg-prompt prompt fg-accent)
    ("spinner" comment fg-accent)
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

(defvar themegen-fzf-rc-file-path (expand-file-name "~/.zshrc")
  "Path to shell rc file that contains the `FZF_DEFAULT_OPTS'
export.")

(defun themegen--find-fzf-default-opts-export ()
  "Find and return the `FZF_DEFAULT_OPTS' export in the current
buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "export FZF_DEFAULT_OPTS=\\\"\\(?:[^\"]\\|\n\\)+\\\"" nil t)
      (match-string-no-properties 0))))

(defun themegen--extract-fzf-default-opts-export (&optional file)
  "Find and return the full shell export of `FZF_DEFAULT_OPTS'."
  (with-temp-buffer
    (insert-file-contents (expand-file-name (or file themegen-fzf-rc-file-path)))
    (themegen--find-fzf-default-opts-export)))

(defvar themegen--fzf-color-option-regexp
  "\\(--color=\\(?:[a-z+]+:\\(?:#[A-Za-z0-9]\\{6\\}\\|-1\\)+,?\\)+\\)"
  "Regular expression to find the FZF color option in a string.")

(defun themegen--replace-fzf-color-option (export theme)
  "Replace the --color option in EXPORT with the value for THEME."
  (let ((option (themegen-generate-fzf-color-option theme)))
    (with-temp-buffer
      (insert export)
      (goto-char (point-min))
      (when (re-search-forward themegen--fzf-color-option-regexp nil t)
        (replace-match option)
        (buffer-string)))))

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
      (when (re-search-forward themegen--fzf-color-option-regexp nil t)
        (replace-match option)
        (write-file rc)
        (revert-buffer t t)
        (themegen--find-fzf-default-opts-export)))))

;; Activation

(defvar themegen-kitty-socket "unix:/tmp/kitty"
  "Path to a kitty socket")

(defvar themegen-kitty-command-buffer-name " *kitty-command*"
  "Name of the buffer associated with kitty command processes")

(defun themegen--run-kitty-command (&rest args)
  (apply #'start-process
         `("kitty-cmd" ,themegen-kitty-command-buffer-name
           "kitty" "@" "--to" ,themegen-kitty-socket ,@args)))

(defun themegen-activate-kitty-theme (theme &optional save-conf no-export-fzf-opts)
  "Activate THEME using kitty's remote control feature. If SAVE-CONF
is non-nil, it will use Kitty's theme selection mechanism to save
the theme for future sessions. Otherwise, the theme will only be
active for the current session.

If NO-EXPORT-FZF-OPTS is non-nil, `FZF_DEFAULT_OPTS' won't be
 exported in the active kitty window."
  (interactive (list (themegen--select-theme) (y-or-n-p "Save? ") nil))
  (if save-conf
      (themegen--run-kitty-command "kitten" "themes" (symbol-name theme))
    (apply #'themegen--run-kitty-command
           `("set-colors" "--all" "--configured"
             ,@(themegen--generate-kitty-rc-set-colors-args theme))))
  (when-let* (((not no-export-fzf-opts))
              (export (if save-conf
                          (themegen-set-fzf-default-opts-color-option theme)
                        (themegen--replace-fzf-color-option
                         (themegen--extract-fzf-default-opts-export)
                         theme))))
    (themegen--run-kitty-command "send-text" (concat export ""))))

(provide 'themegen)
;;; themegen.el ends here
