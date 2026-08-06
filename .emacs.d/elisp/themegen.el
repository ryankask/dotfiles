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
  (cond
   ((modus-themes-known-p theme) #'modus-themes-get-color-value)
   ((string-prefix-p "doric-" (symbol-name theme)) #'my-doric-themes-get-color-value)
   (t (error "unknown theme type: %s" theme))))

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

(defun themegen--theme-dark-p (theme get-color-value-func)
  "Return non-nil if THEME has a dark background."
  (modus-themes-color-dark-p
   (themegen--get-theme-color theme '(bg-main) get-color-value-func)))

(defun themegen--build-theme-values (theme map)
  "Use MAP to translate the values of Prot's emacs THEME to an alist
of theme keys and values suitable for another application.

Load THEME if it hasn't yet been loaded, otherwise its palette
won't be available.

Entries in MAP can use a conditional format:
  (\"key\" :dark (bg-main) :light (fg-main))
This selects different palette keys depending on whether the theme
has a dark or light background."
  (unless (and (custom-theme-p theme) (get theme 'theme-settings))
    (load-theme theme :no-confirm :no-enable))
  (let* ((get-color-value-func (themegen--get-color-value-function theme))
         (dark-p (themegen--theme-dark-p theme get-color-value-func)))
    (mapcar
     (pcase-lambda (`(,conf-key . ,theme-keys))
       (cons conf-key
             (themegen--get-theme-color
              theme
              (if (memq :dark theme-keys)
                  (if dark-p (plist-get theme-keys :dark) (plist-get theme-keys :light))
                theme-keys)
              get-color-value-func)))
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
  "~/Projects/dotfiles/.config/kitty/themes"
  "Default directory to store generated kitty themes.")

(defvar themegen-kitty-theme-metadata
  "## author: Ryan Kaskel\n## license: GNU GPLv3\n## blurb: Powered by Protesilaos Stavrou's emacs themes"
  "Default metadata for kitty themes")

(defvar themegen-kitty-colors-alist
  '(("background" bg-main)
    ("foreground" fg-main)
    ("cursor" cursor)
    ("cursor_text_color" bg-main)
    ("url_color" blue fg-blue)
    ("active_border_color" fg-dim fg-shadow-subtle)
    ("inactive_border_color" bg-dim bg-shadow-subtle)
    ("bell_border_color" yellow-cooler fg-yellow)
    ;; ("active_tab_foreground" green fg-green)
    ;; ("active_tab_background" bg-dim bg-shadow-subtle)
    ;; ("inactive_tab_foreground" fg-dim fg-shadow-subtle)
    ;; ("inactive_tab_background" bg-main)
    ("selection_foreground" bg-main)
    ("selection_background" fg-main)
    ("macos_titlebar_color" "background")
    ;; black
    ("color0" :dark (bg-main) :light (fg-main))
    ;; light black
    ("color8" :dark (bg-dim bg-shadow-subtle) :light (fg-dim fg-shadow-subtle))
    ;; red
    ("color1" red fg-red)
    ;; light red
    ("color9" red-warmer fg-red)
    ;; green
    ("color2" green fg-green)
    ;; light green
    ("color10" green-warmer fg-green)
    ;; yellow
    ("color3" yellow fg-yellow)
    ;; light yellow
    ("color11" yellow-cooler fg-yellow)
    ;; blue
    ("color4" blue fg-blue)
    ;; light blue
    ("color12" blue-warmer fg-blue)
    ;; magenta
    ("color5" magenta fg-magenta)
    ;; light magenta
    ("color13" magenta-cooler fg-magenta)
    ;; cyan
    ("color6" cyan fg-cyan)
    ;; light cyan
    ("color14" cyan-cooler fg-cyan)
    ;; white
    ("color7" :dark (fg-dim fg-shadow-subtle) :light (bg-dim bg-shadow-subtle))
    ;; bright white
    ("color15" :dark (fg-main) :light (bg-main)))
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
    ("info" fg-prompt fg-accent)
    ("border" border)
    ("prompt" fg-prompt fg-accent)
    ("pointer" fg-prompt fg-accent)
    ("marker" fg-prompt fg-accent)
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

;; Pi

(require 'json)

(defvar themegen-pi-themes-dir
  (expand-file-name "~/.pi/agent/themes/")
  "Directory where generated Pi themes are written.")

(defvar themegen-pi-themes
  (delete-dups
   (append modus-themes-collection
           ef-themes-collection
           doric-themes-collection))
  "Modus, Ef, and Doric themes exported for Pi.")

(defvar themegen-pi-schema-url
  "https://raw.githubusercontent.com/earendil-works/pi/main/packages/coding-agent/src/modes/interactive/theme/theme-schema.json"
  "Schema URL written into generated Pi themes.")

(defconst themegen-pi-modus-colors-alist
  '(("accent" accent-0)
    ("border" border)
    ("borderAccent" accent-0)
    ("borderMuted" bg-inactive bg-dim)
    ("success" prose-done green info)
    ("error" err red)
    ("warning" warning yellow-warmer)
    ("muted" fg-dim comment)
    ("dim" fg-dim)
    ("text" fg-main)
    ("thinkingText" fg-dim comment)

    ("selectedBg" bg-active bg-completion)
    ("userMessageBg" bg-alt bg-dim)
    ("userMessageText" fg-main)
    ("customMessageBg" bg-completion bg-alt bg-dim)
    ("customMessageText" fg-main)
    ("customMessageLabel" accent-1 accent-0)
    ("toolPendingBg" bg-warning bg-hl-line bg-dim)
    ("toolSuccessBg" bg-info bg-hl-line bg-dim)
    ("toolErrorBg" bg-err bg-hl-line bg-dim)
    ("toolTitle" fg-main)
    ("toolOutput" fg-dim comment)

    ("mdHeading" rainbow-0 accent-0)
    ("mdLink" fg-link blue)
    ("mdLinkUrl" fg-dim)
    ("mdCode" fg-prose-code constant cyan)
    ("mdCodeBlock" fg-main)
    ("mdCodeBlockBorder" border)
    ("mdQuote" fg-dim)
    ("mdQuoteBorder" border)
    ("mdHr" border)
    ("mdListBullet" rainbow-1 accent-0)

    ("toolDiffAdded" fg-added green)
    ("toolDiffRemoved" fg-removed red)
    ("toolDiffContext" fg-dim)

    ("syntaxComment" comment)
    ("syntaxKeyword" keyword)
    ("syntaxFunction" fnname)
    ("syntaxVariable" variable)
    ("syntaxString" string)
    ("syntaxNumber" constant number)
    ("syntaxType" type)
    ("syntaxOperator" operator)
    ("syntaxPunctuation" punctuation)

    ("thinkingOff" fg-dim)
    ("thinkingMinimal" border)
    ("thinkingLow" blue)
    ("thinkingMedium" cyan)
    ("thinkingHigh" magenta)
    ("thinkingXhigh" red-warmer)
    ("thinkingMax" red-intense red-warmer)
    ("bashMode" success green warning))
  "Map Pi colors to Modus and Ef palette entries.")

(defconst themegen-pi-doric-colors-alist
  '(("accent" cursor fg-accent)
    ("border" border)
    ("borderAccent" fg-accent)
    ("borderMuted" fg-shadow-subtle)
    ("success" fg-green)
    ("error" fg-red)
    ("warning" fg-yellow)
    ("muted" fg-shadow-subtle)
    ("dim" fg-shadow-subtle)
    ("text" fg-main)
    ("thinkingText" fg-shadow-subtle)

    ("selectedBg" bg-neutral)
    ("userMessageBg" bg-shadow-subtle)
    ("userMessageText" fg-main)
    ("customMessageBg" bg-accent)
    ("customMessageText" fg-main)
    ("customMessageLabel" fg-accent)
    ("toolPendingBg" bg-shadow-subtle)
    ("toolSuccessBg" bg-green)
    ("toolErrorBg" bg-red)
    ("toolTitle" fg-main)
    ("toolOutput" fg-shadow-subtle)

    ("mdHeading" fg-accent)
    ("mdLink" fg-blue)
    ("mdLinkUrl" fg-shadow-subtle)
    ("mdCode" fg-cyan)
    ("mdCodeBlock" fg-main)
    ("mdCodeBlockBorder" border)
    ("mdQuote" fg-shadow-subtle)
    ("mdQuoteBorder" border)
    ("mdHr" border)
    ("mdListBullet" fg-accent)

    ("toolDiffAdded" fg-green)
    ("toolDiffRemoved" fg-red)
    ("toolDiffContext" fg-shadow-subtle)

    ("syntaxComment" fg-accent)
    ("syntaxKeyword" fg-main)
    ("syntaxFunction" fg-shadow-intense)
    ("syntaxVariable" fg-main)
    ("syntaxString" fg-shadow-subtle)
    ("syntaxNumber" fg-main)
    ("syntaxType" fg-shadow-intense)
    ("syntaxOperator" fg-main)
    ("syntaxPunctuation" fg-main)

    ("thinkingOff" fg-shadow-subtle)
    ("thinkingMinimal" border)
    ("thinkingLow" fg-blue)
    ("thinkingMedium" fg-cyan)
    ("thinkingHigh" fg-magenta)
    ("thinkingXhigh" fg-red)
    ("thinkingMax" fg-yellow)
    ("bashMode" fg-yellow))
  "Map Pi colors to Doric palette entries.")

(defun themegen--pi-retrieve-palette-value (color palette)
  "Resolve COLOR from PALETTE.
Return `unspecified' when COLOR is not present or cannot be resolved."
  (let ((value (car (alist-get color palette))))
    (cond
     ((null value) 'unspecified)
     ((stringp value) value)
     ((eq value 'unspecified) value)
     ((symbolp value)
      (themegen--pi-retrieve-palette-value value palette))
     (t 'unspecified))))

(defun themegen--pi-get-color (palette keys theme)
  "Return the first resolvable color in KEYS from PALETTE for THEME."
  (or
   (cl-loop
    for key in keys
    for value = (themegen--pi-retrieve-palette-value key palette)
    unless (eq value 'unspecified)
    return value)
   (error "No usable Pi color for %S in theme `%s'" keys theme)))

(defun themegen--pi-theme-family (theme)
  "Return the palette family for THEME."
  (cond
   ((memq theme doric-themes-collection) 'doric)
   ((memq theme modus-themes-collection) 'modus)
   ((memq theme ef-themes-collection) 'ef)
   (t (error "Theme `%s' is not a Pi theme" theme))))

(defun themegen--pi-get-doric-palette (theme)
  "Return the palette for Doric THEME."
  (let ((palette-symbol (intern (format "%s-palette" theme))))
    (unless (boundp palette-symbol)
      (load-theme theme :no-confirm :no-enable))
    (if (boundp palette-symbol)
        (symbol-value palette-symbol)
      (error "No palette found for Doric theme `%s'" theme))))

(defun themegen--pi-get-palette (theme)
  "Return the palette for THEME."
  (pcase (themegen--pi-theme-family theme)
    ('doric (themegen--pi-get-doric-palette theme))
    ((or 'modus 'ef)
     (or (modus-themes-get-theme-palette theme)
         (error "No palette found for theme `%s'" theme)))))

(defun themegen--pi-color-map (theme palette)
  "Return the Pi color alist for THEME and PALETTE."
  (let ((map (if (eq (themegen--pi-theme-family theme) 'doric)
                 themegen-pi-doric-colors-alist
               themegen-pi-modus-colors-alist)))
    (mapcar
     (lambda (entry)
       (cons (car entry)
             (themegen--pi-get-color palette (cdr entry) theme)))
     map)))

(defun themegen--format-pi-config (theme)
  "Return the JSON Pi theme for THEME."
  (let* ((family (themegen--pi-theme-family theme))
         (palette (themegen--pi-get-palette theme))
         (color-map (themegen--pi-color-map theme palette))
         (get (lambda (keys)
                (themegen--pi-get-color palette keys theme)))
         (object
          `(("$schema" . ,themegen-pi-schema-url)
            ("name" . ,(symbol-name theme))
            ("colors" . ,color-map)
            ("export"
             . (("pageBg" . ,(funcall get '(bg-main)))
                ("cardBg" . ,(funcall get
                                      (if (eq family 'doric)
                                          '(bg-neutral bg-shadow-subtle)
                                        '(bg-alt bg-dim))))
                ("infoBg" . ,(funcall get
                                      (if (eq family 'doric)
                                          '(bg-yellow bg-shadow-subtle)
                                        '(bg-warning bg-hl-line bg-dim)))))))))
    (let ((json-encoding-pretty-print t))
      (concat (json-encode object) "\n"))))

(defun themegen--save-pi-theme-file (theme &optional dir)
  "Write THEME's Pi JSON file to DIR."
  (let ((directory (file-name-as-directory
                    (expand-file-name (or dir themegen-pi-themes-dir)))))
    (make-directory directory t)
    (write-region
     (themegen--format-pi-config theme)
     nil
     (expand-file-name (format "%s.json" theme) directory)
     nil
     'silent)))

(defun themegen--select-pi-theme (&optional prompt)
  "Prompt for a theme among `themegen-pi-themes'."
  (intern
   (completing-read
    (or prompt "Select Pi theme: ")
    themegen-pi-themes
    nil t nil
    'themegen--select-theme-history)))

(defun themegen-generate-pi-themes ()
  "Generate Pi JSON themes for Modus, Ef, and Doric."
  (interactive)
  (dolist (theme themegen-pi-themes)
    (themegen--save-pi-theme-file theme))
  (message "Generated %d Pi themes in %s"
           (length themegen-pi-themes)
           (expand-file-name themegen-pi-themes-dir)))

(defun themegen-generate-pi-theme (theme)
  "Generate one selected Pi THEME."
  (interactive (list (themegen--select-pi-theme)))
  (themegen--save-pi-theme-file theme)
  (message "Generated Pi theme `%s' in %s"
           theme
           (expand-file-name themegen-pi-themes-dir)))

(defun themegen-check-pi-themes ()
  "Check that all generated Pi themes are present and valid JSON."
  (interactive)
  (let ((directory (file-name-as-directory
                    (expand-file-name themegen-pi-themes-dir)))
        (errors nil))
    (dolist (theme themegen-pi-themes)
      (let ((file (expand-file-name (format "%s.json" theme) directory)))
        (cond
         ((not (file-readable-p file))
          (push (format "%s: missing file" theme) errors))
         (t
          (condition-case err
              (let* ((json-object-type 'alist)
                     (json-array-type 'list)
                     (json-key-type 'string)
                     (data (json-read-file file))
                     (colors (cdr (assoc "colors" data)))
                     (expected (mapcar #'car themegen-pi-modus-colors-alist))
                     (missing (cl-set-difference
                               expected
                               (mapcar #'car colors)
                               :test #'string=)))
                (when missing
                  (push (format "%s: missing colors %S" theme missing) errors)))
            (error
             (push (format "%s: %s" theme (error-message-string err)) errors)))))))
    (if errors
        (error "Pi theme checks failed:\n%s"
               (mapconcat #'identity (nreverse errors) "\n"))
      (message "Checked %d Pi themes in %s"
               (length themegen-pi-themes)
               directory))))

(provide 'themegen)
;;; themegen.el ends here
