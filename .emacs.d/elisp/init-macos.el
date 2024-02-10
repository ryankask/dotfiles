;;; -*- lexical-binding: t; -*-

(setq ns-function-modifier 'hyper
      ring-bell-function 'ignore
      trash-directory "~/.Trash"
      native-comp-driver-options '("-Wl,-w"))

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-arguments nil
        exec-path-from-shell-variables (append exec-path-from-shell-variables
                                               (list "GOPATH" "DOTFILES")))
  (exec-path-from-shell-initialize))

;; Rectangle app helpers

(defconst my-rectangle-actions
  (list "left-half" "right-half" "center-half" "top-half" "bottom-half"
        "top-left" "top-right" "bottom-left" "bottom-right"
        "first-third" "center-third" "last-third"
        "first-two-thirds" "last-two-thirds"
        "maximize" "almost-maximize" "maximize-height"
        "smaller" "larger" "center" "restore"
        "next-display" "previous-display"
        "move-left" "move-right" "move-up" "move-down"
        "first-fourth" "second-fourth" "third-fourth" "last-fourth"
        "first-three-fourths" "last-three-fourths"
        "top-left-sixth" "top-center-sixth" "top-right-sixth"
        "bottom-left-sixth" "bottom-center-sixth" "bottom-right-sixth"
        "specified" "reverse-all"
        "top-left-ninth" "top-center-ninth" "top-right-ninth"
        "middle-left-ninth" "middle-center-ninth" "middle-right-ninth"
        "bottom-left-ninth" "bottom-center-ninth" "bottom-right-ninth"
        "top-left-third" "top-right-third"
        "bottom-left-third" "bottom-right-third"
        "top-left-eighth" "top-center-left-eighth" "top-center-right-eighth"
        "top-right-eighth" "bottom-left-eighth" "bottom-center-left-eighth"
        "bottom-center-right-eighth" "bottom-right-eighth"
        "tile-all" "cascade-all"))

(defun my-rectangle-exec-action (name)
  "Execute Rectangle app action NAME using the open command. NAME
 must be one the values in `my-rectangle-app-actions'."
  (interactive (list (completing-read "Action: " my-rectangle-actions)))
  (call-process
   "open" nil 0 nil "-g" (format "rectangle://execute-action?name=%s" name)))

(dolist (action my-rectangle-actions)
  (defalias (intern (format "my-rectangle-exec-action--%s" action))
    (lambda ()
      (interactive)
      (my-rectangle-exec-action action))
    (format "Execute Rectangle app action `%s'" action)))

(elpaca nil
  (with-eval-after-load 'transient
    (transient-define-prefix my-rectangle-dispatch ()
      "Rectangle app dispatcher"
      [["Halves"
        ("l" "left half" my-rectangle-exec-action--left-half)
        ("r" "right half" my-rectangle-exec-action--right-half)
        ("c" "center half" my-rectangle-exec-action--center-half)
        ("th" "top half" my-rectangle-exec-action--top-half)
        ("bh" "bottom half" my-rectangle-exec-action--bottom-half)]
       ["Corners"
        ("tl" "top left" my-rectangle-exec-action--top-left)
        ("tr" "top right" my-rectangle-exec-action--top-right)
        ("bl" "bottom left" my-rectangle-exec-action--bottom-left)
        ("br" "bottom right" my-rectangle-exec-action--bottom-right)]
       ["Thirds"
        ("3l" "first third" my-rectangle-exec-action--first-third)
        ("3c" "center third" my-rectangle-exec-action--center-third)
        ("3r" "last third" my-rectangle-exec-action--last-third)
        ("," "first two thirds" my-rectangle-exec-action--first-two-thirds)
        ("." "last two thirds" my-rectangle-exec-action--last-two-thirds)]
       ["Edges"
        ("el" "move left" my-rectangle-exec-action--move-left)
        ("er" "move right" my-rectangle-exec-action--move-right)
        ("eu" "move up" my-rectangle-exec-action--move-up)
        ("ed" "move down" my-rectangle-exec-action--move-down)]
       ["Size"
        ("m" "maximise" my-rectangle-exec-action--maximize)
        ("a" "almost maximise" my-rectangle-exec-action--almost-maximize)
        ("u" "maximise height" my-rectangle-exec-action--maximize-height)
        ("-" "smaller" my-rectangle-exec-action--smaller)
        ("+" "larger" my-rectangle-exec-action--larger)
        ("=" "center" my-rectangle-exec-action--center)
        ("v" "restore" my-rectangle-exec-action--restore)]])

    (bind-key "s-f" #'my-rectangle-dispatch)))

(provide 'init-macos)
