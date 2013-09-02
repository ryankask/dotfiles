;; smartparens - https://github.com/Fuco1/smartparens

(smartparens-global-mode t)
(show-smartparens-global-mode t)
(sp-use-smartparens-bindings)
(setq-default sp-autoescape-string-quote nil)

(sp-pair "'" nil :unless '(sp-point-after-word-p))

(sp-with-modes sp--lisp-modes
  (sp-local-pair "'" nil :actions nil)
  (sp-local-pair "`" "'" :when '(sp-in-string-p)))

;; LaTeX modes
(sp-with-modes '(tex-mode plain-tex-mode latex-mode org-mode)
  (sp-local-pair "$" "$")
  (sp-local-pair "\\[" "\\]")
  (sp-local-pair "`" "'")
  (sp-local-tag "\\b" "\\begin{_}" "\\end{_}"))

;; html modes
(sp-with-modes '(sgml-mode html-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "{%" "%}")
  (sp-local-tag  "<" "<_>" "</_>" :transform 'sp-match-sgml-tags))

(provide 'init-smartparens)
