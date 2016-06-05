;; Indent aggressively
;(require 'aggressive-indent)

;; Indent using spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Dash
(add-hook 'emacs-lisp-mode-hook '(lambda()
                                   (message "[Dash] Loaded docset 'Emacs Lisp'.")
                                   (setq-local helm-dash-docsets '("Emacs_Lisp" "Emacs Lisp"))))
