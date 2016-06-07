;; Indent aggressively
;(require 'aggressive-indent)

;; Indent using spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                   (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t))) ;; Automatically byte-compile emacs-lisp files upon save


;; Dash
(add-hook 'emacs-lisp-mode-hook '(lambda()
                                   (message "[Dash] Loaded docset 'Emacs Lisp'.")
                                   (setq-local helm-dash-docsets '("Emacs_Lisp" "Emacs Lisp"))))
