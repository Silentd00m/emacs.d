(use-package parinfer
  :ensure t
  :bind (("C-," . parinfer-toggle-mode))
  :init (progn (setq parinfer-extensions
                     '(defaults       ; should be included.
                        pretty-parens  ; different paren styles for different modes.
                        paredit        ; Introduce some paredit commands.
                        smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
                        smart-yank))); Yank behavior depend on mode.
  :hook (((common-lisp-mode-hook
           clojure-mode-hook
           scheme-mode-hook
           lisp-mode-hook
           emacs-lisp-mode-hook) . parinfer-mode)
         ((common-lisp-mode-hook
           clojure-mode-hook
           scheme-mode-hook
           lisp-mode-hook
           emacs-lisp-mode-hook) . aggressive-indent-mode)))
