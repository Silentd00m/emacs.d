(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(setq package-enable-at-startup nil)
(add-to-list 'load-path "~/.emacs.d/dep")

(setq gears-base-packages '(bug-hunter

                            ;; Autocompletion
                            company
                            ycmd
                            company-ycmd
                            yasnippet

                            ;; Syntax checking
                            flycheck-pos-tip
                            flycheck
                            helm-flycheck

                            ;; Documentation
                            dash

                            ;; Navigation
                            fzf

                            ;; Interface
                            git-gutter-fringe+
                            git-gutter+
                            helm-dash
                            helm
                            magit
                            moe-theme
                            multiple-cursors
                            nlinum
                            powerline
                            rainbow-delimiters
                            tabbar-ruler
                            material-theme

                            ;; Keyboard
                            evil
                            redo+
                            smartparens
                            comment-dwim-2

                            ;; Libraries
                            use-package
                            fuzzy))
