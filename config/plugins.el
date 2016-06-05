(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(setq package-enable-at-startup nil)
(add-to-list 'load-path "~/.emacs.d/plugins")
(require 'cl)

(setq my/package-list '(
                        use-package

                        ;; Theming
                        rainbow-delimiters ; Colorful delimiters
                        moe-theme ; Nice theming
                        nlinum ; Improved line numbers
                        idle-highlight-mode
                        smart-mode-line
                        pretty-mode
                        tabbar-ruler

                        ;; Auto completion
                        company
                        company-ycmd
                        ycmd

                        ;; Helm
                        helm
                        helm-swoop
                        helm-package
                        helm-flycheck
                        helm-company

                        ;; Syntax checking
                        flycheck ; On the fly syntax checking
                        flycheck-ycmd ; YCMD integration for flycheck

                        ;; Editing
                        aggressive-indent
                        comment-dwim-2
                        multiple-cursors ; Work at multiple places in a buffer at once
                        mmm-mode         ;
                        smartparens ; Handle pairable symbols like parens
                        visual-regexp visual-regexp-steroids ; Visual regular expressions for search and replace
                        avy ; Jump to characters with style
                        saveplace

                        ;; Git
                        magit
                        git-gutter+
                        git-gutter-fringe+

                        ;; CMake
                        cmake-mode
                        cmake-font-lock
                        ))

;; (defvar packages-refreshed nil)

;; (unless packages-refreshed
;;   (package-refresh-contents)
;;   (setq packages-refreshed t))
;; (cl-loop for package in my/package-list
;;          unless (package-installed-p package)
;;          do (package-install package))
