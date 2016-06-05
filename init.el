(package-initialize)

(setq debug-on-error nil)
(setq custom-file "~/.emacs.d/config/custom.el")

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(load "~/.emacs.d/config/plugins")
(load custom-file)
(load "~/.emacs.d/config/theme")
(load "~/.emacs.d/config/auto_completion")
(load "~/.emacs.d/config/syntax_checking")
(load "~/.emacs.d/config/cpp")
(load "~/.emacs.d/config/git")
(load "~/.emacs.d/config/sessions")
(load "~/.emacs.d/config/general")
(load "~/.emacs.d/config/keyboard")
(load "~/.emacs.d/config/python")
(load "~/.emacs.d/config/programming")
(load "~/.emacs.d/config/latex")

(add-to-list "~/.emacs.d/elpa")

;; Hooks
(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                   (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t)
                                   )                                            ;; Automatically byte-compile emacs-lisp files upon save
          )
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)                             ;; Rainbow colored parentheses
(add-hook 'after-change-major-mode-hook '(lambda ()
                                           (setq-default indent-tabs-mode nil)
                                           (setq tab-width 4)
                                           (setq fill-column 81)
                                           )
          )
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'magit-mode '(lambda()
                         (setq ergoemacs-ctl-c-delay 1)
                         )
          )
(add-hook 'git-commit-mode '(setq ergoemacs-ctl-c-delay 1))
(add-hook 'magit-commit-mode '(setq ergoemacs-ctl-c-delay 1))
(add-hook 'org-mode '(lambda ()
                       (setq ergoemacs-ctl-c-delay 1)
                       )
          )
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

;; Functions
(defun compile-emacs-dir ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/plugins") 0)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/config") 0)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/elpa") 0))

(defun my-bell-function ()
  (unless (memq this-command
        '(isearch-abort abort-recursive-edit exit-minibuffer
              keyboard-quit mwheel-scroll down up next-line previous-line
              backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

;(require 'auto-package-update)
;(auto-package-update-maybe)

;(add-hook 'auto-package-update-before-hook
;          (lambda () (message "Updating packages.")))

                                        ;(compile-emacs-dir)
