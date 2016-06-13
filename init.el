(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;(setq debug-on-error nil)
(setq custom-file "~/.emacs.d/config/custom.el")
(load custom-file)
(load "~/.emacs.d/config/layers")

(unless (package-installed-p 'use-package)
  (load "~/.emacs.d/functions/install")

  (gears-install-default-packages))

(require 'f)
(setq gears-emacs-basepath (f-dirname (f-this-file)))

(load "~/.emacs.d/functions/layers")
(gears-layer-init)

;; TODO : Remove all following loads to layers.

(load "~/.emacs.d/config/config")
(load "~/.emacs.d/config/interface")
(load "~/.emacs.d/config/helm")
(load "~/.emacs.d/config/auto_completion")
(load "~/.emacs.d/config/syntax_checking")
(load "~/.emacs.d/config/cpp")
(load "~/.emacs.d/config/git")
(load "~/.emacs.d/config/sessions")
(load "~/.emacs.d/config/general")
;;(load "~/.emacs.d/config/keyboard")
(load "~/.emacs.d/config/python")
(load "~/.emacs.d/config/programming")
(load "~/.emacs.d/config/latex")

;; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
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
