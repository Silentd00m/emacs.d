(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))



(setq custom-file "~/.emacs.d/config/custom.el")
(load custom-file)
(load "~/.emacs.d/config/layers")
(load "~/.emacs.d/config/config")

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package f
             :ensure t)

(require 'f)
(setq gears-emacs-basepath (f-dirname (f-this-file)))

(unless (> (length gears-layer-installed-list) 0)
  (load "~/.emacs.d/functions/install")

  (gears-install-firstrun-setup))

(load (concat gears-emacs-basepath "/functions/layers"))
(gears-layer-init)

;; Functions
(defun compile-emacs-dir ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/plugins") 0)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/config") 0)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/elpa") 0))
