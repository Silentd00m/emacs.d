(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq inhibit-startup-message t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'f)
  (use-package f
    :ensure t))

(require 'f)
(setq gears-emacs-basepath (f-dirname (f-this-file)))

(unless (f-dir? (concat gears-emacs-basepath "/config"))
  (f-mkdir (concat gears-emacs-basepath "/config")))

(setq custom-file (concat gears-emacs-basepath "/config/custom.el"))

(when (f-file? (concat gears-emacs-basepath "/config/custom.el"))
    (load custom-file)
    (load (concat gears-emacs-basepath "/config/config.el")))

(if (f-file? (concat gears-emacs-basepath "/config/layers.el"))
    (load (concat gears-emacs-basepath "/config/layers"))
  (progn (f-write-text "(setq gears-layer-installed-list '())"
                       'utf-8
                       (concat gears-emacs-basepath "/config/layers.el"))
         (load (concat gears-emacs-basepath "/config/layers"))))

(unless (> (length gears-layer-installed-list) 0)
  (load "~/.emacs.d/functions/install")

  (gears-install-firstrun-setup))

(load (concat gears-emacs-basepath "/functions/layers"))
(gears-layers-init)

;; Functions
(defun compile-emacs-dir ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/plugins") 0)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/config") 0)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/elpa") 0)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/functions") 0))
