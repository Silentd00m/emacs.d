;;; -*- lexical-binding: t -*-

(defun gears-layers/dash-init()
  ""

  (defgroup gears-layers/dash nil
    "Dash-Layer configuration."

    :group 'gears-layers)

  (defcustom gears-layers/dash-docset-list '("Emacs_Lisp")
    "List of all docsets to install and keep updated."

    :type 'listp
    :group 'gears-layers/dash)

  (defcustom gears-layers/dash-docset-path (concat "~/.emacs.d/docsets")
    "Directory where the docsets should be stored."

    :type 'stringp
    :group 'gears-layers/dash)

  (defcustom gears-layers/dash-docset-mode-autoloads '((cpp-mode-hook . '("C++"))
                                                       (emacs-lisp-mode-hook . '("Emacs_Lisp"))
                                                       (python-mode-hook . '("Python 3")))
    "Defines which Docsets should be loaded in what mode."

    :type '(alist :value (group symbolp stringp))
    :group 'gears-layers/dash)

  (defcustom gears-layers/dash-minimum-characters 3
    "Minimum search string length."

    :type 'integerp
    :group 'gears-layers/dash)

  (setq helm-dash-browser-func 'eww)
  (setq helm-dash-docsets-path (expand-file-name gears-layers/dash-docset-path))
  (setq helm-dash-enable-debugging t)

  (dolist (docset gears-layers/dash-docset-list)
    ;; Install missing docsets
    (unless (file-exists-p (concat gears-emacs-basepath
                                   "/docsets/"
                                   docset
                                   ".docset"))
      (helm-dash-install-docset docset)))

  (dolist (item gears-layers/dash-docset-mode-autoloads)
    (add-hook (car item) `(lambda()
                            (setq-local helm-dash-docsets ,(cdr item))))))

(defun gears-layers/dash-description()
  "Returns the Dash layer description."

  (eval "Display Dash docsets inside emacs to browse documentation. "))

(defun gears-layers/dash-install()
  "Additional commands for layer installation.")

(defun gears-layers/dash-remove()
  "Removes the Dash layer including packages."

  (dolist (package '(dash-at-point helm-dash))
    (gears-package-remove package)))

(defun gears-layers/dash-configure()
  (customize-group 'gears-layers/dash))

(gears-layer-defdepends dash
                        :packages '(dash-at-point helm-dash)
                        :layers 'base)
