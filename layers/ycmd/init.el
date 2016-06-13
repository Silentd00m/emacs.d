(load "~/.emacs.d/functions/packages")

(defun gears-layers/ycmd-description()
  "Returns a description of the YCMD layer."

  (eval "Provides the YCMD code-completion server."))

(defun gears-layers/ycmd-install()
  "Installs the YCMD layer."

  (gears-install-packages '(ycmd company-ycmd)))

(defun gears-layers/ycmd-remove()
  "Removes the YCMD layer."
  (gears-package-delete 'ycmd)

  (dolist (package '(company-ycmd flycheck-ycmd))
    (when (package-installed-p package)
      (gears-package-delete package))))
