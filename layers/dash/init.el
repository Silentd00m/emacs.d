(defun gears-layers/dash-init()
  (defgroup gears-layers/dash
    "Dash-Layer configuration."
    :group 'gears-layers)

  (defcustom gears-layers/dash-docset-list
    :type 'listp
    :group 'gears-layers/dash))

(defun gears-layers/dash-description()
  "Returns the Dash layer description."

  (eval "Display Dash docsets inside emacs to browse documentation. "))

(defun gears-layers/dash-install()
  "Installs the dash layer."

  (gears-install-packages '(dash-at-point helm-dash)))

(defun gears-layers/dash-remove()
  "Removes the Dash layer including packages."

  (dolist (package '(dash-at-point helm-dash))
    (gears-package-delete package)))

(defun gears-layers/dash-configure()
  (customize-group 'gears-layers/dash))
