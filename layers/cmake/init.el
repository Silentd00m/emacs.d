;;; Code:

(defgroup gears-layers/cmake nil
  "CMake Layer configuration."
  :group 'gears-layers)

(defun gears-layers/cmake-init ()
  "Initializes the cmake layer.")

(defun gears-layers/cmake-description ()
  "Provides CMake highlighting, autocompletion and sets compiler options.")

(defun gears-layers/cmake-install ()
  "Additional installation commands for rtags-layer.")

(gears-layer-defdepends cmake
                        :packages '(cmake-mode))
