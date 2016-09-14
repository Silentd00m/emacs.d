;;; Code:

(defgroup gears-layers/cmake nil
  "CMake Layer configuration."
  :group 'gears-layers)

(defcustom gears-layers/cmake-setup-ide t
  "Setup compiler options and auto-completion."
  :type 'boolean
  :group 'gears-layers/cmake)

(defun gears-layers/cmake-init ()
  "Initializes the cmake layer."

  (when gears-layers/cmake-setup-ide
    ;; Also setup rtags if the layer has been installed.
    (add-hook 'c-mode-common-hook #'(lambda ()
                                      (when (gears-layer-installed 'rtags)
                                        (require 'rtags))

                                      (when (derived-mode-p 'c-mode 'c++-mode)
                                        ;; (cppcm-reload-all)
                                        (cmake-ide-setup)
                                        (when (gears-layer-installed 'rtags)
                                          (cmake-ide-maybe-start-rdm)))))))

(defun gears-layers/cmake-description ()
  "Provides CMake highlighting, autocompletion and sets compiler options.")

(defun gears-layers/cmake-install ()
  "Additional installation commands for rtags-layer.")

(gears-layer-defdepends cmake
                        :packages '(cmake-mode cpputils-cmake cmake-ide))
