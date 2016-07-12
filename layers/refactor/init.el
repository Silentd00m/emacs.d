;;; Code:

(defun gears-layers/refactor-init ()
  "Initializes the refactoring layer."

  t)

(defun gears-layers/refactor-description ()
  "Returns a description of the layer."

  "Provides refactoring support for various programming languages, depending on selected layers.")

(defun gears-layers/refactor-install ()
  "Additional commands for layer installation."

  t)

(gears-layer-defdepends refactor) ;; No dependencies.
