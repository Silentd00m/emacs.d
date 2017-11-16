;;; -*- lexical-binding: t -*-

(require 's)

(defun gears-layers/diff-init()
  ""

  (defgroup gears-layers/diff nil
    "Diff-Layer configuration."

    :group 'gears-layers)

  (setq vdiff-lock-scrolling t))

(defun gears-layers/diff-description ()
  "Returns the Dash layer description."

  (eval "Display Dash docsets inside emacs to browse documentation. "))

(defun gears-layers/diff-install ()
  "Additional commands for layer installation.")

(defun gears-layers/diff-remove ()
  "Removes the Dash layer including packages."

  (dolist (package '(diff-at-point helm-diff))
    (gears-package-remove package)))

(defun gears-layers/diff-configure ()
  (customize-group 'gears-layers/diff))

(gears-layer-defdepends diff
                        :packages '(vdiff vdiff-magit))
