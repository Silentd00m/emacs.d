(defun gears-layers/org-init ()
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

(defun gears-layers/org-description ()
  "Returns the description of the org Layer."

  "Supporting functions for org.")

(defun gears-layers/org-pre-install ()
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-refresh-contents))

(gears-layer-defdepends org
                        :packages '(org org-plus-contrib))
