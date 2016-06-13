(require 'f)

(load "~/.emacs.d/config/layers")

(defun gears-layer-init()
  (interactive)

  "Initializes all installed layers."

  (dolist (layer gears-layer-installed-list)
    (load (concat gears-emacs-basepath "/layers/" (prin1-to-string layer) "/init.el"))
    (funcall (intern (concat "gears-layers/" (prin1-to-string layer) "-init")))))

(defun gears-layer-list-save()
  "Saves the current value of gears-layers-installed-list"

  (f-write-text (concat "(setq gears-layer-installed-list '("
                        (mapconcat #'sequencep gears-layer-installed-list " ")
                        "))")
                'utf-8 "~/.emacs.d/config/layers.el"))

(defun gears-layer-list-available()
  "Returns a list of all available configuration layers."

  (directory-files "~/.emacs.d/layers/" nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))

(defun gears-layer-get-description(layer)
  "Returns the description of the given layer."

  (funcall (intern (concat "gears-layers/" layer "-description"))))

(defun gears-layer--recursive-list-depends(layer)
  "Recursively lists all depended packages for the layer."

  (let ((gli-package-list '()))
    (dolist (layer-r (cdr (assoc 'layers (eval (intern (concat "gears-layers/"
                                                               layer
                                                               "-depends"))))))
      (setq gli-package-list
            (append gli-package-list (gears-layer--recursive-list-depends layer-r))))

    (remove-duplicates (append gli-package-list
            (cdr (assoc 'packages (eval (intern (concat "gears-layers/"
                                                        layer
                                                        "-depends")))))))))

(defun gears-layer-mark-installed(layer)
  (add-to-list 'gears-layer-installed-list layer))

(defun gears-layer--recursive-mark-installed(layer)
  (gears-layer-mark-installed layer)

  (dolist (layer-r (cdr (assoc 'layers (eval (intern (concat "gears-layers/"
                                                               layer
                                                               "-depends"))))))
    (gears-layer--recursive-mark-installed layer-r)))

(defun gears-layer-get-depends(layer)
  "Returns the dependencies for a given layer."

  (eval (intern (concat "gears-layers/" layer "-depends"))))

(defun gears-layer-autoremove-packages()
  "Removes all packages not used by any layer."

  (let ((glarp-depended-pkg-list nil))
    ;; Build a list of all used packages from the dependencies of the installed
    ;; layers.
    (dolist (layer gears-layer-installed-list)
      (add-to-list glarp-depended-pkg-list (cdr (assoc 'packages (gears-layer-get-depends layer)))))

    (dolist (package package-activated-list)
      (unless (member package glarp-depended-pkg-list)
        (gears-package-remove package)))))

(defun gears-layer-install(layer)
  "Install a layer and all dependencies."

  ;; Add layer to list of layers
  (add-to-list 'gears-layer-installed-list layer)
  (gears-layer-list-save)

  (load (concat "~/.emacs.d/layers/" layer "/init"))

  ;; Install all layer dependencies
  (gears-install-packages (cdr (assoc 'packages
                                      (eval (intern (concat "gears-layers/"
                                                            layer
                                                            "-depends"))))))
  (dolist (layer (member 'layers '(intern (concat "gears-layers/"
                                                  layer
                                                  "-depends"))))
    (gears-layer-install layer)))

(defun gears-layer-remove(layer)
  (funcall (intern (concat "gears-layers/" layer "-remove")))

  (delete layer gears-layer-installed-list)

  (gears-layer-list-save))

(defun gears-layer-installed(layer)
  (member layer 'gears-layer-installed-list))
