;;; Code:
;; Requires:*
(require 'f)

(load (concat gears-emacs-basepath "/config/layers"))
(load (concat gears-emacs-basepath "/functions/packages"))

(cl-defstruct gears-layer-dependencies packages layers)

(defgroup gears-layers nil
  "Configurations for all layers."

  :group 'gears)

(defun gears-layer-convert-name (layer)
  (if (stringp layer)
      layer
    (prin1-to-string layer)))

(defun gears-layer-init ()
  "Initializes all installed layers."

  (interactive)

  (dolist (layer gears-layer-installed-list)
    (load (concat gears-emacs-basepath "/layers/" (gears-layer-convert-name layer) "/init.el"))
    (funcall (intern (concat "gears-layers/" (gears-layer-convert-name layer) "-init")))))

(defun gears-layer-list-save ()
  "Saves the current value of gears-layers-installed-list"

  (f-write-text (concat "(setq gears-layer-installed-list '"
                        (prin1-to-string gears-layer-installed-list)
                        ")")
                'utf-8 (concat gears-emacs-basepath "/config/layers.el")))

(defun gears-layer-list-available ()
  "Returns a list of all available configuration layers."

  (delete "base"
          (directory-files (expand-file-name (concat
                                              gears-emacs-basepath
                                              "/layers/"))
                           nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))

(defun gears-layer-get-description (layer)
  "Returns the description of the given layer."

  (load (concat gears-emacs-basepath
                "/layers/"
                (gears-layer-convert-name layer)
                "/init"))

  (funcall (intern (concat "gears-layers/" (gears-layer-convert-name layer) "-description"))))

(defun gears-layer--recursive-list-dependencies (layer)
  (load (concat gears-emacs-basepath "/layers/" (gears-layer-convert-name layer) "/init"))

  ;; Prefer the <layer>-generate-dependency-list function, then fall back to
  ;; <layer>-depends
  (let* ((deps (gears-layer-get-depends layer))
         (package-list (gears-layer-dependencies-packages deps))
         (layer-list (gears-layer-dependencies-layers deps)))

    (dolist (layer-dep layer-list)
      (setq package-list (append package-list
                                 (gears-layer--recursive-list-dependencies layer-dep))))

    (remove-duplicates package-list)))

(defun gears-layer-mark-installed (layer)
  "Marks a layer as installed."

  (if (stringp layer)
      (setq gears-layer-installed-list
            (append gears-layer-installed-list `(,(intern layer))))
    (setq gears-layer-installed-list
          (append gears-layer-installed-list `(,layer))))


  (setq gears-layer-installed-list (remove-duplicates gears-layer-installed-list
                                                      :from-end t)))

(defun gears-layer--recursive-mark-installed (layer)
  "Marks a layer, its sublayers and all dependencies as installed."

  (gears-layer-mark-installed layer)

  (dolist (layer-dep (gears-layer-dependencies-layers
                      (gears-layer-get-depends layer)))
    (gears-layer--recursive-mark-installed layer-dep)))

(defun gears-layer-get-depends (layer)
  "Returns the dependencies for a given layer."

  ;; Prefer the <layer>-generate-dependency-list function, then fall back to
  ;; <layer>-depends
  (if (fboundp (intern (concat "gears-layers/"
                               (gears-layer-convert-name layer)
                               "-generate-dependency-list")))
      (funcall (intern (concat "gears-layers/"
                               (gears-layer-convert-name layer)
                               "-generate-dependency-list")))
    (eval (intern (concat "gears-layers/"
                          (gears-layer-convert-name layer)
                          "-depends")))))

(defun gears-layer-autoremove-packages ()
  "Removes all packages not used by any layer."

  (let ((glarp-depended-pkg-list nil))
    ;; Build a list of all used packages from the dependencies of the installed
    ;; layers.
    (dolist (layer gears-layer-installed-list)
      (add-to-list 'glarp-depended-pkg-list
                   (cdr (assoc 'packages
                               (gears-layer-get-depends layer)))))

    ;; Iterate over the list of activated packages, delete everything not needed
    ;; in a layer.
    (dolist (package package-activated-list)
      (unless (member package glarp-depended-pkg-list)
        (gears-package-remove package)))))

(defun gears-layer-install (layer)
  "Install a layer and all dependencies."

  (interactive "sInstall Layer: ")

  ;; ;; Add layer to list of layers
  ;; (add-to-list 'gears-layer-installed-list layer)
  ;; (gears-layer-list-save)

  (load (concat gears-emacs-basepath "/layers/" (gears-layer-convert-name layer) "/init"))

  ;; Install all layer dependencies
  (gears-package-install-packages (gears-layer--recursive-list-dependencies layer))
  (gears-layer--recursive-mark-installed layer))

(defun gears-layer-remove (layer)
  "Removes a layer and all its files and unused packages."

  (funcall (intern (concat "gears-layers/"
                           (gears-layer-convert-name layer)
                           "-remove")))

  (delete layer gears-layer-installed-list)

  (gears-layer-list-save)

  ;; TODO : Uninstall unused packages.
  )

(defun gears-layer-installed (layer)
  "Returns true if layer is installed."

  (> (length (member layer gears-layer-installed-list)) 0))


(defmacro gears-layer-defdepends (layer &rest depends)
  "Simple way to generate a dependency list for a layer."

  `(setq ,(intern (concat "gears-layers/" (gears-layer-convert-name layer) "-depends"))
         (make-gears-layer-dependencies ,@depends)))
