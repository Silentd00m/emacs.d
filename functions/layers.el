;;; Code:
;; Requires:*
(require 'f)
(require 'cl)
(require 'dash)

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

(defun gears-load-layer (layer)
  "Load LAYER without initializing it."

  (load (concat gears-emacs-basepath
                "/layers/"
                (gears-layer-convert-name layer)
                "/init.el")))

(defun gears-layer-init (layer)
  "Load and initialize LAYER."

  (gears-load-layer layer)

  (funcall (intern (concat "gears-layers/"
                           (gears-layer-convert-name layer)
                           "-init"))))

(defun gears-layers-init ()
  "Initialize all installed layers."

  (interactive)

  (dolist (layer gears-layer-installed-list)
    (gears-layer-init layer)))

(defun gears-layer-list-save ()
  "Save the current value of gears-layers-installed-list."

  (f-write-text (concat "(setq gears-layer-installed-list '"
                        (prin1-to-string gears-layer-installed-list)
                        ")")
                'utf-8 (concat gears-emacs-basepath "/config/layers.el")))

(defun gears-layer-list-available ()
  "Return a list of all available configuration layers."

  (delete "base"
          (directory-files (expand-file-name (concat
                                              gears-emacs-basepath
                                              "/layers/"))
                           nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))

(defun gears-layer-get-description (layer)
  "Return the description of the given LAYER."

  (load (concat gears-emacs-basepath
                "/layers/"
                (gears-layer-convert-name layer)
                "/init"))

  (funcall (intern (concat "gears-layers/" (gears-layer-convert-name layer)
                           "-description"))))

(defun gears-layer--recursive-list-dependencies (layer)
  (load (concat gears-emacs-basepath "/layers/" (gears-layer-convert-name layer)
                "/init"))

  ;; Prefer the <layer>-generate-dependency-list function, then fall back to
  ;; <layer>-depends
  (let* ((deps (gears-layer-get-depends layer))
         (package-list (gears-layer-dependencies-packages deps))
         (layer-list (gears-layer-dependencies-layers deps)))

    (dolist (layer-dep layer-list)
      (setq package-list
            (append package-list
                    (gears-layer--recursive-list-dependencies layer-dep))))

    (-non-nil (remove-duplicates package-list))))

(defun gears-layer-mark-installed (layer)
  "Mark LAYER as installed."

  (if (stringp layer)
      (setq gears-layer-installed-list
            (append gears-layer-installed-list `(,(intern layer))))
    (setq gears-layer-installed-list
          (append gears-layer-installed-list `(,layer))))


  (setq gears-layer-installed-list (remove-duplicates gears-layer-installed-list
                                                      :from-end t))

  (gears-layer-list-save))

(defun gears-layer--recursive-mark-installed (layer)
  "Mark LAYER, its sublayers and all dependencies as installed."

  (gears-layer-mark-installed layer)

  (dolist (layer-dep (gears-layer-dependencies-layers
                      (gears-layer-get-depends layer)))
    (gears-layer--recursive-mark-installed layer-dep)))

(defun gears-layer-get-depends (layer)
  "Return the dependencies for the givne LAYER."

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
  "Remove all packages not used by any layer."

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

(defun gears-layer-install ()
  "Install a layer and all dependencies."

  (interactive)

  (helm :sources
        (helm-build-sync-source "Layers"
          :candidates #'(lambda ()
                          (let ((layer-folders
                                 (f-directories (concat gears-emacs-basepath
                                                        "/layers/")))
                                (layer-list '()))
                            (dolist (layer layer-folders)
                              (unless (gears-layer-installed-p
                                       (intern (f-filename layer)))
                                (add-to-list 'layer-list (f-filename layer) t)))
                            layer-list))
          :fuzzy-match t
          :action '(lambda (package)
                     (package-refresh-contents)
                     (-gears-layer-install package)))
        :buffer "*gears layer-install*"))

(defun -gears-layer-install (layer)
  ;; ;; Add layer to list of layers
  ;; (add-to-list 'gears-layer-installed-list layer)
  ;; (gears-layer-list-save)

  (load (concat gears-emacs-basepath "/layers/"
                (gears-layer-convert-name layer) "/init"))

  ;; Execute pre-install if defined
  (when (boundp (intern (concat "gears-layers/"
                                (gears-layer-convert-name layer)
                                "-pre-install")))
    (funcall (intern (concat "gears-layers/"
                             (gears-layer-convert-name layer)
                             "-pre-install"))))

  ;; Install all layer dependencies
  (gears-package-install-packages
   (gears-layer--recursive-list-dependencies layer))
  (gears-layer--recursive-mark-installed layer)

  (dolist (installed-layer gears-layer-installed-list)
    (gears-load-layer installed-layer)

    (dolist (dependency (gears-layer--recursive-list-dependencies
                         installed-layer)
                        (when dependency
                          (package-install dependency)))))

    ;; Execute pre-install if defined
  (when (boundp (intern (concat "gears-layers/"
                                (gears-layer-convert-name layer)
                                "-post-install")))
    (funcall (intern (concat "gears-layers/"
                             (gears-layer-convert-name layer)
                             "-post-install"))))

  ;; Execute post-install if defined
  (when (boundp (intern (concat "gears-layers/"
                                (gears-layer-convert-name layer)
                                "-post-install")))
    (funcall (intern (concat "gears-layers/"
                             (gears-layer-convert-name layer)
                             "-post-install")))))

(defun gears-layer-remove (layer)
  "Remove LAYER and all its files and unused packages."

  (when (boundp (intern (concat "gears-layers/"
                                (gears-layer-convert-name layer)
                                "-pre-remove")))
    (funcall (intern (concat "gears-layers/"
                             (gears-layer-convert-name layer)
                             "-pre-remove"))))

  (delete layer gears-layer-installed-list)

  (gears-layer-list-save)

  ;; TODO : Uninstall unused packages.
  (when (boundp (intern (concat "gears-layers/"
                                (gears-layer-convert-name layer)
                                "-post-remove")))
    (funcall (intern (concat "gears-layers/"
                             (gears-layer-convert-name layer)
                             "-post-remove")))))

(defun gears-layer-installed-p (layer)
  "Return true if LAYER is installed."

  (> (length (member layer gears-layer-installed-list)) 0))

(defun gears-layer-update (layer)
  (when (gears-layer-installed-p layer)
    (-gears-layer-install layer)))

(defmacro gears-layer-defdepends (layer &rest depends)
  "Simple way to generate a dependency list for LAYER."

  `(setq ,(intern (concat "gears-layers/" (gears-layer-convert-name layer)
                          "-depends"))
         (make-gears-layer-dependencies ,@depends)))
