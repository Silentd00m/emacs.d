;;; -*- lexical-binding: t; -*-

(require 'cl)

(load "~/.emacs.d/functions/packages")
(load "~/.emacs.d/functions/layers")

(defun gears-install-configure()
  (interactive)

  (when gears-use-evil
    (evil-mode nil))

  (setq gic-layer-list '(base))

  (generate-new-buffer-name "*gears-install*")
  (switch-to-buffer "*gears-install*")
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  (widget-insert (gears-ui-create-header-bar "Keyboard Mode" (current-buffer)) "\n")
  (widget-create 'radio-button-choice
                 :value "gears-keys"
                 :notify (lambda (widget &rest ignore)
                           (if (string= (widget-value widget) "vim-keys")
                               (customize-save-variable 'gears-use-evil t)
                             (customize-save-variable 'gears-use-evil nil)))
                 '(item :tag "Gears (modern)"
                        :value "gears-keys")
                 '(item :tag "Vi(m)-Like"
                        :value "vim-keys"))
  (widget-insert "\n" (gears-ui-create-header-bar "Theme" (current-buffer)) "\n")
  (widget-create 'radio-button-choice
                 :value gears-theme
                 :notify (lambda (widget &rest ignore)
                           (let ((value (widget-value widget)))
                             (customize-save-variable 'gears-theme value)
                             (load-theme gears-theme t)))
                 '(item :tag "Default (Moe Dark)"
                        :value moe-dark)
                 '(item :tag "Default (Moe Light)"
                        :value moe-light)
                 '(item :tag "Material (Dark)"
                        :value material)
                 '(item :tag "Material (Light)"
                        :value material-light))

  (widget-insert "\n" (gears-ui-create-header-bar "Additional Features" (current-buffer)) "\n")

  (dolist (layer (gears-layers-list-available))
    (load (concat "~/.emacs.d/layers/" layer "/init"))

    (widget-create 'checkbox
                   :value nil
                   :notify (lambda (widget &rest ignore)
                             (if (widget-value widget)
                                 (progn (add-to-list 'gic-layer-list layer))
                               (delete layer gic-layer-list))))
    (widget-insert " " layer "\n  " (gears-layer-get-description layer) "\n"))

  (widget-insert "\n\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (generate-new-buffer-name "*gears-install*")
                           (switch-to-buffer "*gears-install*")
                           (let ((inhibit-read-only t))
                             (erase-buffer))
                           (remove-overlays)

                           (princ gic-layer-list (current-buffer))

                           (dolist (layer gic-layer-list)
                             ;; Add layer to list beforehand so subsequent layers know it is installed.
                             ;; Important for additional functionality.
                             (add-to-list gears-layers-installed-list layer)
                             (customize-save-variable 'gears-layers-installed-list gears-layers-installed-list))
                           ;; Install all selected layers
                           (dolist (layer gic-layer-list)
                             (gears-layer-install layer)))
                 "Next")

  (use-local-map widget-keymap)
  (widget-setup))

(defun gears-install-default-packages()
  (interactive)

  (customize-save-variable 'gears-layers-installed-list '())
  (gears-install-packages gears-base-packages)

  (princ "Next step: " (current-buffer))
  (remove-overlays)
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (gears-install-configure))
                 "Configure")

  (use-local-map widget-keymap)
  (widget-setup)

  (moe-theme))