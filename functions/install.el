(require 'cl)

(require 'wid-edit)

(load "~/.emacs.d/functions/gui")

(defun gears-install-configure()
  (interactive)

                                        ;(customize-group "gears")

  (generate-new-buffer-name "*gears-install*")
  (switch-to-buffer "*gears-install*")
  (erase-buffer)

  (widget-insert "Keyboard Mode:\n")
  (widget-create 'radio-button-choice
                 :value "gears-keys"
                 :notify (lambda (widget &rest ignore)
                           (when (string= (widget-value widget) "vim-keys")
                             (customize-save-variable 'gears-use-evil t)))
                 '(item :tag "Gears (modern)"
                        :value "gears-keys")
                 '(item :tag "Vi(m)-Like"
                        :value "vim-keys"))
  (widget-insert "\nTheme:\n")
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

  (use-local-map widget-keymap)
  (widget-setup))

(defun gears-install-default-packages()
  (interactive)

  (generate-new-buffer-name "*gears-install*")
  (switch-to-buffer "*gears-install*")

  (read-only-mode 0)
  (princ "Refreshing package repository cache before installation." (current-buffer))
  (read-only-mode t)

  ;(package-refresh-contents)

  (let ((gidp-installed-package-count 0))
    (dolist (pkg gears-base-packages)
      (unless (package-installed-p pkg)
        (package-install pkg))

      (setq gidp-installed-package-count (+ gidp-installed-package-count 1))

      (switch-to-buffer "*gears-install*")

      (read-only-mode 0)
      (erase-buffer)
      (princ "Please wait, installing Gears' base packages... [" (current-buffer))
      (princ gidp-installed-package-count (current-buffer))
      (princ "/" (current-buffer))
      (princ (length gears-base-packages) (current-buffer))
      (princ "]\n" (current-buffer))
      (gears-princ-progress-bar (current-buffer) (floor (* (/ (float gidp-installed-package-count) (length gears-base-packages)) 100)))
      (read-only-mode t)))

  (switch-to-buffer "*gears-install*")

  (read-only-mode 0)
  (princ "\n\n" (current-buffer))
  (princ "Installation complete. Next step: " (current-buffer))

  (remove-overlays)
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (gears-install-configure))
         "Configure")

  (use-local-map widget-keymap)
  (widget-setup)
  (read-only-mode t))
