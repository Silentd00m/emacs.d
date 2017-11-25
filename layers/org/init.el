(defgroup gears-layers/org nil
  "Org-mode Layer Configuration"

  :group 'gears-layers)

(defcustom gears-layers/org-default-editor-bindings t
  "Allow the use of standard editor keys like Shift+Arrow to function as normal."
  :type 'boolean
  :group 'gears-layers/org)

(defun gears-layers/org-init ()
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

  (setq org-replace-disputed-keys gears-layers/org-default-editor-bindings)
  (setq org-support-shift-select 'always)
  (defun myorg-update-parent-cookie ()
    (when (equal major-mode 'org-mode)
      (save-excursion
        (ignore-errors
          (org-back-to-heading)
          (org-update-parent-todo-statistics)))))

  (defadvice org-kill-line (after fix-cookies activate)
    (myorg-update-parent-cookie))

  (defadvice kill-whole-line (after fix-cookies activate)
    (myorg-update-parent-cookie)))

(defun gears-layers/org-description ()
  "Returns the description of the org Layer."

  "Supporting functions for org.")

(defun gears-layers/org-pre-install ()
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-refresh-contents))

(gears-layer-defdepends org
                        :packages '(org org-plus-contrib))
