;;; Code:

(require 'git)
(require 'f)

(defun gears-update ()
  "Update the Emacs directory to the newest version.

If RECOMPILE is set, all layers and functions will be recompiled."

  (interactive)

  (let* ((git-repo gears-emacs-basepath)
         (stash (git-stash)))
    (git-pull "origin" gears-branch)
    (when stash
      (git-stash-pop)))

  ;; Reload layer definitions to update dependencies.
  (dolist (layer gears-layer-installed-list)
    (gears-load-layer layer))

  (get-buffer-create "*gears-install*")
  (switch-to-buffer "*gears-install*")

  (package-refresh-contents)

  (read-only-mode t)

  (let ((inhibit-read-only t))
    (princ "Refreshing package repository cache before update."
           (current-buffer)))

  (package-refresh-contents)

  ;; Update all layers, install missing dependencies.
  (dolist (layer gears-layer-installed-list)
    (gears-layer-update layer))

  ;; Update all outdated packages
  (gears-package-update-all-packages)

  ;; Recompile
  (gears-update-recompile))

(defun gears-update-recompile ()
  "Recompile all elisp files to increase performance."

  (interactive)

  (byte-recompile-directory (expand-file-name "~/.emacs.d/elpa") 0)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/functions") 0)

  (dolist (layer (f-directories (concat gears-emacs-basepath "/layers/")))
    (load (concat layer "/init"))

    (unless (boundp (intern (concat "gears-layers/"
                                    (f-filename layer)
                                    "-do-not-compile")))
      (byte-recompile-directory layer))))
