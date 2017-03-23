;;; Code:

(require 'git)
(require 'f)

(defun gears-update (&optional recompile)
  "Update the Emacs directory to the newest version.

If RECOMPILE is set, all layers and functions will be recompiled.

Recompiling increases performance."

  (interactive)

  (let ((git-repo gears-emacs-basepath))
    (git-stash)
    (git-pull "origin" gears-branch)
    (git-stash-pop))

  (gears-package-update-all-packages)

  (when (bound-and-true-p recompile)
    (gears-update-recompile)))

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