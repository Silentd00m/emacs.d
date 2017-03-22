(require 'git)

(defun gears-update ()
  "Updates the emacs directory to the newest version."

  (interactive)

  (setq git-repo gears-emacs-basepath)
  (git-stash)
  (git-pull "origin" gears-branch)
  (git-stash-pop)

  (gears-package-update-all-packages))
