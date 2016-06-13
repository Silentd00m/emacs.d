(defun gears-layers/base-init()
  (electric-indent-mode)
  (fset 'yes-or-no-p 'y-or-n-p) ;; shorten yes/no to y/n
  (setq scroll-conservatively 10000)
  (add-hook 'before-save-hook 'delete-trailing-whitespace) ;; Delete useless whitespace before saving
  (setq tramp-default-method "ssh")

  (setq create-lockfiles nil)  ;; No lock files
  (setq make-backup-files nil) ;; No backup files
  (setq scroll-step 1)

  (load (concat gears-emacs-basepath "/layers/base/keyboard"))
  (load (concat gears-emacs-basepath "/layers/base/interface"))

  (setq-default save-place t)
  (setq save-place-file (concat gears-emacs-basepath "/saved-places"))
  (setq save-place-forget-unreadable-files nil))

(defun gears-layers/base-remove()
  "Additional uninstall commands for the base-layer")

(defun gears-layers/base-install()
  "Additional install commands for the base-layer")

(setq gears-layers/base-depends '((packages . (bug-hunter
                                               fzf
                                               redo+
                                               smartparens
                                               comment-dwim-2
                                               use-package
                                               rainbow-delimiters
                                               nlinum
                                               multiple-cursors
                                               moe-theme
                                               powerline
                                               material-theme
                                               tabbar-ruler
                                               saveplace))
                                  (layers . ())))
