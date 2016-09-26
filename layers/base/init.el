(defun gears-layers/base-init()
  (defcustom gears-indent-mode 'spaces
    "Defines how text is indented.

Options:
 - tabs: Indent using tabs.
 - spaces: Indent using spaces. (Default)"
    :type 'symbolp
    :options '(tabs spaces)
    :group 'gears)

  (defcustom gears-indent-comments t
    "Indent comments?"
    :type 'boolean
    :group 'gears)

  (defcustom gears-delete-trailing-whitespace t
    "Delete trailing whitespace when saving?"
    :type 'boolean
    :group 'gears)

  (defcustom gears-indent-width 4
    "Defines how many spaces an indent is."
    :type 'integer
    :group 'gears)

  (defcustom gears-insert-eof-newline t
    "Insert a newline at the end of the file?"
    :type 'boolean
    :group 'gears)

  (defcustom gears-disable-alarms t
    "Disable bell and beep sounds?"
    :type 'boolean
    :group 'gears)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Set configuration

  (electric-indent-mode)
  (fset 'yes-or-no-p 'y-or-n-p) ;; shorten yes/no to y/n
  (setq scroll-conservatively 10000)
  (setq tramp-default-method "ssh")

  (setq create-lockfiles nil)  ;; No lock files
  (setq make-backup-files nil) ;; No backup files
  (setq frame-resize-pixelwise t) ;; Resize by pixels instead of rows

  (setq scroll-step 1)

  (when gears-delete-trailing-whitespace
    (add-hook 'before-save-hook 'delete-trailing-whitespace)) ;; Delete useless whitespace before saving

  (add-hook 'after-change-major-mode-hook (lambda()
                                            (unless (eq gears-indent-mode 'tabs)
                                              (setq indent-tabs-mode nil))

                                            (setq tab-width gears-indent-width)

                                            (when gears-insert-eof-newline
                                              (setq require-final-newline t))))

  (when gears-disable-alarms
    (setq ring-bell-function 'my-bell-function)
    (setq bell-volume 0)
    (setq ring-bell-function 'ignore))

  (when gears-indent-comments
    (setq aggressive-indent-comments-too t))

  (load (concat gears-emacs-basepath "/layers/base/helm"))
  (load (concat gears-emacs-basepath "/layers/base/keyboard"))
  (load (concat gears-emacs-basepath "/layers/base/interface"))

  (setq auto-save-default nil)

  (setq-default save-place t)
  (setq save-place-file (concat gears-emacs-basepath "/saved-places"))
  (setq save-place-forget-unreadable-files nil)
  (global-undo-tree-mode 1)
  (projectile-global-mode)
  (global-company-mode)
  (company-quickhelp-mode 1)
  (global-origami-mode t))

(defun gears-layers/base-remove()
  "Additional uninstall commands for the base-layer"

  t)

(defun gears-layers/base-install()
  "Additional install commands for the base-layer"

  t)

(gears-layer-defdepends base
                        :packages '(bug-hunter
                                    diminish
                                    undo-tree
                                    smartparens
                                    comment-dwim-2
                                    use-package
                                    rainbow-delimiters
                                    nlinum
                                    multiple-cursors
                                    powerline
                                    powerline-evil
                                    material-theme
                                    moe-theme
                                    tabbar-ruler
                                    saveplace
                                    git
                                    magit
                                    visual-regexp
                                    origami
                                    hydra
                                    projectile
                                    helm
                                    helm-make
                                    helm-projectile
                                    helm-flycheck
                                    helm-flx
                                    which-key)
                        :layers '(auto_completion))
