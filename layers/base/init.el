(load "custom")

(defgroup gears nil
  "Awesome Emacs configuration"
  :group 'environment
  :link '(url-link :tag "Github" "https://github.com/Silentd00m/emacs.d"))

(defcustom gears-branch "master"
  "The branch to use when updating."

  :type 'string
  :group 'gears)

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

  (defcustom gears-webserver-port 8081
    "Default port for gears' builtin configuration webserver."
    :type 'integer
    :group 'gears)

  (defcustom gears-enable-semantic-mode t
    "Enable semantic mode by default."

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
  (save-place-mode 1)

  (when gears-show-current-context
    (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

    (require 'stickyfunc-enhance))

  (when gears-enable-semantic-mode
    (require 'srefactor)

    (semantic-mode 1))

  (set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:")))))

(defun gears-layers/base-remove()
  "Additional uninstall commands for the base-layer"

  t)

(defun gears-layers/base-install()
  "Additional install commands for the base-layer"

  t)

(setq w3m-default-display-inline-images t)

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
                                    material-theme
                                    tabbar-ruler
                                    saveplace
                                    git
                                    magit
                                    visual-regexp
                                    origami
                                    hydra
                                    projectile
                                    projectile-speedbar
                                    helm
                                    helm-make
                                    helm-projectile
                                    helm-flycheck
                                    helm-flx
                                    helm-descbinds
                                    which-key
                                    sr-speedbar
                                    stickyfunc-mode
                                    w3m)
                        :layers '(auto_completion))
