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
  (load (concat gears-emacs-basepath "/layers/base/config"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; General config

  (defcustom gears-indent-mode 'spaces
    "Defines how text is indented.

Options:
 - tabs: Indent using tabs.
 - spaces: Indent using spaces. (Default)"
    :type 'symbolp
    :options '(tabs spaces)
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-indent-mode val))
    :group 'gears)

  (defcustom gears-indent-comments t
    "Indent comments?"
    :type 'boolean
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-indent-comments val))
    :group 'gears)

  (defcustom gears-delete-trailing-whitespace t
    "Delete trailing whitespace when saving?"
    :type 'boolean
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-delete-trailing-whitespace val))
    :group 'gears)

  (defcustom gears-indent-width 4
    "Defines how many spaces an indent is."
    :type 'integer
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-indent-width val))
    :group 'gears)

  (defcustom gears-insert-eof-newline t
    "Insert a newline at the end of the file?"
    :type 'boolean
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-insert-eof-newline val))
    :group 'gears)

  (defcustom gears-disable-alarms t
    "Disable bell and beep sounds?"
    :type 'boolean
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-disable-alarms val))
    :group 'gears)

  (defcustom gears-webserver-port 8081
    "Default port for gears' builtin configuration webserver."
    :type 'integer
    :group 'gears)

  (defcustom gears-enable-semantic-mode nil
    "Enable semantic mode by default."

    :type 'boolean
    :group 'gears)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Interface config

  (defgroup gears-interface nil
    "Gears' Interface Configuration"
    :group 'gears
    :link '(url-link :tag "Github" "https://github.com/Silentd00m/emacs.d"))

  (defcustom gears-font "Source Code Pro-10"
    "The font to use."
    :type 'face
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-font val))
    :group 'gears-interface)

  (defcustom gears-theme 'material
    "The used theme."
    :type 'symbol
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-theme val))
    :group 'gears-interface)

  (defcustom gears-theme-load-paths '(("moe-dark" . (f-glob (concat gears-emacs-basepath
                                                                    "/elpa/moe-theme*")))
                                      ("moe-light" . (f-glob (concat gears-emacs-basepath
                                                                     "/elpa/moe-theme*"))))
    "Additional theme paths."
    :type '(alist :value (group symbol symbol))
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-theme-load-paths val))
    :group 'gears-interface)

  (defcustom gears-enable-tabbar nil
    "Enable buffer tabs."
    :type 'boolean
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-enable-tabbar val))
    :options '(nil t)
    :group 'gears-interface)

  (defcustom gears-cursor-type 'bar
    "Defines how the cursor should be displayed.

    Options:
    - bar: Vertical line, default in most modern text editors.
    - box: Solid block covering the following character. Like in most terminals.
    - hbar: Horizontal underscore line.
    - hollow: Hollow box.
    - nil: Do not show any cursor."
    :type 'sexp
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-cursor-type val))
    :options '('bar 'box 'hbar 'hollow nil)
    :group 'gears-interface)

  (defcustom gears-hightlight-current-line t
    "Highlight the line the cursor is on."
    :type 'boolean
    :initialize #'(lambda (sym val)
                    (custom-initialize-reset sym val)
                    (gears-layers/base/config-highlight-current-line val))
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-highlight-current-line val))
    :group 'gears-interface)

  (defcustom gears-show-paren-mode 'expression
    "Defines how parentheses should be highlightd.

    Options:
    - expression: Highlight entire expression
    - parenthesis: Highlight only parentheses
    - mixed: Highlight parentheses if visible, else fall back to expression
    - nil: Deactivate
    "
    :type 'symbolp
    :options '('expression 'parenthesis 'mixed nil)
    :group 'gears-interface)

  (defcustom gears-maximize-after-start t
    "If set to true, (gui-)window will maximize after startup."
    :type 'boolean
    :group 'gears-interface)

  (defcustom gears-scrollbar-mode 'yascroll
    "Defines how scrollbars are displayed.

    Options:
    - standard: Emacs' standard scrollbars.
    - yascroll: GTK-Theme indepent ascii scrollbars (Default).
    - none: No scrollbars"
    :options '(standard yascroll none)
    :type 'symbolp
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-scrollbar-mode val))
    :group 'gears-interface)

  (defcustom gears-show-line-numbers t
    "Controls wheter line number should be displayed."
    :type 'boolean
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-show-line-numbers val))
    :group 'gears-interface)

  (defcustom gears-powerline-theme 'powerline-default-theme
    "Controls wheter powerline (the bottom bar) should be used."
    :type 'function
    :type 'symbolp
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-powerline-theme val))
    :group 'gears-interface)

  (defcustom gears-powerline-shape 'slant
    "Defines the shape of the powerline separators.

    Options:
    - arrow: Left or right facing arrows.
    - half: Flat / No separaters, just a few pixels of space.
    - curve: Scallop curve.
    - rounded: Like half, but with rounded corners.
    - chamfer: 'Folded' corner on top.
    - slant: Diagonar from left or right.
    - slant-left: Diagonal from left.
    - slant-right: Diagonal from right."

    :options '('arrow 'half 'curve 'rounded 'chamfer 'slant 'slant-left 'slant-right)
    :type 'symbol
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-powerline-shape val))
    :group 'gears-interface)

  (defcustom gears-enable-rainbow-delimiters t
    "Use colored parentheses, brackets and curly braces for better readability"

    :type 'boolean
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-enable-rainbow-delimiters val))
    :group 'gears-interface)

  (defcustom gears-column-highlight 80
    "The line where to start indicating overlength in programming modes."

    :type 'integer
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-column-highlight val))
    :group 'gears-interface)

  (defcustom gears-column-highlight-style 'face
    "Specify how to indicate line overlength in programming modes.

    Options:
    - face: Highlight the fore- and/or background of any character after the limit.
    - line: Draw a line after the defined column-highlight position.
    - both: Both of the above.
    - disabled: None of the above."

    :options '(face line both disabled)
    :type 'symbol
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-column-highlight-style val))
    :group 'gears-interface)

  (defcustom gears-enable-toolbar nil
    "Enable toolbar in GUI-mode."

    :type 'boolean
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-enable-toolbar val))
    :group 'gears-interface)

  (defcustom gears-disable-auto-key-help nil
    "Disable the keymap popup on normal (Non-Mod) key combinations."

    :type 'boolean
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-disable-auto-key-help val))
    :group 'gears-interface)

  (defcustom gears-show-minor-modes nil
    "Show all minor modes in modeline."

    :type 'boolean
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-show-minor-modes val))
    :group 'gears-interface)

  (defcustom gears-show-file-sidebar nil
    "Show file sidebar."

    :type 'boolean
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-show-file-sidebar val))
    :group 'gears-interface)

  (defcustom gears-file-sidebar-with 20
    "File sidebar with in characters."

    :type 'integer
    :group 'gears-interface)

  (defcustom gears-autoresize-splits t
    "Automatically resize window layout to give more space to the active buffer"

    :type 'integer
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-autoresize-splits val))
    :group 'gears-interface)

  (defcustom gears-show-documentation-mode 'minibuffer
    "Defines how the code documentation should be shown

    Options:
    - popup: Documentation is shown in a popup window near the cursor.
    - minibuffer: Documentation is shown in the minibuffer (with color)
    - overlay: Documentation is shown in a text overlay above the cursor.
    - none: Do not show the documentation."

    :options '('popup 'minibuffer 'overlay 'none)
    :type 'symbol
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-show-documentation-mode val))
    :group 'gears-interface)

  (defcustom gears-show-current-context nil
    "Show current namespace/function/class on top of the buffer.

    Functions will be shown with their parameters."

    :type 'boolean
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-show-current-context val))
    :group 'gears-interface)

  (defcustom gears-helm-display-mode 'frame
    "Defines how the helm will be shown.

Options:
- minibuffer: Show helm inside the minibuffer.
- frame: Show helm inside a frame at the cursor position.
- child-frame: Create a child-frame at the cursor position."

    :type 'symbol
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-helm-display-mode val))
    :group 'gears-interface)

  (defcustom gears-smooth-scrolling t
    "Improve the scrolling and Pg-Up/Pg-Down experience by animating the scrolling."

    :type 'boolean
    :set #'(lambda (sym val)
             (custom-set-default sym val)
             (gears-layers/base/config-smooth-scrolling val))
    :group 'gears-interface)

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

  (add-hook 'after-change-major-mode-hook
            #'(lambda()
                (gears-layers/base/config-indent-mode gears-indent-mode)
                (gears-layers/base/config-indent-width gears-indent-width)
                (gears-layers/base/config-insert-eof-newline
                 gears-insert-eof-newline)))

  (when gears-indent-comments
    (setq aggressive-indent-comments-too t))

  (load (concat gears-emacs-basepath "/layers/base/helm"))
  (load (concat gears-emacs-basepath "/layers/base/keyboard"))
  ;; (load (concat gears-emacs-basepath "/layers/base/interface"))

  (setq auto-save-default nil)

  (setq-default save-place t)
  (setq save-place-file (concat gears-emacs-basepath "/saved-places"))
  (setq save-place-forget-unreadable-files nil)
  ;(global-undo-tree-mode 1)
  (projectile-global-mode)
  (global-company-mode)
  (company-quickhelp-mode 1)
  (save-place-mode 1)

  (column-number-mode t)

  ;; General
  (gears-layers/base/config-delete-trailing-whitespace gears-delete-trailing-whitespace)
  (gears-layers/base/config-indent-mode gears-indent-mode)
  (gears-layers/base/config-indent-width gears-indent-width)
  (gears-layers/base/config-disable-alarms gears-disable-alarms)

  ;; Interface
  (gears-layers/base/config-theme-load-paths gears-theme-load-paths)
  (gears-layers/base/config-theme gears-theme)
  (gears-layers/base/config-highlight-current-line gears-hightlight-current-line)
  (gears-layers/base/config-show-paren-mode gears-show-paren-mode)
  (gears-layers/base/config-cursor-type gears-cursor-type)
  (gears-layers/base/config-show-line-numbers gears-show-line-numbers)
  (gears-layers/base/config-powerline-shape gears-powerline-shape)
  (gears-layers/base/config-powerline-theme gears-powerline-theme)
  (gears-layers/base/config-enable-rainbow-delimiters gears-enable-rainbow-delimiters)
  (gears-layers/base/config-column-highlight gears-column-highlight)
  (gears-layers/base/config-column-highlight-style gears-column-highlight-style)
  (gears-layers/base/config-enable-toolbar gears-enable-toolbar)
  (gears-layers/base/config-enable-tabbar gears-enable-tabbar)
  (gears-layers/base/config-disable-auto-key-help gears-disable-auto-key-help)
  (gears-layers/base/config-autoresize-splits gears-autoresize-splits)
  (gears-layers/base/config-show-minor-modes gears-show-minor-modes)
  (gears-layers/base/config-show-file-sidebar gears-show-file-sidebar)
  (gears-layers/base/config-show-documentation-mode gears-show-documentation-mode)
  (gears-layers/base/config-scrollbar-mode gears-scrollbar-mode)
  (gears-layers/base/config-show-current-context gears-show-current-context)
  (gears-layers/base/config-smooth-scrolling gears-smooth-scrolling)

  (when gears-maximize-after-start
    (add-to-list 'default-frame-alist '(fullscreen . maximized)))

  (when gears-use-evil
    (setq evil-normal-state-cursor '(box "green")
          evil-insert-state-cursor '(bar "DeepSkyBlue")
          evil-visual-state-cursor '(box "orange")
          evil-replace-state-cursor '(hbar "red")))

  ;; Fix tramp hanging when the shell output is colored
  (setq shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

  (when gears-show-current-context
    (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

    (require 'stickyfunc-enhance))

  (when gears-enable-semantic-mode
    (semantic-mode 1))

  (set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:")))))

(defun gears-layers/base-remove ()
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
                                    rainbow-delimiters
                                    hideshowvis
                                    (when (version<= emacs-version "26.0")
                                      nlinum
                                      nlinum-hl)
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
                                    treemacs
                                    stickyfunc-enhance
                                    w3m
                                    fill-column-indicator
                                    sublimity
                                    switch-buffer-functions
                                    zoom
                                    yascroll)
                        :layers '(auto_completion))
