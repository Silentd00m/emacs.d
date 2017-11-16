(defgroup gears-interface nil
  "Gears' Interface Configuration"
  :group 'gears
  :link '(url-link :tag "Github" "https://github.com/Silentd00m/emacs.d"))

(defcustom gears-font "Source Code Pro-10"
  "The font to use."
  :type 'face
  :group 'gears-interface)

(defcustom gears-theme 'material
  "The used theme."
  :type 'symbol
  :group 'gears-interface)

(defcustom gears-theme-load-paths '(("moe-dark" . (f-glob (concat gears-emacs-basepath
                                                                  "/elpa/moe-theme*")))
                                    ("moe-light" . (f-glob (concat gears-emacs-basepath
                                                                   "/elpa/moe-theme*"))))
  "Additional theme paths."
  :type '(alist :value (group symbol symbol))
  :group 'gears-interface)

(defcustom gears-enable-tabbar nil
  "Enable buffer tabs."
  :type 'boolean
  :options '(nil t)
  :group 'gears-interface)

(defcustom gears-cursor-type 'bar
  "Defines how the cursor should be displayed.

Options:
 - bar: Vertical line, default in most modern text editors.
 - box: Solid block covering the following character. Like in most terminals.
 - hbar: Horizontal underscore line.
 - hollow: Hollow box.
 - nil: Do not show any cursor.
"
  :type 'sexp
  :options '('bar 'box 'hbar 'hollow nil)
  :group 'gears-interface)

(defcustom gears-hightlight-current-line t
  "Highlight the line the cursor is on."
  :type 'boolean
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
  :group 'gears-interface)

(defcustom gears-show-line-numbers t
  "Controls wheter line number should be displayed."
  :type 'boolean
  :group 'gears-interface)

(defcustom gears-powerline-theme 'powerline-default-theme
  "Controls wheter powerline (the bottom bar) should be used."
  :type 'function
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
  :group 'gears-interface)

(defcustom gears-enable-rainbow-delimiters t
  "Use colored parentheses, brackets and curly braces for better readability"
  :type 'boolean
  :group 'gears-interface)

(defcustom gears-column-highlight 80
  "The line where to start indicating overlength in programming modes."

  :type 'integer
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
  :group 'gears-interface)

(defcustom gears-enable-toolbar nil
  "Enable toolbar in GUI-mode."
  :type 'boolean
  :group 'gears-interface)

(defcustom gears-disable-auto-key-help nil
  "Disable the keymap popup on normal (Non-Mod) key combinations."
  :type 'boolean
  :group 'gears-interface)

(defcustom gears-show-minor-modes nil
  "Show all minor modes in modeline."
  :type 'boolean
  :group 'gears-interface)

(defcustom gears-show-file-sidebar nil
  "Show file sidebar."

  :type 'boolean
  :group 'gears-interface)

(defcustom gears-file-sidebar-with 20
  "File sidebar with in characters."

  :type 'integer
  :group 'gears-interface)

(defcustom gears-autoresize-splits t
  "Automatically resize window layout to give more space to the active buffer"

  :type 'integer
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
  :group 'gears-interface)

(defcustom gears-show-current-context nil
  "Show current namespace/function/class on top of the buffer.

Functions will be shown with their parameters."

  :type 'boolean
  :group 'gears-interface)

(defcustom gears-smooth-scrolling t
  "Improve the scrolling and Pg-Up/Pg-Down experience by animating the scrolling."

  :type 'boolean
  :group 'gears-interface)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable smooth scrolling

(when gears-smooth-scrolling
  (require 'sublimity)
  (require 'sublimity-scroll)

  (setq sublimity-scroll-weight 5
        sublimity-scroll-drift-length 10)

  (sublimity-mode 1)

  (setq scroll-margin 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gears' powerline themes

;; TODO : Create some themes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set configuration

(set-frame-font gears-font)
(set-face-attribute 'default nil :font gears-font)
(set-default-font gears-font)

(column-number-mode t)

;; Make sure we don't try to push a nil into the custom-theme-load-path, if theme
;; does not have an entry.
(when gears-theme
  (when (car (eval (cdr (assoc (prin1-to-string gears-theme)
                               gears-theme-load-paths))))
    (add-to-list 'custom-theme-load-path
                 (car (eval (cdr (assoc (prin1-to-string gears-theme)
                                        gears-theme-load-paths))))))
  (load-theme gears-theme t))

(when gears-hightlight-current-line
  (global-hl-line-mode t))

(when gears-show-paren-mode
  (show-paren-mode 1)
  (setq show-paren-style gears-show-paren-mode))

(when gears-cursor-type
  (modify-all-frames-parameters (list (cons 'cursor-type gears-cursor-type))))

(when gears-show-line-numbers
  (require 'nlinum-hl)

  (add-hook 'prog-mode-hook 'nlinum-mode)
  (add-hook 'text-mode-hook 'nlinum-mode)

  ;;(add-hook 'nlinum-mode-hook 'nlinum-hl-mode)
  (setq nlinum-highlight-current-line t)

  (add-hook 'focus-in-hook  #'nlinum-hl-flush-all-windows)
  (add-hook 'focus-out-hook #'nlinum-hl-flush-all-windows))

(when gears-powerline-theme
  (setq powerline-default-separator gears-powerline-shape)
  (funcall gears-powerline-theme))

(when gears-enable-rainbow-delimiters
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(cond ((eq gears-column-highlight-style 'both)
       (setq whitespace-line-column gears-column-highlight
             fci-rule-column gears-column-highlight)
       (setq whitespace-style '(face lines-tail))
       (add-hook 'prog-mode-hook 'whitespace-mode)
       (add-hook 'prog-mode-hook 'fci-mode))
      ((eq gears-column-highlight-style 'face)
       (setq whitespace-line-column gears-column-highlight)
       (setq whitespace-style '(face lines-tail))
       (add-hook 'prog-mode-hook 'whitespace-mode))
      ((eq gears-column-highlight-style 'line)
       (setq fci-rule-column gears-column-highlight)
       (add-hook 'prog-mode-hook 'fci-mode)))

(if gears-enable-toolbar
    (tool-bar-mode 1)
  (progn (tool-bar-mode -1)
         (setq tool-bar-mode nil)))

(when gears-enable-tabbar
  (tabbar-mode t)
  (setq tabbar-ruler-global-tabbar t) ; Use tabbar
  (setq tabbar-ruler-movement-timer-dealy 1000000))

(when gears-maximize-after-start
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(unless gears-disable-auto-key-help
  (which-key-mode t))

(when gears-autoresize-splits
  (setq zoom-ignored-buffer-name-regexps '("^*helm" "^helm"))
  (zoom-mode t))

(unless gears-show-minor-modes
  (require 'diminish)

  (add-hook 'after-init-hook
            #'(lambda ()
                (diminish 'company-mode)
                (diminish 'helm-mode)
                (diminish 'which-key-mode)
                (diminish 'undo-tree-mode)
                (diminish 'smartparens-mode)

                (eval-after-load "whitespace"
                  #'(diminish 'whitespace-mode))

                (eval-after-load "undo-tree"
                  #'(diminish 'undo-tree-mode))

                (eval-after-load "yasnippet"
                  #'(diminish 'eldoc-mode))

                (eval-after-load "yasnippet"
                  #'(diminish 'yas-minor-mode))

                (eval-after-load "abbrev"
                  #'(diminish 'abbrev-mode)))))

(when gears-show-file-sidebar
  (require 'treemacs)
  (require 's)

  (setq treemacs-width gears-file-sidebar-with)

  (when (gears-layer-installed-p 'git)
    (setq treemacs-git-integration t))

  (treemacs)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-persist)

  (add-hook 'projectile-after-switch-project-hook
            #'(lambda ()
                (treemacs-projectile))))

(cond ((equal gears-show-documentation-mode 'popup)
       (require 'pos-tip)

       (defun eldoc-popup-message (format-string &rest args)
         "Display eldoc message near point."
         (when format-string
           (pos-tip-show (apply 'format format-string args))))

       (setq eldoc-message-function #'eldoc-popup-message))
      ((equal gears-show-documentation-mode 'overlay)
       (add-hook 'eldoc-mode-hook 'eldoc-overlay-mode)))

(cond ((eq gears-scrollbar-mode 'yascroll)
       (add-to-list 'load-path (concat gears-emacs-basepath "/dep/yascroll-el/"))

       (require 'yascroll)

       (global-yascroll-bar-mode t)
       (scroll-bar-mode -1))
      ((eq gears-scrollbar-mode 'standard)
       (scroll-bar-mode t))
      ((eq gears-scrollbar-mode 'none)
       (scroll-bar-mode -1)))

(unless gears-show-current-context
  (add-to-list 'semantic-inhibit-functions
               (lambda ()
                 (member major-mode '(html-mode
                                      lisp-mode
                                      emacs-lisp-mode)))))

;; TODO : Add cursor and mode color configuration.

(when gears-use-evil
  (setq evil-normal-state-cursor '(box "green")
        evil-insert-state-cursor '(bar "DeepSkyBlue")
        evil-visual-state-cursor '(box "orange")
        evil-replace-state-cursor '(hbar "red")))
