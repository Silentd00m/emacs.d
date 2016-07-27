(defgroup gears-interface nil
  "Gears' Interface Configuration"
  :group 'gears
  :link '(url-link :tag "Github" "https://github.com/Silentd00m/emacs.d"))

(defcustom gears-font "Source Code Pro-10"
  "The font to use."
  :type 'face
  :group 'gears-interface)

(defcustom gears-theme 'moe-dark
  "The used theme."
  :type 'symbolp
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
  :type 'symbolp
  :group 'gears-interface)

(defcustom gears-enable-rainbow-delimiters t
  "Use colored parentheses, brackets and curly braces for better readability"
  :type 'boolean
  :group 'gears-interface)

(defcustom gears-highlight-after-column 80
  "Highlights any character after the provided column number.

Set to nil to disable."
  :type 'integer
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gears' powerline themes

;; TODO : Create some themes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set configuration

(set-frame-font gears-font)
(set-face-attribute 'default nil :font gears-font)
(set-default-font gears-font)

(column-number-mode t)

(load-theme gears-theme t)

(when gears-hightlight-current-line
  (global-hl-line-mode t))

(when gears-show-paren-mode
  (show-paren-mode 1)
  (setq show-paren-style gears-show-paren-mode))

(when gears-cursor-type
  (modify-all-frames-parameters (list (cons 'cursor-type gears-cursor-type))))

(when gears-show-line-numbers
  (use-package nlinum
    :ensure t
    :init (global-nlinum-mode)))

(when gears-powerline-theme
  (use-package powerline
    :ensure t
    :config (progn (setq powerline-default-separator gears-powerline-shape)
                   (funcall gears-powerline-theme))))

(when gears-enable-rainbow-delimiters
  (use-package rainbow-delimiters
    :ensure t
    :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(when (not (< gears-highlight-after-column 0))
  (use-package whitespace
    :ensure t
    :config (progn (setq whitespace-line-column gears-highlight-after-column)
                   (setq whitespace-style '(face lines-tail))
                   (add-hook 'prog-mode-hook 'whitespace-mode))))

(if gears-enable-toolbar
    (tool-bar-mode 1)
  (progn (tool-bar-mode -1)
         (setq tool-bar-mode nil)))

(when gears-enable-tabbar
  (use-package tabbar-ruler
    :ensure tabbar-ruler
    :config (progn (tabbar-mode t)
                   (setq tabbar-ruler-global-tabbar t) ; Use tabbar
                   (setq tabbar-ruler-movement-timer-dealy 1000000))))

(when gears-maximize-after-start
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(unless gears-disable-auto-key-help
  (which-key-mode t))

(unless gears-show-minor-modes
  (require 'diminish)

  (diminish 'company-mode)
  (diminish 'helm-mode)
  (diminish 'which-key-mode)
  (diminish 'whitespace-mode)
  (diminish 'undo-tree-mode)
  (diminish 'smartparens-mode))

;; TODO : Add cursor and mode color configuration.

(when gears-use-evil
  (setq evil-normal-state-cursor '(box "green")
        evil-insert-state-cursor '(bar "DeepSkyBlue")
        evil-visual-state-cursor '(box "orange")
        evil-replace-state-cursor '(hbar "red")))
