;;; Code:
;; Whitespace

(defun gears-layers/base/config-delete-trailing-whitespace (val)
  "Setter callback function for gears-delete-trailing-whitespace."

  (if val
      (add-hook 'before-save-hook 'delete-trailing-whitespace)
    (remove-hook 'before-save-hook 'delete-trailing-whitespace)))

;; Indentation

(defun gears-layers/base/config-indent-mode (val)
  "Setter callback function for gears-indent-mode."

  (if (eq val 'tabs)
      (setq indent-tabs-mode t)
    (setq indent-tabs-mode nil)))

(defun gears-layers/base/config-indent-width (val)
  "Setter callback function for gears-indent-width."

  (setq tab-width val))

(defun gears-layers/base/config-indent-comments (val)
  "Setter callback function for gears-indent-comments."

  (if gears-indent-comments
      (setq aggressive-indent-comments-too t)
    (setq aggressive-indent-comment-too nil)))

;; General

(defun gears-layers/base/config-insert-eof-newline (val)
  (setq require-final-newline val))

(defun gears-layers/base/config-disable-alarms (val)
  "Setter callback function for gears-disable-alarms."

  (if val
      (setq bell-volume 0
            ring-bell-function 'ignore)
    (setq bell-volume 100
          ring-bell-function nil)))

;; Theme

(defun gears-layers/base/config-font (val)
  "Setter callback function for gears-font."

  (set-frame-font val)
  (set-face-attribute 'default nil :font val)
  ;(set-default-font val)
  )

(defun gears-layers/base/config-theme (val)
  "Setter callback function for gears-theme."

  (when val
    ;; Add gears-theme-load-path to the custom theme load paths, if it is set.
    (unless (string= (prin1-to-string val) "")
      (load-theme val t))))

(defun gears-layers/base/config-theme-load-paths (val)
  "Setter callback function for gears-theme-load-paths."

  (when (car (eval (cdr (assoc (prin1-to-string val)
                               val))))
    (add-to-list 'custom-theme-load-path
                 (car (eval (cdr (assoc (prin1-to-string val)
                                        val)))))))

;; Interface

(defun gears-layers/base/config-enable-tabbar (val)
  "Setter callback function for gears-enable-tabbar."

  (if val
      (progn (tabbar-mode t)
             (setq tabbar-ruler-global-tabbar t
                   tabbar-ruler-movement-timer-delay 1000000))
    (tabbar-mode -1)))

(defun gears-layers/base/config-cursor-type (val)
  "Setter callback function for gears-cursor-type."

  (modify-all-frames-parameters (list (cons 'cursor-type val))))

(defun gears-layers/base/config-highlight-current-line (val)
  "Setter callback function for gears-highlight-current-line."

  (if val
      (progn (setq nlinum-highlight-current-line t)
             (global-hl-line-mode t))
    (progn (setq nlinum-highlight-current-line nil)
           (global-hl-line-mode nil))))

(defun gears-layers/base/config-show-paren-mode (val)
  "Setter callback function for gears-show-paren-mode."

  (if val
      (progn (show-paren-mode t)
             (setq show-paren-style gears-show-paren-mode))
    (show-paren-mode nil)))

(defun gears-layers/base/config-scrollbar-mode (val)
  "Setter callback function for gears-scrollbar-mode."

  (cond ((eq val 'yascroll)
         (add-to-list 'load-path (concat gears-emacs-basepath "/dep/yascroll-el/"))

         (require 'yascroll)

         (global-yascroll-bar-mode t)
         (scroll-bar-mode -1))
        ((eq val 'standard)
         (scroll-bar-mode t))
        ((eq val 'none)
         (scroll-bar-mode -1))))

(defun gears-layers/base/config-show-line-numbers (val)
  "Setter callback function for gears-show-line-numbers."

  (if (version<= emacs-version "26.0")
      (progn
        (require 'nlinum-hl)

        (if val
            (progn (add-hook 'prog-mode-hook 'nlinum-mode)
                   (add-hook 'text-mode-hook 'nlinum-mode)
                   (add-hook 'focus-in-hook 'nlinum-hl-flush-all-windows)
                   (add-hook 'focus-out-hook 'nlinum-hl-flush-all-windows))
          (progn (remove-hook 'prog-mode-hook 'nlinum-mode)
                 (remove-hook 'text-mode-hook 'nlinum-mode)
                 (remove-hook 'focus-in-hook 'nlinum-hl-flush-all-windows)
                 (remove-hook 'focus-out-hook 'nlinum-hl-flush-all-windows))))
    (progn
      (add-hook 'prog-mode-hook 'display-line-numbers-mode)
      (add-hook 'text-mode-hook 'display-line-numbers-mode))))

(defun gears-layers/base/config-powerline-theme (val)
  "Setter callback function for gears-powerline-theme."

  (funcall val))

(defun gears-layers/base/config-powerline-shape (val)
  "Setter callback function for gears-powerline-theme."

  (setq powerline-default-separator val)
  (funcall gears-powerline-theme))

(defun gears-layers/base/config-enable-rainbow-delimiters (val)
  "Setter callback function for gears-powerline-theme."

  (if val
      (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (remove-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(defun gears-layers/base/config-column-highlight (val)
  "Setter callback function for gears-column-highlight."

  (setq whitespace-line-column val
        fci-rule-column val))

(defun gears-layers/base/config-column-highlight-style (val)
  "Setter callback function for gears-column-highlight-style."

  (require 'whitespace)

  (cond ((eq val 'both)
         (add-to-list 'whitespace-style '(face lines-tail))

         (add-hook 'prog-mode-hook 'whitespace-mode)
         (add-hook 'prog-mode-hook 'fci-mode))
        ((eq val 'face)
         (add-to-list 'whitespace-style '(face lines-tail))
         (add-hook 'prog-mode-hook 'whitespace-mode))
        ((eq val 'line)
         (add-hook 'prog-mode-hook 'fci-mode))))

(defun gears-layers/base/config-helm-display-mode (val)
  "Setter callback function for gears-helm-display-mode"

  (cond ((eq val 'minibuffer)
         (setq helm-display-function 'helm-default-display-buffer
               helm-display-buffer-reuse-frame t
               helm-use-undecorated-frame-option t))
        ((eq val 'frame)
         (setq helm-display-function 'helm-display-buffer-in-own-frame
               helm-display-buffer-reuse-frame t
               helm-use-undecorated-frame-option t))
        ((eq val 'child-frame)
         (setq helm-display-function 'gears-helm-display-child-frame
               helm-display-buffer-reuse-frame t
               helm-display-buffer-width 80))))

(defun gears-layers/base/config-enable-toolbar (val)
  "Setter callback function for gears-enable-toolbar."

  (if val
      (tool-bar-mode t)
    (progn (tool-bar-mode -1)
           (setq tool-bar-mode nil))))

(defun gears-layers/base/config-disable-auto-key-help (val)
  "Setter callback function for gears-disable-auto-key-help."

  (if val
      (which-key-mode nil)
    (which-key-mode t)))

(defun gears-layers/base/config-show-minor-modes (val)
  "Setter callback function for gears-show-minor-modes."

  (if val
      (remove-hook 'after-init-hook
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
                         #'(diminish 'abbrev-mode))))
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
                    #'(diminish 'abbrev-mode))))))

(defun gears-layers/base/config-show-file-sidebar (val)
  "Setter callback function for gears-show-file-sidebar."

  (require 'treemacs)
  (require 's)

  (if val
      (progn   (treemacs)
               (treemacs-follow-mode t)
               (treemacs-filewatch-mode t)
               (treemacs-persist)

               (add-hook 'projectile-after-switch-project-hook
                         #'(lambda ()
                             (treemacs-projectile))))
    (remove-hook 'projectile-after-switch-project-hook
                 #'(lambda ()
                     (treemacs-projectile)))))

(defun gears-layers/base/config-autoresize-splits (val)
  "Setter callback function for gears-autoresize-splits."

  (setq zoom-ignored-buffer-names '("^*helm" "^helm"))

  (if val
      (zoom-mode t)
    (zoom-mode nil)))

(defun gears-layers/base/config-show-documentation-mode (val)
  "Setter callback function for gears-show-documentation-mode."

  (cond ((equal val 'popup)
         (require 'pos-tip)

         (defun eldoc-popup-message (format-string &rest args)
           "Display eldoc message near point."
           (when format-string
             (pos-tip-show (apply 'format format-string args))))

         (setq eldoc-message-function #'eldoc-popup-message))
        ((equal val 'overlay)
         (add-hook 'eldoc-mode-hook 'eldoc-overlay-mode)
         (setq eldoc-message-function #'inline-docs))
        ((equal val 'minibuffer)
         (global-eldoc-mode t))
        ((equal val 'none)
         (eldoc-mode nil))))

(defun gears-layers/base/config-show-current-context (val)
  "Setter callback function for gears-show-current-context."

  (when gears-enable-semantic-mode
    (if val
        (add-to-list 'semantic-inhibit-functions
                     (lambda ()
                       (member major-mode '(html-mode
                                            lisp-mode
                                            emacs-lisp-mode))))
      (setq semantic-inhibit-functions
            (remove (lambda ()
                      (member major-mode '(html-mode
                                           lisp-mode
                                           emacs-lisp-mode)))
                    semantic-inhibit-functions)))))

;; Keyboard

(defun gears-layers/base/config-global-keymap (val)
  "Setter callback function for gears-autoresize-splits."

  (dolist (i val)
    (global-set-key (kbd (car i)) (cdr i))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General config

(defgroup gears nil
  "Awesome Emacs configuration"
  :group 'environment
  :link '(url-link :tag "Github" "https://github.com/Silentd00m/emacs.d"))

(defcustom gears-branch "master"
  "The branch to use when updating."

  :type 'string
  :group 'gears)

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
