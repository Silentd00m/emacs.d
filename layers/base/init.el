;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base

;; Make sure deadkeys work
(require 'iso-transl)

(fset 'yes-or-no-p 'y-or-n-p) ;; shorten yes/no to y/n
(setq scroll-conservatively 10000)
(setq tramp-default-method "ssh")

(setq create-lockfiles nil)  ;; No lock files
(setq make-backup-files nil) ;; No backup files
(setq frame-resize-pixelwise t) ;; Resize by pixels instead of rows

(use-package undo-tree
  :ensure t
  :bind (("C-u" . undo-tree)))

(use-package comment-dwim-2
  :ensure t
  :bind (("M-," . comment-dwim-2)))

(use-package flycheck
  :ensure t
  :hook (prog-mode-hook . flycheck-mode)
  :diminish flycheck-mode)

(use-package flycheck-pos-tip
  :ensure t
  :config (with-eval-after-load 'flycheck
	    (flycheck-pos-tip-mode)))

(use-package helm
  :ensure t
  :bind (("C-p" . gears-helm-mini)
         ("C-o" . helm-find-files)
         ("M-x" . helm-M-x)
         ("<f7>" . helm-semantic-or-imenu)

         :map helm-map
         ("<left>" . helm-previous-source)
         ("<right>" . helm-next-source)
         ("<tab>" . helm-execute-persistent-action))
  :init (progn (require 'helm-config)

               (helm-mode 1)
               (setq helm-quick-update t
		     helm-X-x-fuzzy-match t
		     helm-buffers-fuzzy-matching t
		     helm-recentf-fuzzy-matching t
		     helm-move-to-line-cycle-in-source t
		     helm-semantic-fuzzy-match t
		     helm-apropos-fuzzy-match t
		     helm-imenu-fuzzy-match t
		     helm-candidate-number-limit 30)
               (helm-autoresize-mode 1)
               (advice-add 'helm-ff-delete-char-backward :around #'gears/helm-find-files-navigate-back)

               (when (>= emacs-major-version 27)
                   (with-eval-after-load 'helm-mode
                     (dolist (face '(helm-source-header helm-selection))
                       (set-face-attribute face nil :extend t))))))

(use-package helm-flx
  :ensure t
  :config (progn (setq helm-flx-for-helm-find-files t
                       helm-flx-for-helm-locate t)
                 (helm-flx-mode +1)))

(use-package helm-projectile
  :ensure t
  :config (projectile-global-mode))

(use-package helm-flycheck
  :ensure t)

(use-package visual-regexp
  :bind ("C-h" . vr/replace)
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package hydra
  :ensure t)

(use-package projectile
  :ensure t)

(use-package smartparens
  :ensure t
  :config (progn (require 'smartparens-config)
                 (smartparens-global-mode t)))

(use-package origami
  :ensure t)

(when (version<= emacs-version "26.0")
  (use-package nlinum
    :ensure t)
  (use-package nlinum-hl
    :ensure t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look and Feel

;; General
(setq shift-select-mode t                                                       ;; Allow shift+arrow selection
      cua-keep-region-after-copy t
      x-select-enable-clipboard t
      select-enable-clipboard t)
(cua-mode t)                                                                    ;; Sane C-c, C-x and C-v
(transient-mark-mode 1)
(setq scroll-step 1)

(whitespace-mode -1)

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package powerline
  :ensure t)

(use-package hl-line
  :init (when (>= emacs-major-version 27)
          (with-eval-after-load 'hl-line
            (set-face-attribute 'hl-line nil :extend t))))

(use-package zoom
  :ensure t
  :config (progn (setq zoom-size '(0.618 . 0.618))
                 (when (bound-and-true-p gears-autoresize-splits)
                   (zoom-mode))))

(use-package fill-column-indicator
  :ensure t
  :config (progn
            (setq fci-rule-column 80)
            (add-hook 'prog-mode-hook 'fci-mode)))

(use-package diminish
  :ensure t)

(use-package yascroll
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package material-theme
  :ensure t)

(use-package treemacs
  :ensure t)

(diminish 'eldoc-mode)
(diminish 'auto-revert-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General dependencies

(use-package aggressive-indent
  :ensure t)

(use-package use-package-hydra
  :ensure t
  :after hydra)

(use-package git
  :ensure t)

(use-package ranger
  :ensure t
  :after use-package-hydra
  :hydra)

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

           (if (eq val 'tabs)
               (setq indent-tabs-mode t)
             (setq indent-tabs-mode nil)))
  :group 'gears)

(defcustom gears-indent-comments t
  "Indent comments?"
  :type 'boolean
  :set #'(lambda (sym val)
           (custom-set-default sym val)

           (if gears-indent-comments
               (setq aggressive-indent-comments-too t)
             (setq aggressive-indent-comment-too nil)))
  :group 'gears)

(defcustom gears-delete-trailing-whitespace t
  "Delete trailing whitespace when saving?"
  :type 'boolean
  :set #'(lambda (sym val)
           (custom-set-default sym val)

           (if val
               (add-hook 'before-save-hook 'delete-trailing-whitespace)
             (remove-hook 'before-save-hook 'delete-trailing-whitespace)))
  :group 'gears)

(defcustom gears-indent-width 4
  "Defines how many spaces an indent is."
  :type 'integer
  :set #'(lambda (sym val)
           (custom-set-default sym val)

           (setq tab-width val))
  :group 'gears)

(defcustom gears-insert-eof-newline t
  "Insert a newline at the end of the file?"
  :type 'boolean
  :set #'(lambda (sym val)
           (custom-set-default sym val)

           (setq require-final-newline val))
  :group 'gears)

(defcustom gears-disable-alarms t
  "Disable bell and beep sounds?"

  :type 'boolean
  :set #'(lambda (sym val)
           (custom-set-default sym val)

           (if val
               (setq bell-volume 0
                     ring-bell-function 'ignore)
             (setq bell-volume 100
                   ring-bell-function nil)))
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

           (set-frame-font val)
           (set-face-attribute 'default nil :font val))
  :group 'gears-interface)

(defcustom gears-theme 'material
  "The used theme."

  :type 'symbol
  :set #'(lambda (sym val)
           (custom-set-default sym val)

           (when val
             ;; Add gears-theme-load-path to the custom theme load paths, if it is set.
             (unless (string= (prin1-to-string val) "")
               (load-theme val t))))
  :group 'gears-interface)

(defcustom gears-theme-load-paths '(("moe-dark" . (f-glob (concat gears-emacs-basepath
                                                                  "/elpa/moe-theme*")))
                                    ("moe-light" . (f-glob (concat gears-emacs-basepath
                                                                   "/elpa/moe-theme*"))))
  "Additional theme paths."
  :type '(alist :value (group symbol symbol))
  :set #'(lambda (sym val)
           (custom-set-default sym val)

           (when (car (eval (cdr (assoc (prin1-to-string val)
                                        val))))
             (add-to-list 'custom-theme-load-path
                          (car (eval (cdr (assoc (prin1-to-string val)
                                                 val)))))))
  :group 'gears-interface)

(when (boundp 'tabbar-mode)
  (defcustom gears-enable-tabbar nil
    "Enable buffer tabs."
    :type 'boolean
    :set #'(lambda (sym val)
             (custom-set-default sym val)

             (if val
                 (progn (tabbar-mode t)
                        (setq tabbar-ruler-global-tabbar t
                              tabbar-ruler-movement-timer-delay 1000000))
               (tabbar-mode -1)))
    :options '(nil t)
    :group 'gears-interface))

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

           (modify-all-frames-parameters (list (cons 'cursor-type val))))
  :options '('bar 'box 'hbar 'hollow nil)
  :group 'gears-interface)

(defcustom gears-hightlight-current-line t
  "Highlight the line the cursor is on."
  :type 'boolean
  :set #'(lambda (sym val)
           (custom-set-default sym val)

           (if val
               (progn (setq nlinum-highlight-current-line t)
                      (global-hl-line-mode t))
             (progn (setq nlinum-highlight-current-line nil)
                    (global-hl-line-mode nil))))
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
  :set #'(lambda (sym val)
           (custom-set-default sym val)

           (if val
               (progn (show-paren-mode t)
                      (setq show-paren-style gears-show-paren-mode))
             (show-paren-mode nil)))
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

           (cond ((eq val 'yascroll)
                  (add-to-list 'load-path (concat gears-emacs-basepath "/dep/yascroll-el/"))

                  (require 'yascroll)

                  (global-yascroll-bar-mode t)
                  (scroll-bar-mode -1))
                 ((eq val 'standard)
                  (scroll-bar-mode t))
                 ((eq val 'none)
                  (scroll-bar-mode -1))))
  :group 'gears-interface)

(defcustom gears-show-line-numbers t
  "Controls wheter line number should be displayed."
  :type 'boolean
  :set #'(lambda (sym val)
           (custom-set-default sym val)

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
  :group 'gears-interface)

(defcustom gears-powerline-theme 'powerline-default-theme
  "Controls wheter powerline (the bottom bar) should be used."
  :type 'function
  :type 'symbolp
  :set #'(lambda (sym val)
           (custom-set-default sym val)

           (funcall val))
  :group 'gears-interface)

(defcustom gears-powerline-shape 'slant
  "Defines the shape of the powerline separators.

    Options:
    - arrow: Left or right facing arrows.
    - half: Flat / No separators, just a few pixels of space.
    - curve: Scallop curve.
    - rounded: Like half, but with rounded corners.
    - chamfer: 'Folded' corner on top.
    - slant: Diagonal from left or right.
    - slant-left: Diagonal from left.
    - slant-right: Diagonal from right."

  :options '('arrow 'half 'curve 'rounded 'chamfer 'slant 'slant-left 'slant-right)
  :type 'symbol
  :set #'(lambda (sym val)
           (setq powerline-default-separator val)
           (funcall gears-powerline-theme))
  :group 'gears-interface)

(defcustom gears-enable-rainbow-delimiters t
  "Use colored parentheses, brackets and curly braces for better readability"

  :type 'boolean
  :set #'(lambda (sym val)
           (custom-set-default sym val)

           (if val
               (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
             (remove-hook 'prog-mode-hook 'rainbow-delimiters-mode)))
  :group 'gears-interface)

(defcustom gears-column-highlight 80
  "The line where to start indicating overlength in programming modes."

  :type 'integer
  :set #'(lambda (sym val)
           (custom-set-default sym val)

           (setq whitespace-line-column val
                 fci-rule-column val))
  :group 'gears-interface)

(defcustom gears-column-highlight-style 'line
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
  :group 'gears-interface)

(defcustom gears-enable-toolbar nil
  "Enable toolbar in GUI-mode."

  :type 'boolean
  :set #'(lambda (sym val)
           (custom-set-default sym val)
           (if val
               (tool-bar-mode t)
             (progn (tool-bar-mode -1)
                    (setq tool-bar-mode nil))))
  :group 'gears-interface)

(defcustom gears-enable-menu-bar nil
  "Specify whether the menu bar should be shown."

  :type 'boolean
  :set #'(lambda (sym val)
           (custom-set-default sym val)

           (if val
               (menu-bar-mode t)
             (menu-bar-mode -1)))
  :group 'gears-interface)

(defcustom gears-disable-auto-key-help nil
  "Disable the keymap popup on normal (Non-Mod) key combinations."

  :type 'boolean
  :set #'(lambda (sym val)
           (custom-set-default sym val)

           (if val
               (which-key-mode nil)
             (which-key-mode t)))
  :group 'gears-interface)

(defcustom gears-show-file-sidebar nil
  "Show file sidebar."

  :type 'boolean
  :set #'(lambda (sym val)
           (custom-set-default sym val)

           (require 'treemacs)
           (require 's)

           (if val
               (progn (treemacs)
                      (treemacs-follow-mode t)
                      (treemacs-filewatch-mode t)
                      (treemacs-persist)

                      (add-hook 'projectile-after-switch-project-hook
                                #'(lambda ()
                                    (treemacs-projectile))))
             (remove-hook 'projectile-after-switch-project-hook
                          #'(lambda ()
                              (treemacs-projectile)))))
  :group 'gears-interface)

(defcustom gears-file-sidebar-width 20
  "File sidebar width in characters."

  :type 'integer
  :group 'gears-interface)

(defcustom gears-autoresize-splits t
  "Automatically resize window layout to give more space to the active buffer"

  :type 'integer
  :set #'(lambda (sym val)
           (custom-set-default sym val)

           (setq zoom-ignored-buffer-names '("^*helm" "^helm"))

           (if val
               (zoom-mode t)
             (zoom-mode nil)))
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
  :group 'gears-interface)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apply other configs

(electric-indent-mode)

;(add-hook 'after-change-major-mode-hook
;          #'(lambda()
;              (gears-layers/base/config-indent-mode gears-indent-mode)
;              (gears-layers/base/config-indent-width gears-indent-width)
;              (gears-layers/base/config-insert-eof-newline
;               gears-insert-eof-newline)))

;(gears-layers/base/config-show-paren-mode gears-show-paren-mode)

(when gears-indent-comments
  (setq aggressive-indent-comments-too t))

(setq column-number-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Fix tramp hanging when the shell output is colored
(setq shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"
      tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

(when gears-maximize-after-start
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(load (concat gears-emacs-basepath "/functions/hydra"))

(defdynhydra 'M-a '(("s" align-current "Selection" :column "Align" :exit t)
                    ("e" align-enture "Everything" :column "Align" :exit t)))
(defdynhydra 'M-b '())
(defdynhydra 'M-c '(("L" downcase-dwim "Lowercase" :column "Convert" :exit t)
                    ("U" upcase-dwim "Uppercase" :column "Convert" :exit t)
                    ("C" capitalize-dwim "Capitalize" :column "Convert" :exit t)))
(defdynhydra 'M-d '())
(defdynhydra 'M-e '(("b" eval-buffer "Buffer" :column "Eval" :exit t)
                    ("R" eval-region "Region" :column "Eval" :exit t)
                    ("f" eval-defun "Function" :column "Eval" :exit t)
                    ("e" eval-expression "Expression" :column "Eval" :exit t)
                    ("C" helm-M-x "Command" :column "Eval" :exit t)
                    ("c" cua-copy-region "Copy" :column "Edit" :exit t)
                    ("x" cua-cut-region "Cut" :column "Edit" :exit t)
                    ("v" cua-paste "Paste" :column "Edit" :exit t)
                    ("s" save-buffer "Save" :column "Edit" :exit t)
                    ("o" helm-find-files "Open" :column "Edit" :exit t)))
(defdynhydra 'M-f '(("s" save-buffer "Save" :column "File" :exit t)
                    ("o" helm-find-files "Open" :column "File" :exit t)
                    ("n" find-file "New" :column "File" :exit t)
                    ("b" ranger "Filebrowser" :column "File" :exit t)))
(defdynhydra 'M-g '(("l" goto-line "Line" :column "GoTo" :exit t)
                    ("c" goto-char "Character" :column "GoTo" :exit t)))
(defdynhydra 'M-h '())
(defdynhydra 'M-i '())
(defdynhydra 'M-j '())
(defdynhydra 'M-k '())
(defdynhydra 'M-l '())
(defdynhydra 'M-m '())
(defdynhydra 'M-n '())
(defdynhydra 'M-o '())
(defdynhydra 'M-p '())
(defdynhydra 'M-q '())
(defdynhydra 'M-r '())
(defdynhydra 'M-s '(("h" split-window-horizontally "Horizontal" :column "Split" :exit t)
                    ("v" split-window-vertically "Verital" :column "Split" :exit t)
                    ("c" delete-window "Close Current" :column "Split" :exit t)
                    ("o" (lambda ()
                           (interactive)

                           (delete-other-windows)

                           (when gears-show-file-sidebar
                             (if (projectile-project-p)
                                 (treemacs-projectile)
                               (treemacs)))) "Close Other" :column "Split" :exit t)))
(defdynhydra 'M-t '(("c" comment-dwim-2 "Comment" :column "Toggle" :exit nil)
                    ("r" origami-forward-toggle-node "Region" :column "Toggle" :exit t)))
(defdynhydra 'M-u '())
(defdynhydra 'M-v '())
(defdynhydra 'M-w '())
(defdynhydra 'M-x '())
(defdynhydra 'M-y '())
(defdynhydra 'M-z '())

(load (concat gears-emacs-basepath "/layers/base/helm"))
(load (concat gears-emacs-basepath "/layers/base/keyboard"))
