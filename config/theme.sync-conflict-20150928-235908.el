(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config variables
(defvar gears-tabbar-enabled nil "Enable tabbar.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main theme and font
(require 'moe-theme)

(use-package moe-dark)
(load-theme 'moe-dark t)

(set-default-font "Source Code Pro-10")
(set-frame-font "Source Code Pro-10")

(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))
(global-hl-line-mode 1) ; Turn on highlighting current line
(show-paren-mode 1)                                                             ; Show matching delimiters.
(setq show-paren-style 'expression)
(global-hl-line-mode t) ; Highlight current line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI Elemnts
(use-package nlinum
  :config (global-nlinum-mode t))
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(column-number-mode t) ; Show column numers
(tool-bar-mode -1) ; Disable toolbar
(setq tool-bar-mode nil) ; Disable toolbar

;; Maximize window on start
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Smart Mode Line
(use-package smart-mode-line
  :init (progn
          (sml/setup)
          (sml/apply-theme 'dark))
  :config (setq sml/no-confirm-load-theme t))

;; Whitespace
(use-package whitespace
  :config (progn (setq whitespace-line-column 80) ; limit line length
                 (setq whitespace-style '(face lines-tail)) ; only highlight long lines
                 (add-hook 'prog-mode-hook 'whitespace-mode)))

;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Tabbar-ruler
(if gears-tabbar-enabled
    (use-package tabbar-ruler
      :config (progn (tabbar-mode t)
                     (setq tabbar-ruler-global-tabbar t) ; Use tabbar
                     (setq tabbar-ruler-movement-timer-dealy 1000000))) ; Reduce lag
  (tabbar-mode nil))
