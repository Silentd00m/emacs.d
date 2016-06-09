;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main theme and font
(when (string= gears-theme "moe-theme")
  (use-package moe-theme
    :ensure t
    :config (load-theme 'moe-dark t)))

(when (not (string= gears-theme "moe-theme"))
  (require gears-theme))

;(use-package moe-dark)
;(load-theme 'moe-dark t)

(set-default-font "Source Code Pro-9")
(set-frame-font "Source Code Pro-9")

(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))
(global-hl-line-mode 1) ; Turn on highlighting current line
(show-paren-mode 1)                                                             ; Show matching delimiters.
(setq show-paren-style 'expression)
(global-hl-line-mode t) ; Highlight current line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI Elemnts
(use-package nlinum
  :ensure t
  :init (global-nlinum-mode t))
(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(column-number-mode t) ; Show column numers
(tool-bar-mode -1) ; Disable toolbar
(setq tool-bar-mode nil) ; Disable toolbar

;; Maximize window on start
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Powerline
(use-package powerline
  :ensure t
  :config (progn (setq powerline-default-separator 'slant)
                 (powerline-moe-theme)))

;; Whitespace
(use-package whitespace
  :ensure t
  :config (progn (setq whitespace-line-column 80) ; limit line length
                 (setq whitespace-style '(face lines-tail)) ; only highlight long lines
                 (add-hook 'prog-mode-hook 'whitespace-mode)))

;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Tabbar-ruler
(if gears-tabbar-enabled
    (use-package tabbar-ruler
      :ensure tabbar-ruler
      :config (progn (tabbar-mode t)
                     (setq tabbar-ruler-global-tabbar t) ; Use tabbar
                     (setq tabbar-ruler-movement-timer-dealy 1000000)))) ; Reduce lag
