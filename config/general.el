;(require 'tramp)

(setq inhibit-startup-message t) ; Don't show welcome screen:
(setq make-backup-files nil)  ; Don't create backup files
(setq auto-save-default nil) ; Don't create autosave files

;; Indentation
(require 'aggressive-indent)

(global-aggressive-indent-mode 1)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(setq create-lockfiles nil)  ; No lock files
(setq make-backup-files nil) ; No backup files
(setq bell-volume 0) ; No alarm beep

(setq scroll-step 1)

;(setq redisplay-dont-pause t)
(setq scroll-conservatively 10000)

(fset 'yes-or-no-p 'y-or-n-p) ; shorten yes/no to y/n

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq ediff-split-window-function 'split-window-horizontally) ; split ediff horzontally

;(xterm-register-default-colors)
;(tty⁻set-up-initial-frame-faces)

;; Tramp comfig
;(setq tramp-default-method "ssh")

;; Comment DWIM
;(setq comment-dwim-2--inline-comment-behavior 'reindent-comment)
