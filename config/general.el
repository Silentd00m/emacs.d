(setq ns-pop-up-frames nil)
(setq frame-resize-pixelwise t) ; Resize pixels instead of rows

(setq inhibit-startup-message t) ; Don't show welcome screen:
(setq make-backup-files nil)  ; Don't create backup files
(setq auto-save-default nil) ; Don't create autosave files

(add-to-list 'exec-path "/usr/local/bin/")

;; Indentation
                                        ;(require 'aggressive-indent)

                                        ;(global-aggressive-indent-mode 1)
(electric-indent-mode)

(add-hook 'after-change-major-mode-hook '(lambda ()
                                           (setq-default indent-tabs-mode nil)
                                           (setq tab-width 4)
                                           (setq fill-column 81)))

;; General
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(setq create-lockfiles nil)  ; No lock files
(setq make-backup-files nil) ; No backup files

(setq scroll-step 1)
(setq indent-tabs-mode nil) ; Do not indent using tabs
(setq aggressive-indent-comments-too t) ; Indent comments aswell

(setq require-final-newline t) ; Add newline to end of file

(setq scroll-conservatively 10000)

(fset 'yes-or-no-p 'y-or-n-p) ; shorten yes/no to y/n

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq ediff-split-window-function 'split-window-horizontally) ; split ediff horzontally

                                        ;(xterm-register-default-colors)
                                        ;(tty⁻set-up-initial-frame-faces)

;; Tramp comfig
(setq tramp-default-method "ssh")

;; Comment DWIM
                                        ;(setq comment-dwim-2--inline-comment-behavior 'reindent-comment)

(when (fboundp 'one-buffer-one-frame-mode)
  (setq one-buffer-one-frame nil)
  (one-buffer-one-frame-mode 0)
  (tabbar-window-merge-windows))
(when (fboundp 'smart-frame-positioning-mode)
  (smart-frame-positioning-mode 0))


;; Disable alarm
(setq bell-volume 0) ; No alarm beep
(setq ring-bell-function 'ignore)
