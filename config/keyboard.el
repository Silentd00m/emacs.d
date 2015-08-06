(require 'multiple-cursors)
(require 'redo+)
(require 'god-mode)
(require 'comment-dwim-2)

;; Ergoemacs
                                        ;(setq ergoemacs-ignore-prev-global nil)
                                        ;(setq ergoemacs-theme nil)
                                        ;(setq ergoemacs-keyboard-layout "de")
                                        ;(setq ergoemacs-smart-paste nil)                                                ;; Don't cycle through pastes by pressing Ctrl-V
                                        ;(setq ergoemacs-ctl-c-or-ctl-x-delay 0.1)

                                        ;(require 'ergoemacs-mode)

;; General
(setq shift-select-mode t)                                                      ;; Allow shift+arrow selection
(cua-mode t)                                                                    ;; Sane C-c, C-x and C-v

(global-unset-key (kbd "<insert>"))
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'save-buffer)
(global-unset-key (kbd "C-o"))
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "M-a") 'helm-M-x)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key [M-left] 'tabbar-backward-tab)
(global-set-key [M-right] 'tabbar-forward-tab)
                                        ;(local-set-key (kbd "DEL") 'backward-delete-whitespace-to-column)
(global-set-key (kbd "<C-tab>") 'tab-to-tab-stop)
(global-set-key (kbd "C-S-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-e") 'dired)
(global-set-key (kbd "M-S-<left>") 'windmove-left)
(global-set-key (kbd "M-S-<right>") 'windmove-right)
(global-set-key (kbd "M-S-<up>") 'windmove-up)
(global-set-key (kbd "M-S-<down>") 'windmove-down)

(add-hook 'after-change-major-mode-hook '(lambda()
					   (local-set-key (kbd "<C-tab>") 'tab-to-tab-stop)
					   (local-set-key (kbd "C-S-d") 'mc/mark-next-like-this)
					   (global-unset-key (kbd "C-d"))
					   (global-set-key (kbd "C-S-d") 'mc/mark-next-like-this)
					   (global-set-key (kbd "C-S-a") 'mc/mark-all-like-this)
					   )
	  )

(global-set-key (kbd "C-p") 'helm-mini)
(global-set-key (kbd "C-h") 'vr/replace)
(global-set-key (kbd "C-f") 'helm-occur)
(global-set-key (kbd "<f8>") 'helm-flycheck)
(global-set-key (kbd "<f7>") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-t") 'hs-toggle-hiding)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-y") 'redo)
(global-unset-key (kbd "C-j"))
(global-set-key (kbd "M-g") 'avy-goto-word-or-subword-1)

(global-unset-key (kbd "M-c"))
(global-unset-key (kbd "M-v"))
(global-unset-key (kbd "M-x"))
(global-set-key (kbd "M-c") 'cua-copy-region)
(global-set-key (kbd "M-v") 'cua-paste)
(global-set-key (kbd "M-x") 'cua-cut-region)
(global-set-key (kbd "M-r") 'ranger-enable)
(global-set-key (kbd "M-,") 'comment-dwim-2)
(global-set-key (kbd "M-s") 'helm-swoop)

;; God mode
(global-set-key (kbd "<f1>") 'god-local-mode)
(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd ".") 'repeat)
