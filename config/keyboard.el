(require 'multiple-cursors)
(require 'redo+)
(require 'comment-dwim-2)

;; Unbind annoying plugin keys
;(define-key flycheck-command-map (kbd "C-c") nil)

;; General
(setq shift-select-mode t)                                                      ;; Allow shift+arrow selection
(cua-mode t)                                                                    ;; Sane C-c, C-x and C-v
(setq cua-keep-region-after-copy t)
(transient-mark-mode 1)
(setq x-select-enable-clipboard t)

(global-unset-key (kbd "<insert>"))
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-c"))
(global-set-key (kbd "C-s") 'save-buffer)
(global-unset-key (kbd "C-o"))
(global-set-key (kbd "C-o") 'helm-find-files)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-unset-key (kbd "C-e"))
(global-set-key (kbd "M-a") 'helm-M-x)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key [M-left] 'tabbar-backward-tab)
(global-set-key [M-right] 'tabbar-forward-tab)
                                        ;(local-set-key (kbd "DEL") 'backward-delete-whitespace-to-column)
(global-set-key (kbd "<C-tab>") 'tab-to-tab-stop)
(global-set-key (kbd "C-S-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-a") 'mc/mark-all-like-this)
					;(global-set-key (kbd "C-e") 'dired)
(global-set-key (kbd "M-S-<left>") 'windmove-left)
(global-set-key (kbd "M-S-<right>") 'windmove-right)
(global-set-key (kbd "M-S-<up>") 'windmove-up)
(global-set-key (kbd "M-S-<down>") 'windmove-down)

(add-hook 'after-change-major-mode-hook '(lambda()
                                           (local-set-key (kbd "<C-tab>") 'tab-to-tab-stop)
                                           (local-set-key (kbd "C-S-d") 'mc/mark-next-like-this)
                                           (global-unset-key (kbd "C-d"))
                                           (global-set-key (kbd "C-S-d") 'mc/mark-next-like-this)
                                           (global-set-key (kbd "C-S-a") 'mc/mark-all-like-this)))

(global-set-key (kbd "C-p") 'helm-mini)
(global-set-key (kbd "C-h") 'vr/replace)
(global-set-key (kbd "C-f") 'helm-occur)
;;(global-set-key (kbd "<f8>") 'helm-flycheck)
(global-set-key (kbd "<f7>") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-t") 'hs-toggle-hiding)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-y") 'redo)
(global-set-key (kbd "M-w") 'avy-goto-word-or-subword-1)
;; (global-set-key (kbd "M-l") 'avy-goto-line)
(global-set-key (kbd "M-:") 'goto-line)
(global-set-key (kbd "M-,") 'comment-dwim-2)
(global-set-key (kbd "M-s") 'helm-swoop)
(global-unset-key (kbd "M-c"))
(global-set-key (kbd "M-a") 'helm-M-x)

(global-set-key (kbd "<f1>") 'helm-dash)
(global-set-key (kbd "<f2>") 'helm-dash-at-point)
(global-set-key (kbd "<f3>") 'help-command)

;; Company mode
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") #'company-complete-selection))

;; Aquamacs functions
(defun aquamacs-insert-or-replace(char)
  (interactive)

  (if mark-active (delete-region (region-beginning) (region-end)))
  (insert char))
(defun aquamacs-insert-or-surround(char)
  (interactive)

  (if mark-active (sp-wrap-with-pair char)
    (progn (if (string-equal "[" char)
               (insert "[]"))
           (if (string-equal "{" char)
               (insert "{}"))
           (unless mark-active (backward-char 1)))))

(defun mac-bind-key(key function)
  (if (boundp 'aquamacs-version)
      (define-key osx-key-mode-map key function)
    (global-set-key key function)))

;; Aquamacs bindings
(if (eq system-type 'darwin)
    (progn (setq mac-option-modifier nil
                 mac-command-modifier 'meta)
           (mac-bind-key (kbd "M-ß") (lambda() (interactive) (aquamacs-insert-or-replace "\\"))) ; Fix for FUBAR'd Alt-ß for \ on german keyboards
           (mac-bind-key (kbd "M-7") (lambda() (interactive) (aquamacs-insert-or-surround "{"))) ; Fix for FUBAR'd Alt-7 for { on german keyboards
           (mac-bind-key (kbd "M-8") (lambda() (interactive) (aquamacs-insert-or-surround "["))) ; Fix for FUBAR'd Alt-7 for [ on german keyboards
           (mac-bind-key (kbd "M-l") (lambda() (interactive) (aquamacs-insert-or-replace "@"))) ; Fix FUBAR'd Alt-q for | on german keyboards
           (mac-bind-key (kbd "M-<") (lambda() (interactive) (aquamacs-insert-or-replace "|"))) ; Fix FUBAR'd Alt-< for @ on german keyboards
           (mac-bind-key (kbd "M-e") (lambda() (interactive) (aquamacs-insert-or-replace "€"))) ; Fix FUBAR'd Alt-< for € on german keyboards
           (mac-bind-key (kbd "M-+") (lambda() (interactive) (aquamacs-insert-or-replace "~"))) ; Fix for FUBAR'd Alt-+ for { on german keyboards
           (mac-bind-key (kbd "M-9") (lambda() (interactive) (aquamacs-insert-or-replace "]"))) ; Fix for FUBAR'd Alt-9 for ] on german keyboards
           (mac-bind-key (kbd "M-0") (lambda() (interactive) (aquamacs-insert-or-replace "}"))) ; Fix for FUBAR'd Alt-9 for } on german keyboards
           ;; (mac-bind-key (kbd "C-s") 'save-buffer)
           ;; (mac-bind-key (kbd "C-c") 'kill-ring-save)
           ;; (mac-bind-key (kbd "C-v") 'cua-paste)
           ;; (mac-bind-key (kbd "C-x") 'cua-cut-region)
           ;; (mac-bind-key (kbd "C-z") 'undo)
           ;; (mac-bind-key (kbd "C-y") 'redo)
           (mac-bind-key (kbd "<home>") 'beginning-of-line)
           (mac-bind-key (kbd "<end>") 'end-of-line)
           (mac-bind-key (kbd "C-<right>") 'right-word)
           (mac-bind-key (kbd "C-<left>") 'left-word)
           ;; (mac-bind-key (kbd "A-a") 'helm-M-x)
           ;; (mac-bind-key (kbd "A-:") 'goto-line)
           (mac-bind-key (kbd "M-,") 'comment-dwim-2)
           ;; (mac-bind-key (kbd "A-s") 'helm-swoop)
           ;; (mac-bind-key (kbd "A-f") 'helm-occur)
	   ))
