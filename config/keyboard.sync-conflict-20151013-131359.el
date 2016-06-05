(require 'multiple-cursors)
(require 'redo+)
(require 'comment-dwim-2)

;; General
(setq shift-select-mode t)                                                      ;; Allow shift+arrow selection
(cua-mode t)                                                                    ;; Sane C-c, C-x and C-v

(global-unset-key (kbd "<insert>"))
(global-unset-key (kbd "C-s"))
(global-set-key (kbd "C-s") 'save-buffer)
(global-unset-key (kbd "C-o"))
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-unset-key (kbd "C-e"))
(global-set-key (kbd "C-e") 'helm-M-x)
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
(global-set-key (kbd "<f8>") 'helm-flycheck)
(global-set-key (kbd "<f7>") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-t") 'hs-toggle-hiding)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-y") 'redo)
(global-set-key (kbd "M-w") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "M-l") 'avy-goto-line)
(global-set-key (kbd "M-:") 'goto-line)
(global-unset-key (kbd "M-c"))
(global-unset-key (kbd "M-v"))
(global-unset-key (kbd "M-x"))
(global-set-key (kbd "M-c") 'cua-copy-region)
(global-set-key (kbd "M-v") 'cua-paste)
(global-set-key (kbd "M-x") 'cua-cut-region)
(global-set-key (kbd "M-r") 'ranger-enable)
(global-set-key (kbd "M-,") 'comment-dwim-2)
(global-set-key (kbd "M-s") 'helm-swoop)
(global-unset-key (kbd "M-a"))
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

;; Aquamacs bindings
(when (boundp 'aquamacs-version)
  (define-key osx-key-mode-map (kbd "M-ß") (lambda() (interactive) (aquamacs-insert-or-replace "\\"))) ; Fix for FUBAR'd Alt-ß for \ on german keyboards
  (define-key osx-key-mode-map (kbd "M-7") (lambda() (interactive) (aquamacs-insert-or-surround "{"))) ; Fix for FUBAR'd Alt-7 for { on german keyboards
  (define-key osx-key-mode-map (kbd "M-8") (lambda() (interactive) (aquamacs-insert-or-surround "["))) ; Fix for FUBAR'd Alt-7 for [ on german keyboards
  (define-key osx-key-mode-map (kbd "M-q") (lambda() (interactive) (aquamacs-insert-or-replace "@"))) ; Fix FUBAR'd Alt-q for | on german keyboards
  (define-key osx-key-mode-map (kbd "M-<") (lambda() (interactive) (aquamacs-insert-or-replace "|"))) ; Fix FUBAR'd Alt-< for @ on german keyboards
  (define-key osx-key-mode-map (kbd "M-e") (lambda() (interactive) (aquamacs-insert-or-replace "€"))) ; Fix FUBAR'd Alt-< for € on german keyboards
  (define-key osx-key-mode-map (kbd "M-+") (lambda() (interactive) (aquamacs-insert-or-replace "~"))) ; Fix for FUBAR'd Alt-+ for { on german keyboards
  (define-key osx-key-mode-map (kbd "M-9") (lambda() (interactive) (aquamacs-insert-or-replace "]"))) ; Fix for FUBAR'd Alt-9 for ] on german keyboards
  (define-key osx-key-mode-map (kbd "M-0") (lambda() (interactive) (aquamacs-insert-or-replace "}"))) ; Fix for FUBAR'd Alt-9 for } on german keyboards
  (define-key osx-key-mode-map (kbd "M-a") 'helm-M-x)
  (define-key osx-key-mode-map (kbd "A-o") 'find-file)
  (define-key osx-key-mode-map (kbd "A-p") 'helm-mini)
  (define-key osx-key-mode-map (kbd "A-f") 'helm-occur)
  (define-key osx-key-mode-map (kbd "A-<left>") 'backward-word)
  (define-key osx-key-mode-map (kbd "A-<right>") 'forward-word)
  (define-key osx-key-mode-map (kbd "<end>") 'end-of-line)
  (define-key osx-key-mode-map (kbd "<home>") 'beginning-of-line))
