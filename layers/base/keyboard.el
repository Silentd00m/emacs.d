(defcustom gears-use-evil nil
  "Use evil-mode instead of Gears' cua-mode."
  :type 'boolean
  :group 'gears)

;; General
(setq shift-select-mode t)                                                      ;; Allow shift+arrow selection
(cua-mode t)                                                                    ;; Sane C-c, C-x and C-v
(setq cua-keep-region-after-copy t)
(transient-mark-mode 1)
(setq x-select-enable-clipboard t)

(use-package comment-dwim-2
  :ensure t)
(use-package redo+
  :ensure t)
(use-package multiple-cursors
  :ensure t)

(defcustom gears-global-keymap '(;; General
                                 ("<escape>" . keyboard-escape-quit)

                                 ;; Sane text editing
                                 ("C-s" . save-buffer)
                                 ("C-a" . mark-whole-buffer)
                                 ("C-c" . cua-copy-region)
                                 ;; ("C-x" . cua-cut-region)
                                 ;; ("C-v" . cua-paste-region)
                                 ("C-y" . cua-redo)
                                 ("C-z" . cua-undo)
                                 ("C-f" . helm-occur)
                                 ("C-h" . vr/replace)

                                 ;; Editing
                                 ("<C-tab>" . tab-to-tab-stop)
                                 ("C-S-d" . mc/mark-next-like-this)
                                 ("C-S-a" . mc/mark-all-like-this)
                                 ("M-," . comment-dwim-2)

                                 ;; Document navigation
                                 ("M-w" . avy-goto-word-or-subword-1)
                                 ("M-l" . goto-line)
                                 ("C-t" . hs-toggle-hiding)

                                 ;; Window navigation
                                 ("M-S-<left>" . windmove-left)
                                 ("M-S-<right>" . windmove-right)

                                 ("M-S-<up>" . windmove-up)
                                 ("M-S-<down>" . windmove-down)

                                 ;; Documentation
                                 ("<f3>" . help-command)

                                 ;; Helm key bindings
                                 ("C-p" . helm-mini)
                                 ("M-a" . helm-M-x)
                                 ("M-x" . helm-M-x)
                                 ("C-o" . helm-find-files)
                                 ("M-o" . helm-fzf)
                                 ("<f1>" . helm-dash)
                                 ("<f2>" . helm-dash-at-point)
                                 ("<f5>" . helm-make)
                                 ("<f7>" . helm-semantic-or-imenu)
                                 ("<f8>" . helm-flycheck)
                                 ("M-s" . helm-swoop))

  ""
  :type '(alist :value (group key-sequence symbol))
  :group 'gears)

(defcustom gears-use-evil nil
  "Use evil-mode instead of Gears' cua-mode."
  :type 'boolean
  :group 'gears-keyboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set configuration

(dolist (i gears-global-keymap)
  (global-set-key (kbd (car i)) (cdr i)))

;; Evil-mode
(when gears-use-evil
  (use-package evil
    :ensure t
    :config (progn (evil-mode t)

                   (dolist (i gears-global-keymap)
                     (dolist (map '(evil-normal-state-map
                                    evil-insert-state-map
                                    evil-motion-state-map))
                       (unless (string= (car i) "<escape>")
                         (define-key (eval map) (kbd (car i)) (cdr i)))))))

  (use-package powerline-evil
    :ensure t))

;; Company mode
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") #'company-complete-selection))