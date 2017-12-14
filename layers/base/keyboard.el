(defcustom gears-use-evil nil
  "Use evil-mode instead of Gears' cua-mode."
  :type 'boolean
  :group 'gears)

;; General
(setq shift-select-mode t)                                                      ;; Allow shift+arrow selection
(cua-mode t)                                                                    ;; Sane C-c, C-x and C-v
(setq cua-keep-region-after-copy t)
(transient-mark-mode 1)
(setq x-select-enable-clipboard t
      select-enable-clipboard t)

(require 'smartparens-config)
(smartparens-global-mode t)

(defcustom gears-global-keymap '(;; General
                                 ("<escape>" . keyboard-escape-quit)
                                 ("<menu>" . helm-M-x)
                                 ("<apps>" . helm-M-x)

                                 ;; Sane text editing
                                 ("C-s" . save-buffer)
                                 ("C-a" . mark-whole-buffer)
                                 ("C-c" . cua-copy-region)
                                 ;; ("C-x" . cua-cut-region)
                                 ;; ("C-v" . cua-paste)
                                 ("C-y" . undo-tree-redo)
                                 ("C-z" . undo-tree-undo)
                                 ("C-f" . helm-occur)
                                 ("C-h" . vr/replace)

                                 ;; Editing
                                 ("<C-tab>" . tab-to-tab-stop)
                                 ("C-S-d" . mc/mark-next-like-this)
                                 ("C-S-a" . mc/mark-all-like-this)
                                 ("M-," . comment-dwim-2)

                                 ;; Document navigation
                                 ("C-t" . hs-toggle-hiding)

                                 ;; Window navigation
                                 ("M-S-<left>" . windmove-left)
                                 ("M-S-<right>" . windmove-right)

                                 ("M-S-<up>" . windmove-up)
                                 ("M-S-<down>" . windmove-down)

                                 ;; Helm key bindings
                                 ("C-p" . gears-helm-mini)
                                 ("M-a" . helm-M-x)
                                 ("M-x" . helm-M-x)
                                 ("C-o" . helm-find-files)
                                 ("M-o" . helm-fzf)
                                 ("<f1>" . helm-descbinds)
                                 ("<f2>" . helm-dash-at-point)
                                 ("<f3>" . helm-dash)
                                 ("<f5>" . helm-make)
                                 ("<f7>" . helm-semantic-or-imenu)
                                 ("<f8>" . helm-flycheck)
                                 ("C-u" . undo-tree-visualize)

                                 ;; Hydras
                                 ("M-a" . (lambda ()
                                            (interactive)
                                            (eval (dynhydra--open gears-layers/base-hydra-m-a))))
                                 ("M-c" . (lambda ()
                                            (interactive)
                                            (eval (dynhydra--open gears-layers/base-hydra-m-c))))
                                 ("M-e" . (lambda ()
                                            (interactive)
                                            (eval (dynhydra--open gears-layers/base-hydra-m-e))))
                                 ("M-f" . (lambda ()
                                            (interactive)
                                            (eval (dynhydra--open gears-layers/base-hydra-m-f))))
                                 ("M-g" . (lambda ()
                                            (interactive)
                                            (eval (dynhydra--open gears-layers/base-hydra-m-g))))
                                 ("M-p" . (lambda ()
                                            (interactive)
                                            (eval (dynhydra--open gears-layers/base-hydra-m-p))))
                                 ("M-r" . (lambda ()
                                            (interactive)
                                            (eval (dynhydra--open gears-layers/base-hydra-m-r))))
                                 ("M-s" . (lambda ()
                                            (interactive)
                                            (eval (dynhydra--open gears-layers/base-hydra-m-s))))
                                 ("M-t" . (lambda ()
                                            (interactive)
                                            (eval (dynhydra--open gears-layers/base-hydra-m-t)))))

  ""
  :type '(alist :value (group key-sequence symbol))
  :set #'(lambda (sym val)
           (custom-set-default sym val)
           (gears-layers/base/config-global-keymap val))
  :group 'gears)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set configuration

(define-key helm-map (kbd "<escape>") 'helm-keyboard-quit)

(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  "Don't unsplit window when pressing escape."

  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

;; (dolist (i gears-global-keymap)
;;   (global-set-key (kbd (car i)) (cdr i)))

(gears-layers/base/config-global-keymap gears-global-keymap)

;; Company mode
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") #'company-complete-selection))

(load (concat gears-emacs-basepath "/layers/base/hydra"))
