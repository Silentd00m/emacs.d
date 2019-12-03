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
                                 ("C-u" . undo-tree-visualize))

  ""
  :type '(alist :value (group key-sequence symbol))
  :set #'(lambda (sym val)
           (custom-set-default sym val)
           (gears-layers/base/config-global-keymap val))
  :group 'gears)

;; Hydras
                                 (dolist (hydra '(M-a M-c M-e M-f M-g M-s M-t))
                                    (global-unset-key (kbd (prin1-to-string hydra)))

                                    (global-set-key (kbd (prin1-to-string hydra))
                                                    `(lambda () (interactive) (dynhydra-open ,hydra))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set configuration

;;(define-key helm-map (kbd "<escape>") 'helm-keyboard-quit)

(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  "Don't unsplit window when pressing escape."

  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(gears-layers/base/config-global-keymap gears-global-keymap)

;; Company mode
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") #'company-complete-selection))
