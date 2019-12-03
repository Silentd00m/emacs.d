(load (concat gears-emacs-basepath "/layers/base/config"))
(load (concat gears-emacs-basepath "/layers/base/helm"))
(load (concat gears-emacs-basepath "/layers/base/keyboard"))
(load (concat gears-emacs-basepath "/functions/hydra"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base

;; Make sure deadkeys work
(require 'iso-transl)

(fset 'yes-or-no-p 'y-or-n-p) ;; shorten yes/no to y/n
(setq scroll-conservatively 10000)
(setq tramp-default-method "ssh")

(setq create-lockfiles nil)  ;; No lock files
(setq make-backup-files nil) ;; No backup files
(setq frame-resize-pixelwise t) ;; Resize by pixels instead of rows

(use-package undo-tree
  :ensure t
  :bind (("C-u" . undo-tree)))

(use-package comment-dwim-2
  :ensure t
  :bind (("M-," . comment-dwim-2)))

(use-package flycheck
  :ensure t
  :hook (prog-mode-hook . flycheck-mode)
  :diminish flycheck-mode)

(use-package flycheck-pos-tip
  :ensure t
  :hook (flycheck-mode . (flycheck-pos-tip-mode t)))

(use-package helm
  :ensure t
  :bind (("C-p" . gears-helm-mini)
         ("C-o" . helm-find-files)
         ("M-x" . helm-M-x)
         ("<f7>" . helm-semantic-or-imenu)

         :map helm-map
         ("<left>" . helm-previous-source)
         ("<right>" . helm-next-source)
         ("<tab>" . helm-execute-persistent-action))
  :init (progn (require 'helm-config)

               (helm-mode 1)
               (setq helm-quick-update t)
               (setq helm-X-x-fuzzy-match t)
               (setq helm-buffers-fuzzy-matching t)
               (setq helm-recentf-fuzzy-matching t)
               (setq helm-move-to-line-cycle-in-source t)
               (setq helm-semantic-fuzzy-match t)
               (setq helm-apropos-fuzzy-match t)
               (setq helm-imenu-fuzzy-match t)
               (setq helm-candidate-number-limit 30)
               (helm-autoresize-mode 1)
               (advice-add 'helm-ff-delete-char-backward :around #'gears/helm-find-files-navigate-back))
  :config (progn (when (>= emacs-major-version 27)
                   (with-eval-after-load 'helm-mode
                     (dolist (face '(helm-source-header helm-selection))
                       (set-face-attribute face nil :extend t))))))

(use-package helm-flx
  :ensure t
  :config (progn (setq helm-flx-for-helm-find-files t
                       helm-flx-for-helm-locate t)
                 (helm-flx-mode +1)))

(use-package helm-projectile
  :ensure t
  :config (projectile-global-mode))

(use-package helm-flycheck
  :ensure t)

(use-package visual-regexp
  :bind ("C-h" . vr/replace)
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package hydra
  :ensure t)

(use-package projectile
  :ensure t)

(use-package smartparens
  :ensure t
  :config (progn (require 'smartparens-config)
                 (smartparens-global-mode t)))

(use-package origami
  :ensure t)

(when (version<= emacs-version "26.0")
  (use-package nlinum
    :ensure t)
  (use-package nlinum-hl
    :ensure t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look and Feel

;; General
(setq shift-select-mode t                                                       ;; Allow shift+arrow selection
      cua-keep-region-after-copy t
      x-select-enable-clipboard t
      select-enable-clipboard t)
(cua-mode t)                                                                    ;; Sane C-c, C-x and C-v
(transient-mark-mode 1)
(setq scroll-step 1)

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package powerline
  :ensure t
  :config (setq powerline-default-theme gears-powerline-theme
                powerline-default-separator gears-powerline-shape))

(use-package hl-line
  :init (when (>= emacs-major-version 27)
          (with-eval-after-load 'hl-line
            (set-face-attribute 'hl-line nil :extend t))))

(use-package zoom
  :ensure t
  :config (progn (setq zoom-size '(0.618 . 0.618))
                 (when gears-autoresize-splits
                   (zoom-mode))))

(use-package fill-column-indicator
  :ensure t
  :config (progn
            (setq fci-rule-column 80)
            (add-hook 'prog-mode-hook 'fci-mode)))

(use-package diminish
  :ensure t)

(use-package yascroll
  :ensure t)

(use-package multiple-cursors
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General dependencies

(use-package use-package-hydra
  :ensure t
  :after hydra)

(use-package git
  :ensure t)

(use-package ranger
  :ensure t
  :after use-package-hydra
  :hydra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apply other configs

(electric-indent-mode)

(add-hook 'after-change-major-mode-hook
          #'(lambda()
              (gears-layers/base/config-indent-mode gears-indent-mode)
              (gears-layers/base/config-indent-width gears-indent-width)
              (gears-layers/base/config-insert-eof-newline
               gears-insert-eof-newline)))

(when gears-indent-comments
  (setq aggressive-indent-comments-too t))

(setq column-number-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Fix tramp hanging when the shell output is colored
(setq shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"
      tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

(when gears-maximize-after-start
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(defun gears-layers/base/config-global-keymap (val)
  "Setter callback function for gears-autoresize-splits."

  (dolist (i val)
    (global-set-key (kbd (car i)) (cdr i))))

(defdynhydra 'M-a '(("s" align-current "Selection" :column "Align" :exit t)
                    ("e" align-enture "Everything" :column "Align" :exit t)))
(defdynhydra 'M-b '())
(defdynhydra 'M-c '(("L" downcase-dwim "Lowercase" :column "Convert" :exit t)
                    ("U" upcase-dwim "Uppercase" :column "Convert" :exit t)
                    ("C" capitalize-dwim "Capitalize" :column "Convert" :exit t)))
(defdynhydra 'M-d '())
(defdynhydra 'M-e '(("b" eval-buffer "Buffer" :column "Eval" :exit t)
                    ("R" eval-region "Region" :column "Eval" :exit t)
                    ("f" eval-defun "Function" :column "Eval" :exit t)
                    ("e" eval-expression "Expression" :column "Eval" :exit t)
                    ("C" helm-M-x "Command" :column "Eval" :exit t)
                    ("c" cua-copy-region "Copy" :column "Edit" :exit t)
                    ("x" cua-cut-region "Cut" :column "Edit" :exit t)
                    ("v" cua-paste "Paste" :column "Edit" :exit t)
                    ("s" save-buffer "Save" :column "Edit" :exit t)
                    ("o" helm-find-files "Open" :column "Edit" :exit t)))
(defdynhydra 'M-f '(("s" save-buffer "Save" :column "File" :exit t)
                    ("o" helm-find-files "Open" :column "File" :exit t)
                    ("n" find-file "New" :column "File" :exit t)
                    ("b" ranger "Filebrowser" :column "File" :exit t)))
(defdynhydra 'M-g '(("l" goto-line "Line" :column "GoTo" :exit t)
                    ("c" goto-char "Character" :column "GoTo" :exit t)))
(defdynhydra 'M-h '())
(defdynhydra 'M-i '())
(defdynhydra 'M-j '())
(defdynhydra 'M-k '())
(defdynhydra 'M-l '())
(defdynhydra 'M-m '())
(defdynhydra 'M-n '())
(defdynhydra 'M-o '())
(defdynhydra 'M-p '())
(defdynhydra 'M-q '())
(defdynhydra 'M-r '())
(defdynhydra 'M-s '(("h" split-window-horizontally "Horizontal" :column "Split" :exit t)
                    ("v" split-window-vertically "Verital" :column "Split" :exit t)
                    ("c" delete-window "Close Current" :column "Split" :exit t)
                    ("o" (lambda ()
                           (interactive)

                           (delete-other-windows)

                           (when gears-show-file-sidebar
                             (if (projectile-project-p)
                                 (treemacs-projectile)
                               (treemacs)))) "Close Other" :column "Split" :exit t)))
(defdynhydra 'M-t '(("c" comment-dwim-2 "Comment" :column "Toggle" :exit nil)
                    ("r" origami-forward-toggle-node "Region" :column "Toggle" :exit t)))
(defdynhydra 'M-u '())
(defdynhydra 'M-v '())
(defdynhydra 'M-w '())
(defdynhydra 'M-x '())
(defdynhydra 'M-y '())
(defdynhydra 'M-z '())
