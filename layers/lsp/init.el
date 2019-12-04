(use-package lsp-mode
  :ensure t
  :init (when (gears-layer-installed-p 'flycheck)
          (setq lsp-prefer-flymake t))
  :hook ((prog-mode . lsp-deferred))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(when (gears-layer-installed-p 'auto_completion)
  (use-package company-lsp
    :ensure t
    :commands company-lsp))

(use-package helm-lsp
  :ensure t)

;; (use-package dap-mode
;;   :ensure t
;;   :config (progn (dap-mode 1)
;;                  (dap-ui-mode 1)
;;                  (dap-tooltip-mode 1)))

(use-package lsp-origami
  :ensure t
  :hook ((lsp-mode . lsp-origami-mode)))

;; (use-package lsp-treemacs
;;   :ensure t
;;   :after treemacs
;;   :init (lsp-treemacs-sync-mode 1))
