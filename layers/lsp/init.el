(defgroup gears-layers/lsp nil
  "LSP Layer Configuration"
  :group 'gears-layers)

(defcustom gears-layers/lsp-enable-breadcrumbs nil
  "Show the breadcrumbs in the header line."
  :type 'boolean
  :group 'gears-layers/lsp)

(setq lsp-headerline-breadcrumb-enable gears-layers/lsp-enable-breadcrumbs)

(use-package lsp-mode
  :ensure t
  :init (progn (when (gears-layer-installed-p 'flycheck)
				 (setq lsp-prefer-flymake nil))
			   (setq gc-cons-threshold 100000000)
			   (setq read-process-output-max (* 1024 1024))
			   (setq lsp-completion-provider :capf))
  :hook ((prog-mode . lsp)
		 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;(use-package dap-mode
;  :ensure t
;  :config (setq dap-auto-configure-features '(sessions locals controls tooltip expressions))
;  :hook ((dap-stopped-hook
;          (lambda (arg) (call-interactively #'dap-hydra)))))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config (progn (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
				 (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))

(when (gears-layer-installed-p 'auto_completion)
  (use-package company-lsp
    :ensure t
    :commands company-lsp
    :config (setq company-lsp-cache-candidates 'auto
				  company-lsp-enable-snippet (package-installed-p yasnippet))))

(use-package helm-lsp
  :ensure t)

(use-package lsp-origami
  :ensure t
  :hook (lsp-mode . lsp-origami-mode))

(use-package lsp-treemacs
  :ensure t
  :after treemacs lsp-mode
  :hook (lsp-mode . (lambda ()
					  (lsp-treemacs-sync-mode 1))))

(use-package yasnippet
  :ensure t)
