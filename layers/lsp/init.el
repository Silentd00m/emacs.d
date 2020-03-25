(use-package lsp-mode
  :ensure t
  :init (when (gears-layer-installed-p 'flycheck)
          (setq lsp-prefer-flymake t))
  :hook ((prog-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(when (gears-layer-installed-p 'auto_completion)
  (use-package company-lsp
    :ensure t
    :commands company-lsp
    :config (setq company-lsp-cache-candidates 'auto)))

(use-package helm-lsp
  :ensure t)

(use-package lsp-origami
  :ensure t
  :hook ((lsp-mode . lsp-origami-mode)))
