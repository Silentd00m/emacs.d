(use-package go-mode
  :config (progn (setq gofmt-command "goimports")
				 (lsp-register-custom-settings
				  '(("gopls.local" t t)
					("gopls.completeUnimported" t t)
					("gopls.staticcheck" t t))))
  :init (add-hook 'before-save-hook
				  #'(lambda ()
					  (when (eq major-mode 'go-mode)
						(gofmt-before-save))))
  :hook ((go-mode . lsp-deferred)
		 (go-mode . yas-minor-mode)))

(use-package go-projectile
  :ensure t)

(use-package web-mode
  :ensure t
  :mode (".gohtml$")
  :init (add-hook 'web-mode-hook
				  #'(lambda ()
					  (setq web-mode-enable-auto-pairing nil
							web-mode-markup-indent-offset 4))))
