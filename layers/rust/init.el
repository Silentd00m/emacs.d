(use-package rust-mode
  :ensure t
  :config (progn
			(setq rustic-format-on-save t)
			(setq rust-format-on-save t))
  :custom (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  ;; enable / disable the hints as you prefer:
  ;; (lsp-rust-analyzer-server-display-inlay-hints t)
  ;; (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  ;; (lsp-rust-analyzer-display-chaining-hints t)
  ;; (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  ;; (lsp-rust-analyzer-display-closure-return-type-hints t)
  ;; (lsp-rust-analyzer-display-parameter-hints t)
  ;; (lsp-rust-analyzer-display-reborrow-hints nil)
  :config (progn
			(add-hook 'lsp-mode-hook 'lsp-ui-mode)
			(add-hook 'rust-mode-hook
					  (lambda () (setq indent-tabs-mode nil)))))

(use-package rustic
  :ensure t
  :config '(push 'rustic-clippy flycheck-checkers))
