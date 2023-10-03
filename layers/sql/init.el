(use-package sql-indent
  :ensure t
  :hook (sql-mode-hook . sqlind-minor-mode))

(use-package sqlformat
  :ensure t
  :config (add-hook 'sql-mode-hook 'sqlformat-on-save-mode)
  :diminish sqlformat-on-save-mode)
