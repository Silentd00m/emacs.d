(use-package typescript
  :ensure t
  :hook (typescript-mode . lsp))

(use-package javascript
  :ensure t
  :hook (javascript-mode . lsp))
