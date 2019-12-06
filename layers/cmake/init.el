;;; Code:

(use-package cmake-mode
  :ensure t
  :hook (cmake-mode-hook . fci-mode))

(use-package cmake-font-lock
  :ensure t
  :init (progn (add-hook 'cmake-mode-hook 'cmake-font-lock-activate)))
