;;; Code:

(use-package cmake-mode
  :ensure t)

(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate))
