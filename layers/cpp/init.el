(defun gears-layers/cpp-setup ()
  (lsp))

(use-package c++-mode
  :hook (c++-mode . gears-layers/cpp-setup))

(use-package c-mode
  :hook (c-mode . gears-layers/cpp-setup))
