(defun gears-layers/cpp-setup ()
  (clang-format+-mode t)
  (lsp))

(use-package clang-format+
  :ensure t)

(use-package c++-mode
  :ensure nil
  :hook ((c++-mode . gears-layers/cpp-setup)))

(use-package c-mode
  :ensure nil
  :hook ((c-mode . gears-layers/cpp-setup)))
