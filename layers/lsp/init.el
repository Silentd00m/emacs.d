;;; Code:

(defgroup gears-layers/lsp nil
  "Language Server Protocol Layer configuration."
  :group 'gears-layers)

(defun gears-layers/lsp-init ()
  "Initializes the lsp layer."

  (require 'lsp-mode)
  (require 'company)
  (require 'lsp-ui)

  (when (gears-layer-installed-p 'auto_completion)
    (push 'company-lsp company-backends))

  (setq company-lsp-async t
        company-lsp-cache-candidates t)

  (when (gears-layer-installed-p 'cpp)
    (add-hook 'c++-mode-hook #'lsp-mode))

  (add-hook 'lsp-mode-hook #'(progn
      (flycheck-mode)))

  (when (gears-layer-installed-p 'flycheck)
    (require 'flycheck)
    (with-eval-after-load 'lsp-mode
      (lsp-ui-mode)
      (dap-mode 1))))

(defun gears-layers/lsp-description ()
  "Provides lsp highlighting, autocompletion and sets compiler options.")

(defun gears-layers/lsp-install ()
  "Additional installation commands for lsp-layer.")

(gears-layer-defdepends lsp
                        :packages '(lsp-mode lsp-ui company-lsp dap-mode)
                        :layers '(flycheck))
