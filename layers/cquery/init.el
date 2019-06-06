;;; Code:

(defun gears-layers/cquery-hydra-add ()
  (dynhydra-category--add-head (dynhydra--get-category 'gears-layers/base-hydra-m-g "GOTO")
                               `(,(make-dynhydra-head :key "d"
                                                      :text "Definition"
                                                      :command 'xref-find-definitions
                                                      :exit t
                                                      :condition (lambda ()
                                                                   (or (eq major-mode 'c++-mode)
                                                                       (eq major-mode 'c-mode))))))
  (unless (dynhydra--get-category 'gears-layers/base-hydra-m-r "Refactor")
    (dynhydra--add-category 'gears-layers/base-hydra-m-r
                            `(,(make-dynhydra-category :title "Refactor"))))

  (dynhydra-category--add-head (dynhydra--get-category 'gears-layers/base-hydra-m-r
                                                       "Refactor")
                               `(,(make-dynhydra-head :key "S"
                                                      :text "Rename Symbol"
                                                      :command 'lsp-rename
                                                      :exit t
                                                      :condition (lambda()
                                                                   (or (eq major-mode 'c++-mode)
                                                                       (eq major-mode 'c-mode)))))))

(defun gears-layers/cquery-init ()
  "Initializes the cquery layer."

  (defgroup gears-layers/cquery nil
    "cquery Layer configuration."
    :group 'gears-layers)

  (defcustom gears-layers/cquery-executable-path
    (concat gears-emacs-basepath "/dep/cquery/build/cquery")
    "Specify the path to the cquery executable."

    :type 'string
    :group 'gears-layers/cquery)

  (require 'ccls)

  (add-hook 'c++-mode-hook #'(lambda ()
                               (setq-local company-transformers nil)
                               (setq-local company-lsp-async t)
                               (setq-local company-lsp-cache-candidates nil)
                               (lsp)))
  (add-hook 'c++-mode-hook 'flycheck-popup-tip-mode)
  (add-hook 'c++-mode-hook 'yas-minor-mode)

  ;; (setq company-lsp-async t)

  (gears-layers/cquery-hydra-add))

(defun gears-layers/cquery-description ()
  "Provides cquery highlighting, autocompletion and sets compiler options.")

(defun gears-layers/cquery-install ()
  "Additional installation commands for cquery-layer.")

(gears-layer-defdepends cquery
                        :packages '(flycheck-popup-tip yasnippet cquery)
                        :layers '(lsp cpp))
