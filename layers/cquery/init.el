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

(defgroup gears-layers/cquery nil
  "cquery Layer configuration."
  :group 'gears-layers)

(defun gears-layers/cquery-init ()
  "Initializes the cquery layer."

  (add-to-list 'load-path (concat gears-emacs-basepath "/dep/cquery/emacs"))

  (require 'cquery)

  (setq cquery-executable (concat gears-emacs-basepath "/dep/cquery/build/release/bin/cquery"))
  (add-hook 'c++-mode-hook 'lsp-cquery-enable))

(defun gears-layers/cquery-description ()
  "Provides cquery highlighting, autocompletion and sets compiler options.")

(defun gears-layers/cquery-install ()
  "Additional installation commands for cquery-layer.")

(gears-layer-defdepends cquery
                        :layers '(lsp cpp))
