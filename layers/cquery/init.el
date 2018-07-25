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

  (add-to-list 'load-path (concat gears-emacs-basepath "/dep/emacs-cquery"))

  (require 'cquery)

  (setq cquery-extra-init-params '(:enableComments 2
                                                   :cacheFormat "msgpack"
                                                   :completion (:detailedLabel t))
        cquery-sem-highlight-method 'overlay
        ;; cquery-executable (concat gears-emacs-basepath "/dep/cquery/build/release/bin/cquery")
        max-specpdl-size 32000)

  (add-hook 'c++-mode-hook #'(lambda ()
                               (setq-local company-transformers nil)
                               (setq-local company-lsp-async t)
                               (setq-local company-lsp-cache-candidates nil)
                               (lsp-cquery-enable)))
  (add-hook 'c++-mode-hook 'flycheck-popup-tip-mode)
  (add-hook 'c++-mode-hook 'yas-minor-mode)

  ;; (setq company-lsp-async t)

  (gears-layers/cquery-hydra-add))

(defun gears-layers/cquery-description ()
  "Provides cquery highlighting, autocompletion and sets compiler options.")

(defun gears-layers/cquery-install ()
  "Additional installation commands for cquery-layer."

  (custom-set-default 'cquery-executable
                      (concat gears-emacs-basepath "/dep/cquery/build/release/bin/cquery")))

(gears-layer-defdepends cquery
                        :packages '(flycheck-popup-tip yasnippet)
                        :layers '(lsp cpp))
