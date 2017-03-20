;; -*- lexical-binding: t -*-

;;; Code:

(defun gears-layers/rtags-hydra-add ()
  (dynhydra-category--add-head (dynhydra--get-category 'gears-layers/base-hydra-m-g "GOTO")
                               `(,(make-dynhydra-head :key "d"
                                                      :text "Definition"
                                                      :command 'rtags-find-symbol-at-point
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
                                                      :command 'rtags-rename-symbol
                                                      :exit t
                                                      :condition (lambda()
                                                                   (or (eq major-mode 'c++-mode)
                                                                       (eq major-mode 'c-mode)))))))

(defun gears-layers/rtags-init ()
  (defgroup gears-layers/rtags nil
    "Rtags layer."

    :group 'gears-layers)

  (defcustom gears-layers/rtags-provide-completion t
    "Use rtags for completion wherever possible."

    :type 'boolean
    :group 'gears-layers/rtags)

  (defcustom gears-layers/rtags-provide-error-checking t
    "Use rtags for error checking wherever possible."

    :type 'boolean
    :group 'gears-layers/rtags)

  (require 'flycheck-rtags)
  (require 'rtags-helm)

  (setq rtags-use-helm t)

  ;; Add goto-definition for c-modes.
  (gears-layers/rtags-hydra-add)

  (when (and (gears-layer-installed-p 'auto_completion)
             gears-layers/rtags-provide-completion)
    (setq rtags-completions-enabled t)
    (add-to-list 'company-backends 'company-rtags))

  (when gears-layers/rtags-provide-error-checking
    (add-hook 'c-mode-common-hook
              #'(lambda ()
                  (flycheck-select-checker 'rtags)
                  (setq-local flycheck-highlighting-mode nil)
                  (setq-local flycheck-check-syntax-automatically nil)))))

(defun gears-layers/rtags-description ()
  "Client/Server indexer for C++.")

(defun gears-layers/rtags-install ()
  )

(gears-layer-defdepends rtags
                        :packages '(rtags)
                        :layers '(cpp))
