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
  ;; Add goto-definition for c-modes.
  (gears-layers/rtags-hydra-add)

  (when (gears-layer-installed 'auto_completion)
    (setq rtags-completions-enabled t)
    (add-to-list 'company-backends 'company-rtags)))

(defun gears-layers/rtags-description ()
  "Client/Server indexer for C++.")

(defun gears-layers/rtags-install ()
  )

(gears-layer-defdepends rtags
                        :packages '(rtags)
                        :layers '(cpp))
