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
                                                                       (eq major-mode 'c-mode)))))))

(defun gears-layers/rtags-init ()
  ;; Add goto-definition for c-modes.
  (gears-layers/rtags-hydra-add))

(defun gears-layers/rtags-description ()
  )

(defun gears-layers/rtags-install ()
  )

(gears-layer-defdepends rtags
                        :packages '(rtags))
