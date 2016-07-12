;; -*- lexical-binding: t -*-

;;; Code:

(defun gears-layers/python-hydra-add ()
  (dynhydra-category--add-head (dynhydra--get-category 'gears-layers/base-hydra-m-g "GOTO")
                               `(,(make-dynhydra-head :key "d"
                                                      :text "Definition"
                                                      :command 'anaconda-mode-find-definitions
                                                      :exit t
                                                      :condition (lambda ()
                                                                   (eq major-mode 'python-mode))))))

(defun gears-layers/python-init ()
  (defgroup gears-layers/python nil
    "Python Layer Configuration"
    :group 'gears-layers)

  (defcustom gears-layers/python-show-coverage t
    "Show coverage test indicators in python-mode."
    :type 'boolean
    :group 'gears-layers/python)

  (when (not (gears-layer-installed 'ycmd))
    (gears-layers/python-hydra-add)

    (use-package anaconda-mode
      :ensure t
      :config (progn ()
                     (add-hook 'anaconda-mode-hook 'anaconda-eldoc-mode)
                     (add-hook 'anaconda-mode-hook #'(lambda ()
                                                       (eval-after-load "company"
                                                         '(add-to-list 'company-backends '(company-anaconda)))))))

    (when gears-layers/python-show-coverage
      (add-hook 'python-mode-hook 'pycoverage-mode))

    (add-hook 'python-mode-hook 'anaconda-mode)))

(defun gears-layers/python-install()
  "Additional commands for layer installation."

  t)

(defun gears-layers/python-description ()
  "Returns a description of the layer."

  "Provides a python development environment.")

(gears-layer-defdepends python
                        :packages '(elpy
                                    company-anaconda
                                    helm-pydoc
                                    pycoverage
                                    py-yapf
                                    python-docstring
                                    pip-requirements))
