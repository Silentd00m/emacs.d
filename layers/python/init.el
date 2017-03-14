;; -*- lexical-binding: t -*-

;;; Code:

(defun gears-layers/python-hydra-add ()
  ;; TODO : Find why this does not work when the layer is precompiled
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

  (defcustom gears-layers/python-show-coverage nil
    "Show coverage test indicators in python-mode."
    :type 'boolean
    :group 'gears-layers/python)

  (defcustom gears-layers/python-show-indent-guides t
    "Show indentation guides in python-mode"
    :type 'boolean
    :group 'gears-layers/python)

  (when (not (gears-layer-installed 'ycmd))
    (gears-layers/python-hydra-add)

    (add-hook 'anaconda-mode-hook #'(lambda ()
                                      (eval-after-load "company"
                                        '(add-to-list 'company-backends '(company-anaconda)))))

    (when gears-layers/python-show-coverage
      (add-hook 'python-mode-hook 'pycoverage-mode))

    (when gears-layers/python-show-indent-guides
      (add-hook 'python-mode-hook 'highlight-indent-guides-mode))

    (add-hook 'python-mode-hook 'anaconda-mode)

    (unless (equal gears-show-documentation-mode 'none)
      (add-hook 'anaconda-mode-hook 'anaconda-eldoc-mode))

    (add-hook 'anaconda-mode-hook #'(lambda ()
                                      (dolist (i gears-global-keymap)
                                        (define-key anaconda-mode-map (kbd (car i)) (cdr i))))))

  (unless gears-show-minor-modes
    (eval-after-load "anaconda" #'(diminish 'anaconda-mode)))

  (unless (dynhydra--get-category 'gears-layers/base-hydra-m-f "Format")
    (dynhydra--add-category 'gears-layers/base-hydra-m-f
                            `(,(make-dynhydra-category :title "Format"))))


  (dynhydra-category--add-head
   (dynhydra--get-category
    'gears-layers/base-hydra-m-f "Format")
   `(,(make-dynhydra-head :key "F"
                          :text "Format Buffer"
                          :command '(lambda ()
                                      (interactive)

                                      (py-yapf-buffer)
                                      (py-autopep8))
                          :exit t
                          :condition (lambda ()
                                       (or (eq major-mode 'python-mode)
                                           (eq major-mode 'anaconda-mode)))))))

(defun gears-layers/python-install()
  "Additional commands for layer installation."

  t)

(defun gears-layers/python-description ()
  "Returns a description of the layer."

  "Provides a python development environment.")

(gears-layer-defdepends python
                        :packages '(elpy
                                    company-anaconda
                                    anaconda-mode
                                    helm-pydoc
                                    pycoverage
                                    py-yapf
                                    python-docstring
                                    pip-requirements
                                    highlight-indent-guides))
