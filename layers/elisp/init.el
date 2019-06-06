(defun gears-layers/elisp-init()
  ""

  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(defun gears-layers/elisp-description()
  "Returns the elisp layer description."

  (eval "Sane emacs-lisp configuration."))

(defun gears-layers/elisp-install()
  "Additional commands for layer installation.")

(defun gears-layers/elisp-remove()
  "Removes the Dash layer including packages."
  )

(gears-layer-defdepends elisp
                        :packages '(parinfer)
                        :layers '(base))
