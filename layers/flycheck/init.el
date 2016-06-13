(defun gears-layers/flycheck-description()
  "Returns the description of the flycheck Layer."

  (eval ""))

(defun gears-layers/flycheck-install()
  (dolist (package '(flycheck flycheck-pos-tip))
    (package-install package)))

(defun gears-layers/flycheck-init()
  (setq flycheck-display-errors-function (function flycheck-pos-tip-error-message)))
