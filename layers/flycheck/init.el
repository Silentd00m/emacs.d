(defun gears-layers/flycheck-init()
  (require 'flycheck-pos-tip)

  (add-hook 'prog-mode-hook #'(lambda()
                                (setq-local flycheck-display-errors-function (function flycheck-pos-tip-error-message))
                                (flycheck-pos-tip-mode t)))
  (global-flycheck-mode t))

(defun gears-layers/flycheck-description()
  "Returns the description of the flycheck Layer."

  (eval ""))

(defun gears-layers/flycheck-install()
  )

(defun gears-layers/flycheck-remove()
  )

(setq gears-layers/flycheck-depends '((packages . (flycheck flycheck-pos-tip))
                                      (layers . nil)))
