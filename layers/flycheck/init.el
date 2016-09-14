(defun gears-layers/flycheck-init()
  (require 'flycheck-pos-tip)

  (add-hook 'prog-mode-hook #'(lambda()
                                (setq-local flycheck-display-errors-function (function flycheck-pos-tip-error-message))
                                (flycheck-pos-tip-mode t)))
  (global-flycheck-mode t)

  (unless gears-show-minor-modes
    (diminish 'flycheck-mode)))

(defun gears-layers/flycheck-description()
  "Returns the description of the flycheck Layer."

  "On the fly syntax checking.")

(defun gears-layers/flycheck-install()
  )

(defun gears-layers/flycheck-remove()
  )

(gears-layer-defdepends flycheck
                        :packages '(flycheck flycheck-pos-tip))
