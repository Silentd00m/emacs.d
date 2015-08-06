(require 'flycheck)

(setq flycheck-highlighting-mode `symbols)

(add-hook 'after-init-hook 'global-flycheck-mode)

(require 'flycheck-ycmd)
(flycheck-ycmd-setup)

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
